# Phase 2: Ontario Hospital Strategic Plan Discovery & PDF Download
# Purpose: Search validated hospital websites for strategic plan PDFs and download them
# Author: Created for hospital strategic plan database project
# Date: 2025-11-28

# Required Libraries ----
library(yaml)
library(rvest)
library(httr)
library(dplyr)
library(stringr)
library(purrr)
library(lubridate)
library(urltools)

# Configuration ----
CONFIG <- list(
  # Project paths
  base_dir = "E:/Hospital_Strategic_Plans",
  yaml_file = "code/base_hospitals_validated.yaml",
  output_folder = "Outputs",
  pdf_folder = "strategic_plans",
  
  # Search keywords (case-insensitive)
  tier1_keywords = c(
    "strategic plan",
    "strategic direction", 
    "strategic priorities"
  ),
  
  tier2_keywords = c(
    "multi-year plan",
    "strategic framework",
    "future planning"
  ),
  
  # Search parameters
  max_pages_to_check = 5,
  search_depth = 2,
  
  # Ethical scraping
  delay_between_requests = 2,  # seconds
  user_agent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
  request_timeout = 30,
  
  # Testing mode
  test_mode = TRUE,  # Set to FALSE for full run
  test_sample_size = 5
)

# Initialize ----
initialize_project <- function() {
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("Phase 2: Strategic Plan Discovery & PDF Download\n")
  cat(strrep("=", 70), "\n\n")
  
  # Check base directory
  if (!dir.exists(CONFIG$base_dir)) {
    stop("Base directory not found: ", CONFIG$base_dir)
  }
  
  # Create directories if needed
  pdf_path <- file.path(CONFIG$base_dir, CONFIG$pdf_folder)
  output_path <- file.path(CONFIG$base_dir, CONFIG$output_folder)
  
  dir.create(pdf_path, showWarnings = FALSE, recursive = TRUE)
  dir.create(output_path, showWarnings = FALSE, recursive = TRUE)
  
  cat("Project directory:", CONFIG$base_dir, "\n")
  cat("PDF output:", pdf_path, "\n")
  cat("Summary output:", output_path, "\n")
  
  if (CONFIG$test_mode) {
    cat("\n*** TEST MODE: Processing only", CONFIG$test_sample_size, "hospitals ***\n")
  }
  
  cat("\n")
  
  # Store paths globally
  CONFIG$pdf_path <<- pdf_path
  CONFIG$output_path <<- output_path
}

# Helper Functions ----

# Read YAML file
read_yaml_hospitals <- function() {
  yaml_path <- file.path(CONFIG$base_dir, CONFIG$yaml_file)
  
  if (!file.exists(yaml_path)) {
    stop("YAML file not found: ", yaml_path)
  }
  
  data <- read_yaml(yaml_path)
  
  # Convert to data frame for easier processing
  hospitals <- data$hospitals
  
  cat("Loaded", length(hospitals), "hospitals from YAML\n\n")
  
  return(hospitals)
}

# Write updated YAML file
write_yaml_hospitals <- function(hospitals) {
  yaml_path <- file.path(CONFIG$base_dir, CONFIG$yaml_file)
  
  # Create backup
  backup_path <- paste0(yaml_path, ".backup.", format(Sys.time(), "%Y%m%d_%H%M%S"))
  file.copy(yaml_path, backup_path)
  cat("Created backup:", backup_path, "\n")
  
  # Write updated YAML
  data <- list(hospitals = hospitals)
  write_yaml(data, yaml_path)
  
  cat("Updated YAML file:", yaml_path, "\n")
}

# Clean hospital name for folder/file names
clean_hospital_name <- function(name) {
  name %>%
    str_replace_all("[^A-Za-z0-9]", "_") %>%
    str_replace_all("_{2,}", "_") %>%
    str_remove("^_|_$")
}

# Safe GET request with error handling
safe_get <- function(url, timeout = CONFIG$request_timeout) {
  Sys.sleep(CONFIG$delay_between_requests)
  
  tryCatch({
    response <- GET(
      url,
      user_agent(CONFIG$user_agent),
      timeout(timeout)
    )
    
    if (status_code(response) == 200) {
      return(list(success = TRUE, response = response, error = NULL))
    } else {
      return(list(
        success = FALSE,
        response = NULL,
        error = paste("HTTP", status_code(response))
      ))
    }
  }, error = function(e) {
    return(list(success = FALSE, response = NULL, error = e$message))
  })
}

# Get all links from a page
get_page_links <- function(html_content, base_url) {
  tryCatch({
    page <- read_html(html_content)
    
    # Get all links
    links <- page %>%
      html_nodes("a") %>%
      html_attr("href")
    
    # Get link text
    link_text <- page %>%
      html_nodes("a") %>%
      html_text() %>%
      str_trim()
    
    # Handle empty results
    if (length(links) == 0) {
      return(data.frame(
        url = character(0),
        text = character(0),
        stringsAsFactors = FALSE
      ))
    }
    
    # Create data frame
    df <- data.frame(
      url = links,
      text = link_text,
      stringsAsFactors = FALSE
    ) %>%
      filter(!is.na(url), url != "", url != "#") %>%
      mutate(
        # Convert relative URLs to absolute
        url = if_else(
          str_starts(url, "http"),
          url,
          if_else(
            str_starts(url, "/"),
            paste0(base_url, url),
            paste0(base_url, "/", url)
          )
        ),
        text_lower = str_to_lower(text),
        url_lower = str_to_lower(url)
      )
    
    return(df)
    
  }, error = function(e) {
    return(data.frame(
      url = character(0),
      text = character(0),
      stringsAsFactors = FALSE
    ))
  })
}

# Check if link matches strategic plan keywords
matches_strategy_keywords <- function(url, text) {
  combined <- paste(str_to_lower(url), str_to_lower(text))
  
  # Check tier 1 keywords first
  tier1_match <- any(sapply(CONFIG$tier1_keywords, function(kw) {
    str_detect(combined, fixed(kw, ignore_case = TRUE))
  }))
  
  if (tier1_match) return(list(match = TRUE, tier = 1))
  
  # Check tier 2 keywords
  tier2_match <- any(sapply(CONFIG$tier2_keywords, function(kw) {
    str_detect(combined, fixed(kw, ignore_case = TRUE))
  }))
  
  if (tier2_match) return(list(match = TRUE, tier = 2))
  
  return(list(match = FALSE, tier = NA))
}

# Check if URL is a PDF
is_pdf_url <- function(url) {
  str_detect(str_to_lower(url), "\\.pdf($|\\?)")
}

# Search for strategy page URLs
search_for_strategy_page <- function(base_url) {
  cat("  Searching for strategic plan pages...\n")
  
  # Get main page
  response <- safe_get(base_url)
  
  if (!response$success) {
    cat("  ERROR: Could not access base URL -", response$error, "\n")
    return(list(found = FALSE, urls = NULL, error = response$error))
  }
  
  # Get all links from main page
  main_links <- get_page_links(content(response$response), base_url)
  
  if (nrow(main_links) == 0) {
    cat("  WARNING: No links found on main page\n")
    return(list(found = FALSE, urls = NULL, error = "No links found"))
  }
  
  cat("  Found", nrow(main_links), "total links on main page\n")
  
  # Find strategy-related links
  strategy_links <- main_links %>%
    rowwise() %>%
    mutate(keyword_match = matches_strategy_keywords(url, text)) %>%
    ungroup() %>%
    mutate(
      is_match = sapply(keyword_match, function(x) x$match),
      tier = sapply(keyword_match, function(x) x$tier)
    ) %>%
    filter(is_match == TRUE) %>%
    arrange(tier, url) %>%
    select(url, text, tier)
  
  if (nrow(strategy_links) == 0) {
    cat("  No strategic plan pages found\n")
    return(list(found = FALSE, urls = NULL, error = "No strategy keywords found"))
  }
  
  cat("  Found", nrow(strategy_links), "potential strategy pages\n")
  
  # Limit to max pages
  if (nrow(strategy_links) > CONFIG$max_pages_to_check) {
    strategy_links <- strategy_links[1:CONFIG$max_pages_to_check, ]
    cat("  Limiting to first", CONFIG$max_pages_to_check, "pages\n")
  }
  
  return(list(found = TRUE, urls = strategy_links, error = NULL))
}

# Find PDF on a strategy page
find_strategy_pdf <- function(strategy_url, base_url) {
  cat("  Checking:", strategy_url, "\n")
  
  # Get the strategy page
  response <- safe_get(strategy_url)
  
  if (!response$success) {
    cat("    ERROR:", response$error, "\n")
    return(list(found = FALSE, pdf_url = NULL, error = response$error))
  }
  
  # Get all links from strategy page
  page_links <- get_page_links(content(response$response), base_url)
  
  if (nrow(page_links) == 0) {
    cat("    No links found on page\n")
    return(list(found = FALSE, pdf_url = NULL, error = "No links on page"))
  }
  
  # Find PDF links that also match strategy keywords
  pdf_links <- page_links %>%
    filter(is_pdf_url(url)) %>%
    rowwise() %>%
    mutate(keyword_match = matches_strategy_keywords(url, text)) %>%
    ungroup() %>%
    mutate(
      is_match = sapply(keyword_match, function(x) x$match),
      tier = sapply(keyword_match, function(x) x$tier)
    ) %>%
    filter(is_match == TRUE) %>%
    arrange(tier, url)
  
  if (nrow(pdf_links) > 0) {
    cat("    ✓ PDF found:", basename(pdf_links$url[1]), "\n")
    return(list(found = TRUE, pdf_url = pdf_links$url[1], error = NULL))
  }
  
  # If no strategy PDF found, check for any PDFs
  any_pdf <- page_links %>%
    filter(is_pdf_url(url))
  
  if (nrow(any_pdf) > 0) {
    cat("    Found PDF but no strategy keywords in filename\n")
    return(list(found = TRUE, pdf_url = any_pdf$url[1], error = "PDF found but no strategy keywords"))
  }
  
  cat("    No PDF found\n")
  return(list(found = FALSE, pdf_url = NULL, error = "No PDF on page"))
}

# Download PDF
download_pdf <- function(pdf_url, fac, hospital_name) {
  # Create folder name
  safe_name <- clean_hospital_name(hospital_name)
  folder_name <- paste0(fac, "_", safe_name)
  folder_path <- file.path(CONFIG$pdf_path, folder_name)
  
  # Create folder
  dir.create(folder_path, showWarnings = FALSE, recursive = TRUE)
  
  # Create filename
  year_month <- format(Sys.time(), "%Y%m")
  filename <- paste0("Strategy_", year_month, "_", fac, ".pdf")
  filepath <- file.path(folder_path, filename)
  
  cat("  Downloading PDF...\n")
  cat("    Destination:", file.path(folder_name, filename), "\n")
  
  # Download
  response <- safe_get(pdf_url, timeout = 60)
  
  if (!response$success) {
    cat("    ERROR: Download failed -", response$error, "\n")
    return(list(
      success = FALSE,
      folder = folder_name,
      filename = filename,
      error = response$error
    ))
  }
  
  # Write file
  tryCatch({
    writeBin(content(response$response, "raw"), filepath)
    cat("    ✓ Downloaded successfully\n")
    return(list(
      success = TRUE,
      folder = folder_name,
      filename = filename,
      error = NULL
    ))
  }, error = function(e) {
    cat("    ERROR: Could not write file -", e$message, "\n")
    return(list(
      success = FALSE,
      folder = folder_name,
      filename = filename,
      error = e$message
    ))
  })
}

# Process single hospital
process_hospital <- function(hospital) {
  fac <- hospital$FAC
  name <- hospital$name
  base_url <- hospital$base_url
  
  cat("\n", strrep("=", 70), "\n")
  cat("Processing:", name, "(FAC:", fac, ")\n")
  cat("Base URL:", base_url, "\n")
  cat(strrep("=", 70), "\n")
  
  # Initialize result structure
  result <- list(
    search_attempted = TRUE,
    search_date = as.character(Sys.Date()),
    strategy_url_found = FALSE,
    strategy_url = "",
    pdf_found = FALSE,
    pdf_url = "",
    pdf_downloaded = FALSE,
    local_folder = "",
    local_filename = "",
    strategy_notes = ""
  )
  
  # Check if validated and allowed
  if (is.null(hospital$base_url_validated) || hospital$base_url_validated != "yes") {
    cat("  SKIPPED: Base URL not validated\n")
    result$strategy_notes <- "Skipped - base URL not validated"
    return(result)
  }
  
  if (is.null(hospital$robots_allowed) || hospital$robots_allowed != "yes") {
    cat("  SKIPPED: Robots.txt does not allow scraping\n")
    result$strategy_notes <- "Skipped - robots.txt disallows scraping"
    return(result)
  }
  
  # Search for strategy pages
  search_result <- search_for_strategy_page(base_url)
  
  if (!search_result$found) {
    result$strategy_notes <- search_result$error
    return(result)
  }
  
  result$strategy_url_found <- TRUE
  
  # Try each strategy page
  for (i in 1:nrow(search_result$urls)) {
    strategy_url <- search_result$urls$url[i]
    result$strategy_url <- strategy_url
    
    # Look for PDF
    pdf_result <- find_strategy_pdf(strategy_url, base_url)
    
    if (pdf_result$found) {
      result$pdf_found <- TRUE
      result$pdf_url <- pdf_result$pdf_url
      
      # Download PDF
      download_result <- download_pdf(pdf_result$pdf_url, fac, name)
      
      if (download_result$success) {
        result$pdf_downloaded <- TRUE
        result$local_folder <- download_result$folder
        result$local_filename <- download_result$filename
        result$strategy_notes <- paste("Found on page:", strategy_url)
        return(result)
      } else {
        result$strategy_notes <- paste("PDF found but download failed:", download_result$error)
        return(result)
      }
    }
  }
  
  # If we get here, no PDF was found on any strategy page
  result$strategy_notes <- paste("Strategy page found but no PDF. Checked", 
                                 nrow(search_result$urls), "pages")
  
  return(result)
}

# Main Processing Function ----
run_phase2 <- function() {
  initialize_project()
  
  # Read hospitals
  hospitals <- read_yaml_hospitals()
  
  # Test mode - limit sample
  if (CONFIG$test_mode) {
    hospitals <- hospitals[1:min(CONFIG$test_sample_size, length(hospitals))]
    cat("TEST MODE: Processing", length(hospitals), "hospitals\n\n")
  }
  
  # Process each hospital
  cat("Starting processing...\n")
  
  for (i in seq_along(hospitals)) {
    hospital <- hospitals[[i]]
    
    # Process hospital
    result <- process_hospital(hospital)
    
    # Add result to hospital entry
    hospitals[[i]]$strategy_search <- result
    
    # Progress
    cat("\nProgress:", i, "/", length(hospitals), "hospitals processed\n")
  }
  
  # Write updated YAML
  cat("\n", strrep("=", 70), "\n")
  cat("Writing updated YAML file...\n")
  write_yaml_hospitals(hospitals)
  
  # Create summary CSV
  cat("\nCreating summary CSV...\n")
  create_summary_csv(hospitals)
  
  # Final summary
  print_final_summary(hospitals)
  
  return(hospitals)
}

# Create summary CSV
create_summary_csv <- function(hospitals) {
  summary_data <- map_dfr(hospitals, function(h) {
    search <- h$strategy_search
    
    if (is.null(search)) {
      search <- list(
        search_attempted = FALSE,
        search_date = NA,
        strategy_url_found = FALSE,
        strategy_url = "",
        pdf_found = FALSE,
        pdf_url = "",
        pdf_downloaded = FALSE,
        local_folder = "",
        local_filename = "",
        strategy_notes = "Not processed"
      )
    }
    
    data.frame(
      FAC = h$FAC,
      Hospital_Name = h$name,
      Hospital_Type = h$hospital_type,
      Base_URL = h$base_url,
      Strategy_URL_Found = search$strategy_url_found,
      Strategy_URL = search$strategy_url,
      PDF_Found = search$pdf_found,
      PDF_URL = search$pdf_url,
      PDF_Downloaded = search$pdf_downloaded,
      Local_Folder = search$local_folder,
      Local_Filename = search$local_filename,
      Notes = search$strategy_notes,
      Search_Date = search$search_date,
      stringsAsFactors = FALSE
    )
  })
  
  # Save CSV
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  csv_file <- file.path(CONFIG$output_path, paste0("phase2_summary_", timestamp, ".csv"))
  write.csv(summary_data, csv_file, row.names = FALSE)
  
  cat("Summary CSV saved:", csv_file, "\n")
  
  return(summary_data)
}

# Print final summary
print_final_summary <- function(hospitals) {
  cat("\n", strrep("=", 70), "\n")
  cat("FINAL SUMMARY\n")
  cat(strrep("=", 70), "\n")
  
  total <- length(hospitals)
  
  # Count results - handle both logical and character types
  attempted <- 0
  url_found <- 0
  pdf_found <- 0
  downloaded <- 0
  
  for (h in hospitals) {
    search <- h$strategy_search
    if (!is.null(search)) {
      if (!is.null(search$search_attempted) && (search$search_attempted == TRUE || search$search_attempted == "true")) {
        attempted <- attempted + 1
      }
      if (!is.null(search$strategy_url_found) && (search$strategy_url_found == TRUE || search$strategy_url_found == "true")) {
        url_found <- url_found + 1
      }
      if (!is.null(search$pdf_found) && (search$pdf_found == TRUE || search$pdf_found == "true")) {
        pdf_found <- pdf_found + 1
      }
      if (!is.null(search$pdf_downloaded) && (search$pdf_downloaded == TRUE || search$pdf_downloaded == "true")) {
        downloaded <- downloaded + 1
      }
    }
  }
  
  not_found <- attempted - url_found
  
  cat("Total hospitals:", total, "\n")
  cat("Search attempted:", attempted, "\n")
  cat("Strategy URL found:", url_found, "\n")
  cat("PDF found:", pdf_found, "\n")
  cat("PDF downloaded:", downloaded, "\n")
  cat("Not found:", not_found, "\n")
  cat("\n")
  if (attempted > 0) {
    cat("Success rate:", round(downloaded / attempted * 100, 1), "%\n")
  } else {
    cat("Success rate: N/A (no searches attempted)\n")
  }
  cat(strrep("=", 70), "\n")
}

# Run the script ----
if (!interactive()) {
  results <- run_phase2()
} else {
  cat("\nScript loaded. Run with: results <- run_phase2()\n")
  cat("To change test mode: CONFIG$test_mode <- FALSE\n\n")
}