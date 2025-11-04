# Ontario Hospital Strategic Plan Scraper
# Purpose: Download strategic plan PDFs from Ontario Acute Care hospitals
# Author: Created for hospital strategic plan database project
# Date: 2025-11-02

# Required Libraries ----
library(rvest)
library(httr)
library(dplyr)
library(stringr)
library(purrr)
library(robotstxt)
library(urltools)
library(lubridate)

# Configuration ----
CONFIG <- list(
  # Keywords to search for strategic plans
  plan_keywords = c(
    "strategic plan", "strategic direction", "corporate plan",
    "multi-year plan", "strategic priorities", "strategic framework"
  ),
  
  # Keywords for "about" sections
  about_keywords = c("about", "who-we-are", "about-us", "our-organization"),
  
  # User agent for requests
  user_agent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
  
  # Request delays (seconds) - be polite!
  delay_between_requests = 2,
  
  # Base project directory (adjust if needed)
  base_dir = "E:/Hospital_Strategic_Plans",
  
  # Output folders (will be created within base_dir structure)
  output_folder = "Outputs",
  pdf_folder = "strategic_plans/pdfs",
  log_folder = "strategic_plans/logs"
)

# Initialize directories ----
initialize_directories <- function() {
  # Construct full paths
  output_path <- file.path(CONFIG$base_dir, CONFIG$output_folder)
  pdf_path <- file.path(CONFIG$base_dir, CONFIG$pdf_folder)
  log_path <- file.path(CONFIG$base_dir, CONFIG$log_folder)
  
  # Check if base directory exists
  if (!dir.exists(CONFIG$base_dir)) {
    stop("Base directory not found: ", CONFIG$base_dir, 
         "\nPlease ensure the project folder exists or update CONFIG$base_dir")
  }
  
  # Create subdirectories if they don't exist
  # (They should already exist based on your structure, but this ensures they're there)
  dir.create(output_path, showWarnings = FALSE, recursive = TRUE)
  dir.create(pdf_path, showWarnings = FALSE, recursive = TRUE)
  dir.create(log_path, showWarnings = FALSE, recursive = TRUE)
  
  cat("Using project directory structure:\n")
  cat("  - Base:", CONFIG$base_dir, "\n")
  cat("  - Outputs:", output_path, "\n")
  cat("  - PDFs:", pdf_path, "\n")
  cat("  - Logs:", log_path, "\n\n")
  
  # Store full paths for later use
  CONFIG$output_path <<- output_path
  CONFIG$pdf_path <<- pdf_path
  CONFIG$log_path <<- log_path
}

# Helper Functions ----

# Extract base URL from full URL
get_base_url <- function(url) {
  parsed <- url_parse(url)
  base_url <- paste0(parsed$scheme, "://", parsed$domain)
  return(base_url)
}


# Check if scraping is allowed via robots.txt
check_robots_txt <- function(url) {
  result <- tryCatch({
    base_url <- get_base_url(url)
    can_scrape <- paths_allowed(
      paths = url,
      domain = base_url,
      bot = "*"
    )
    return(list(allowed = can_scrape, error = NULL))
  }, error = function(e) {
    return(list(allowed = TRUE, error = paste("Could not check robots.txt:", e$message)))
  })
  return(result)
}

# Safe GET request with error handling
safe_get <- function(url, timeout = 30) {
  Sys.sleep(CONFIG$delay_between_requests)
  
  result <- tryCatch({
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
  
  return(result)
}

# Parse HTML and extract links
get_page_links <- function(html_content, base_url) {
  tryCatch({
    page <- read_html(html_content)
    
    # Get all links
    links <- page %>%
      html_nodes("a") %>%
      html_attr("href")
    
    # Get link text for context
    link_text <- page %>%
      html_nodes("a") %>%
      html_text() %>%
      str_trim()
    
    # Create data frame
    df <- data.frame(
      url = links,
      text = link_text,
      stringsAsFactors = FALSE
    ) %>%
      filter(!is.na(url))
    
    # Return empty data frame with proper structure if no links found
    if (nrow(df) == 0) {
      return(data.frame(
        url = character(0),
        text = character(0),
        text_lower = character(0),
        url_lower = character(0),
        stringsAsFactors = FALSE
      ))
    }
    
    # Process URLs and text only if we have links
    df <- df %>%
      mutate(
        # Convert relative URLs to absolute
        url = if_else(
          str_starts(url, "http"),
          url,
          url_compose(url_parse(paste0(base_url, "/", str_remove(url, "^/"))))
        ),
        text_lower = str_to_lower(text),
        url_lower = str_to_lower(url)
      )
    
    return(df)
  }, error = function(e) {
    # Return properly structured empty data frame on error
    return(data.frame(
      url = character(0),
      text = character(0),
      text_lower = character(0),
      url_lower = character(0),
      stringsAsFactors = FALSE
    ))
  })
}

# Check if a link is likely a strategic plan
is_strategic_plan_link <- function(url, text) {
  combined <- paste(str_to_lower(url), str_to_lower(text))
  
  any(sapply(CONFIG$plan_keywords, function(keyword) {
    str_detect(combined, fixed(keyword))
  }))
}

# Check if link is PDF
is_pdf_link <- function(url) {
  str_detect(str_to_lower(url), "\\.pdf($|\\?)")
}

# Download PDF
download_strategic_plan <- function(url, hospital_name, fac) {
  tryCatch({
    # Create safe filename
    safe_name <- str_replace_all(hospital_name, "[^A-Za-z0-9]", "_")
    filename <- paste0(fac, "_", safe_name, "_strategic_plan.pdf")
    filepath <- file.path(CONFIG$pdf_path, filename)
    
    # Download
    response <- safe_get(url, timeout = 60)
    
    if (response$success) {
      writeBin(content(response$response, "raw"), filepath)
      return(list(success = TRUE, filepath = filename, error = NULL))
    } else {
      return(list(success = FALSE, filepath = NULL, error = response$error))
    }
  }, error = function(e) {
    return(list(success = FALSE, filepath = NULL, error = e$message))
  })
}

# Main Search Function ----

search_hospital_strategic_plan <- function(hospital_name, fac, url, hospital_type) {
  cat("\n", strrep("=", 70), "\n")
  cat("Processing:", hospital_name, "(", fac, ")\n")
  cat("URL:", url, "\n")
  cat(strrep("=", 70), "\n")
  
  # Initialize result
  result <- data.frame(
    Hospital_Name = hospital_name,
    FAC = fac,
    Type = hospital_type,
    Input_URL = url,
    Base_URL = NA,
    Robots_Allowed = NA,
    Robots_Note = NA,
    Plan_Found = FALSE,
    PDF_URL = NA,
    Local_Path = NA,
    Search_Method = NA,
    Status = "Not Started",
    Notes = NA,
    Date_Checked = Sys.time(),
    stringsAsFactors = FALSE
  )
  
  # Step 1: Get base URL
  base_url <- get_base_url(url)
  result$Base_URL <- base_url
  cat("Base URL:", base_url, "\n")
  
  # Step 2: Check robots.txt
  cat("Checking robots.txt... ")
  robots_check <- check_robots_txt(base_url)
  result$Robots_Allowed <- robots_check$allowed
  result$Robots_Note <- robots_check$error
  
  if (!robots_check$allowed) {
    cat("BLOCKED by robots.txt\n")
    result$Status <- "Blocked by robots.txt"
    result$Notes <- "Scraping not allowed per robots.txt"
    return(result)
  }
  cat("OK\n")
  
  # Step 3: Search main page (Depth 1)
  cat("\n[STEP 1] Searching main page...\n")
  main_response <- safe_get(base_url)
  
  if (!main_response$success) {
    result$Status <- "Error accessing main page"
    result$Notes <- main_response$error
    cat("ERROR:", main_response$error, "\n")
    return(result)
  }
  
  # Get all links from main page
  main_links <- get_page_links(content(main_response$response), base_url)
  cat("Found", nrow(main_links), "links on main page\n")
  
  # Search for strategic plan links
  strategic_links <- main_links %>%
    filter(is_strategic_plan_link(url, text))
  
  cat("Found", nrow(strategic_links), "potential strategic plan links\n")
  
  # Look for PDF links
  pdf_links <- strategic_links %>%
    filter(is_pdf_link(url))
  
  if (nrow(pdf_links) > 0) {
    cat("SUCCESS! Found PDF strategic plan on main page\n")
    cat("URL:", pdf_links$url[1], "\n")
    
    # Download the PDF
    cat("Downloading PDF... ")
    download_result <- download_strategic_plan(pdf_links$url[1], hospital_name, fac)
    
    if (download_result$success) {
      cat("SUCCESS\n")
      result$Plan_Found <- TRUE
      result$PDF_URL <- pdf_links$url[1]
      result$Local_Path <- download_result$filepath
      result$Search_Method <- "Main page (Depth 1)"
      result$Status <- "Success"
      result$Notes <- "Found on main page"
      return(result)
    } else {
      cat("FAILED:", download_result$error, "\n")
      result$Plan_Found <- TRUE
      result$PDF_URL <- pdf_links$url[1]
      result$Search_Method <- "Main page (Depth 1)"
      result$Status <- "Found but download failed"
      result$Notes <- download_result$error
      return(result)
    }
  }
  
  # Check non-PDF strategic plan links (might lead to PDF)
  if (nrow(strategic_links) > 0) {
    cat("Checking strategic plan page links...\n")
    for (i in 1:min(3, nrow(strategic_links))) {  # Check first 3 links
      link_url <- strategic_links$url[i]
      cat("  Checking:", link_url, "... ")
      
      link_response <- safe_get(link_url)
      if (link_response$success) {
        link_page_links <- get_page_links(content(link_response$response), base_url)
        link_pdfs <- link_page_links %>%
          filter(is_pdf_link(url)) %>%
          filter(is_strategic_plan_link(url, text))
        
        if (nrow(link_pdfs) > 0) {
          cat("FOUND PDF!\n")
          cat("Downloading... ")
          download_result <- download_strategic_plan(link_pdfs$url[1], hospital_name, fac)
          
          if (download_result$success) {
            cat("SUCCESS\n")
            result$Plan_Found <- TRUE
            result$PDF_URL <- link_pdfs$url[1]
            result$Local_Path <- download_result$filepath
            result$Search_Method <- "Strategic plan page (Depth 1)"
            result$Status <- "Success"
            result$Notes <- paste("Found via", link_url)
            return(result)
          }
        } else {
          cat("No PDF\n")
        }
      }
    }
  }
  
  # Step 4: Fallback - Search "About" section
  cat("\n[STEP 2] Strategic plan not found on main page. Searching 'About' section...\n")
  
  about_links <- main_links %>%
    filter(str_detect(url_lower, paste(CONFIG$about_keywords, collapse = "|")))
  
  cat("Found", nrow(about_links), "'About' section links\n")
  
  if (nrow(about_links) > 0) {
    about_url <- about_links$url[1]
    cat("Checking:", about_url, "\n")
    
    about_response <- safe_get(about_url)
    if (about_response$success) {
      about_page_links <- get_page_links(content(about_response$response), base_url)
      cat("Found", nrow(about_page_links), "links in About section\n")
      
      # Search for strategic plans in about section
      about_strategic <- about_page_links %>%
        filter(is_strategic_plan_link(url, text))
      
      about_pdfs <- about_strategic %>%
        filter(is_pdf_link(url))
      
      if (nrow(about_pdfs) > 0) {
        cat("SUCCESS! Found PDF in About section\n")
        cat("Downloading... ")
        download_result <- download_strategic_plan(about_pdfs$url[1], hospital_name, fac)
        
        if (download_result$success) {
          cat("SUCCESS\n")
          result$Plan_Found <- TRUE
          result$PDF_URL <- about_pdfs$url[1]
          result$Local_Path <- download_result$filepath
          result$Search_Method <- "About section (Depth 2)"
          result$Status <- "Success"
          result$Notes <- "Found in About section"
          return(result)
        }
      }
    }
  }
  
  # Not found
  cat("\nNOT FOUND: Strategic plan not found after search\n")
  result$Status <- "Not Found"
  result$Notes <- "Strategic plan not found in main page or About section"
  return(result)
}

# Main Processing Function ----

process_hospitals <- function(input_csv) {
  cat("\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("Ontario Hospital Strategic Plan Scraper\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
  
  # Initialize directories
  initialize_directories()
  
  # Read input file
  cat("Reading input file:", input_csv, "\n")
  hospitals <- read.csv(input_csv, stringsAsFactors = FALSE)
  
  required_cols <- c("Hospital_Name", "FAC", "URL", "Type")
  missing_cols <- setdiff(required_cols, names(hospitals))
  
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  cat("Loaded", nrow(hospitals), "hospitals\n")
  cat("\nStarting scraping process...\n")
  
  # Process each hospital
  results <- pmap_dfr(
    list(
      hospitals$Hospital_Name,
      hospitals$FAC,
      hospitals$URL,
      hospitals$Type
    ),
    search_hospital_strategic_plan
  )
  
  # Save results
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  output_file <- file.path(CONFIG$output_path, paste0("results_", timestamp, ".csv"))
  write.csv(results, output_file, row.names = FALSE)
  
  # Print summary
  cat("\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("SUMMARY\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("Total hospitals processed:", nrow(results), "\n")
  cat("Strategic plans found:", sum(results$Plan_Found), "\n")
  cat("Not found:", sum(results$Status == "Not Found"), "\n")
  cat("Blocked by robots.txt:", sum(results$Status == "Blocked by robots.txt"), "\n")
  cat("Errors:", sum(str_detect(results$Status, "Error")), "\n")
  cat("\nResults saved to:", output_file, "\n")
  cat("PDFs saved to:", CONFIG$pdf_path, "\n")
  
  return(results)
}
process_hospitals("E:/Hospital_Strategic_Plans/source/input_csv.csv")
# Example Usage ----
# The script expects your CSV file to be in the 'source' folder
# Results will be saved to the 'Outputs' folder
# PDFs will be saved to the 'strategic_plans/pdfs' folder
#
# Example:
# results <- process_hospitals("E:/Hospital_Strategic_Plans/source/hospital_list.csv")
# 
# Or if your working directory is already set to the project folder:
# setwd("E:/Hospital_Strategic_Plans")
# results <- process_hospitals("source/hospital_list.csv")
#
# View results:
# View(results)