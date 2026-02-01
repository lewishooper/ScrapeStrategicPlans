# Phase 2: Ontario Hospital Strategic Plan Discovery & PDF Download
# Purpose: Search validated hospital websites for strategic plan PDFs and download them
# Author: Created for hospital strategic plan database project
# Date: 2025-11-28
# Enhanced: 2025-12-01 - Single output file, streamlined structure, enhanced manual review

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
  output_folder = "Outputs",
  pdf_folder = "strategic_plans",
  
  # File paths - UPDATED: Single output file approach
  master_yaml = "code/base_hospitals_validated.yaml",      # Master source (Phase 1)
  working_yaml = "code/Hospital_strategy.yaml",             # Working file (Phase 2)
  
  # Search keywords (case-insensitive)
  tier1_keywords = c(
    "strategic plan",
    "strategic-plan",
    "strat-plan",
    "strategic direction", 
    "strategic priorities"
  ),
  
  tier2_keywords = c(
    "multi-year plan",
    "multi year plan",
    "strategic framework",
    "future planning"
  ),
  
  # High-value sections worth searching deeper (depth-2)
  high_value_sections = c(
    "about",
    "about-us",
    "about us",
    "who-we-are",
    "who we are",
    "our-organization",
    "our organization",
    "publications",
    "reports",
    "governance",
    "corporate"
  ),
  
  # Low-value sections to skip
  skip_sections = c(
    "contact",
    "map",
    "directions",
    "location",
    "careers",
    "jobs",
    "employment",
    "news",
    "media",
    "events",
    "patient",
    "visitor",
    "donate",
    "donation",
    "foundation",
    "volunteer"
  ),
  
  # Search parameters
  max_pages_to_check = 5,
  max_depth2_pages = 3,
  search_depth = 2,
  
  # Ethical scraping
  delay_between_requests = 2,
  user_agent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
  request_timeout = 30,
  
  # Testing mode
  test_mode = TRUE,  # Set to FALSE for full run
  test_sample_size = 70,
  
  # Diagnostic mode
  diagnostic_mode = TRUE
)

# Initialize ----
initialize_project <- function() {
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("Phase 2: Strategic Plan Discovery & Download\n")
  cat("Single Working File Approach\n")
  cat(strrep("=", 70), "\n\n")
  
  # Display file configuration
  cat("[CONFIG] Master source: ", CONFIG$master_yaml, "\n")
  cat("[CONFIG] Working file:  ", CONFIG$working_yaml, "\n")
  
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
  } else {
    cat("\n*** FULL RUN MODE: Processing all hospitals ***\n")
  }
  
  if (CONFIG$diagnostic_mode) {
    cat("*** DIAGNOSTIC MODE: Verbose logging enabled ***\n")
    log_file <- file.path(output_path, paste0("diagnostic_log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"))
    cat("Diagnostic log will be saved to:", log_file, "\n")
    CONFIG$log_file <<- log_file
    sink(log_file, split = TRUE)
  }
  
  cat("\n")
  
  # Store paths globally
  CONFIG$pdf_path <<- pdf_path
  CONFIG$output_path <<- output_path
}

# Helper Functions ----

# NEW: Streamline hospital data for output file
streamline_hospital <- function(hospital) {
  list(
    FAC = hospital$FAC,
    name = hospital$name,
    hospital_type = hospital$hospital_type,
    base_url = hospital$base_url,
    strategy_search = if (!is.null(hospital$strategy_search)) {
      hospital$strategy_search
    } else {
      list(
        search_attempted = FALSE,
        search_date = NA,
        strategy_url_found = FALSE,
        strategy_url = "",
        pdf_found = FALSE,
        pdf_url = "",
        pdf_downloaded = FALSE,
        download_confidence = "",
        content_type = "",
        local_folder = "",
        local_filename = "",
        manual_pdf_url = "",
        requires_manual_review = FALSE,
        strategy_notes = ""
      )
    }
  )
}

# NEW: Read hospitals with first-run detection
read_yaml_hospitals <- function() {
  working_path <- file.path(CONFIG$base_dir, CONFIG$working_yaml)
  master_path <- file.path(CONFIG$base_dir, CONFIG$master_yaml)
  
  # Check if working file exists
  if (file.exists(working_path)) {
    cat("[LOAD] Reading existing working file: ", CONFIG$working_yaml, "\n")
    data <- read_yaml(working_path)
    
    # Handle both structures
    if (is.null(data$hospitals)) {
      hospitals <- data
    } else {
      hospitals <- data$hospitals
    }
    
    cat("[LOAD] Total hospitals in working file:", length(hospitals), "\n\n")
    return(hospitals)
    
  } else {
    cat("[FIRST RUN] Working file not found. Creating from master...\n")
    cat("[LOAD] Reading master file: ", CONFIG$master_yaml, "\n")
    
    if (!file.exists(master_path)) {
      stop("Master file not found: ", master_path)
    }
    
    data <- read_yaml(master_path)
    
    # Handle both structures
    if (is.null(data$hospitals)) {
      master_hospitals <- data
    } else {
      master_hospitals <- data$hospitals
    }
    
    cat("[LOAD] Total hospitals in master:", length(master_hospitals), "\n")
    
    # Filter to validated hospitals only and streamline
    validated_hospitals <- master_hospitals[sapply(master_hospitals, function(h) {
      !is.null(h$base_url_validated) && 
        (h$base_url_validated == "yes" || h$base_url_validated == TRUE) &&
        !is.null(h$robots_allowed) &&
        (h$robots_allowed == "yes" || h$robots_allowed == TRUE)
    })]
    
    cat("[FILTER] Validated and robots-allowed hospitals:", length(validated_hospitals), "\n")
    
    # Streamline structure (remove Phase 1 fields)
    streamlined <- lapply(validated_hospitals, streamline_hospital)
    
    cat("[CREATE] Creating new working file with streamlined structure\n\n")
    
    return(streamlined)
  }
}

# Write updated YAML file
write_yaml_hospitals <- function(hospitals) {
  yaml_path <- file.path(CONFIG$base_dir, CONFIG$working_yaml)
  
  # Create backup if file exists
  if (file.exists(yaml_path)) {
    backup_dir <- file.path(CONFIG$base_dir, "Backups")
    if (!dir.exists(backup_dir)) {
      dir.create(backup_dir, recursive = TRUE)
    }
    
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    backup_path <- file.path(backup_dir, paste0("Hospital_strategy_", timestamp, ".yaml"))
    
    file.copy(yaml_path, backup_path)
    cat("[BACKUP] Created:", basename(backup_path), "\n")
  }
  
  # Write updated YAML
  write_yaml(hospitals, yaml_path)
  
  cat("[SAVE] Updated working file:", CONFIG$working_yaml, "\n")
  cat("[SAVE] Total hospitals written:", length(hospitals), "\n")
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
  if (CONFIG$diagnostic_mode) {
    cat("    [HTTP GET] Requesting:", url, "\n")
  }
  
  Sys.sleep(CONFIG$delay_between_requests)
  
  tryCatch({
    response <- GET(
      url,
      user_agent(CONFIG$user_agent),
      timeout(timeout)
    )
    
    if (status_code(response) == 200) {
      if (CONFIG$diagnostic_mode) {
        cat("    [HTTP 200] Success - Content length:", length(content(response, "raw")), "bytes\n")
      }
      return(list(success = TRUE, response = response, error = NULL))
    } else {
      if (CONFIG$diagnostic_mode) {
        cat("    [HTTP", status_code(response), "] Failed\n")
      }
      return(list(
        success = FALSE,
        response = NULL,
        error = paste("HTTP", status_code(response))
      ))
    }
  }, error = function(e) {
    if (CONFIG$diagnostic_mode) {
      cat("    [ERROR]", e$message, "\n")
    }
    return(list(success = FALSE, response = NULL, error = e$message))
  })
}

# Get all links from a page
get_page_links <- function(response_content, base_url) {
  tryCatch({
    if (CONFIG$diagnostic_mode) {
      cat("    [PARSE] Extracting links from HTML...\n")
    }
    
    if (inherits(response_content, "xml_document")) {
      page <- response_content
    } else {
      page <- read_html(response_content)
    }
    
    links <- page %>%
      html_nodes("a") %>%
      html_attr("href")
    
    link_text <- page %>%
      html_nodes("a") %>%
      html_text() %>%
      str_trim()
    
    if (CONFIG$diagnostic_mode) {
      cat("    [PARSE] Found", length(links), "raw <a> tags\n")
    }
    
    if (length(links) == 0) {
      if (CONFIG$diagnostic_mode) {
        cat("    [PARSE] WARNING: No <a> tags found in HTML\n")
      }
      return(data.frame(
        url = character(0),
        text = character(0),
        stringsAsFactors = FALSE
      ))
    }
    
    df <- data.frame(
      url = links,
      text = link_text,
      stringsAsFactors = FALSE
    ) %>%
      filter(!is.na(url), url != "", url != "#") %>%
      mutate(
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
    
    if (CONFIG$diagnostic_mode) {
      cat("    [PARSE] After filtering:", nrow(df), "valid links\n")
    }
    
    return(df)
    
  }, error = function(e) {
    if (CONFIG$diagnostic_mode) {
      cat("    [PARSE ERROR]", e$message, "\n")
    }
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
  
  tier1_match <- any(sapply(CONFIG$tier1_keywords, function(kw) {
    str_detect(combined, fixed(kw, ignore_case = TRUE))
  }))
  
  if (tier1_match) return(list(match = TRUE, tier = 1))
  
  tier2_match <- any(sapply(CONFIG$tier2_keywords, function(kw) {
    str_detect(combined, fixed(kw, ignore_case = TRUE))
  }))
  
  if (tier2_match) return(list(match = TRUE, tier = 2))
  
  return(list(match = FALSE, tier = NA))
}

# Check if link is a high-value section worth searching
is_high_value_section <- function(url, text) {
  combined <- str_to_lower(paste(url, text))
  
  is_high_value <- any(sapply(CONFIG$high_value_sections, function(kw) {
    str_detect(combined, fixed(kw, ignore_case = TRUE))
  }))
  
  should_skip <- any(sapply(CONFIG$skip_sections, function(kw) {
    str_detect(combined, fixed(kw, ignore_case = TRUE))
  }))
  
  return(is_high_value && !should_skip)
}

# Check if URL is a PDF
is_pdf_url <- function(url) {
  str_detect(str_to_lower(url), "\\.pdf($|\\?)")
}

# Search for strategy page URLs (DEPTH 1 - Main Page)
search_for_strategy_page <- function(base_url) {
  cat("  [DEPTH-1] Searching main page for strategic plan links...\n")
  
  if (CONFIG$diagnostic_mode) {
    cat("  [DIAGNOSTIC MODE ENABLED]\n")
    cat("  Target base URL:", base_url, "\n")
  }
  
  response <- safe_get(base_url)
  
  if (!response$success) {
    cat("  ERROR: Could not access base URL -", response$error, "\n")
    return(list(found = FALSE, urls = NULL, high_value_links = NULL, error = response$error))
  }
  
  main_links <- get_page_links(content(response$response), base_url)
  
  if (nrow(main_links) == 0) {
    cat("  WARNING: No links found on main page\n")
    return(list(found = FALSE, urls = NULL, high_value_links = NULL, error = "No links found"))
  }
  
  cat("  Found", nrow(main_links), "total links on main page\n")
  
  if (CONFIG$diagnostic_mode) {
    cat("\n  === DIAGNOSTIC: First 20 links on page ===\n")
    for (i in 1:min(20, nrow(main_links))) {
      link <- main_links[i, ]
      cat("    ", i, ". URL:", substr(link$url, 1, 80), "\n")
      cat("       Text:", substr(link$text, 1, 60), "\n")
    }
    cat("  === END DIAGNOSTIC ===\n\n")
  }
  
  match_results <- lapply(1:nrow(main_links), function(i) {
    matches_strategy_keywords(main_links$url[i], main_links$text[i])
  })
  
  strategy_links <- main_links %>%
    mutate(
      is_match = sapply(match_results, function(x) x$match),
      tier = sapply(match_results, function(x) x$tier)
    ) %>%
    filter(is_match == TRUE) %>%
    arrange(tier, url) %>%
    select(url, text, tier)
  
  high_value_results <- lapply(1:nrow(main_links), function(i) {
    is_high_value_section(main_links$url[i], main_links$text[i])
  })
  
  high_value_links <- main_links %>%
    mutate(is_high_value = unlist(high_value_results)) %>%
    filter(is_high_value == TRUE) %>%
    select(url, text)
  
  if (CONFIG$diagnostic_mode && nrow(high_value_links) > 0) {
    cat("  [DEPTH-2 PREP] Found", nrow(high_value_links), "high-value sections for potential depth-2 search:\n")
    for (i in 1:min(3, nrow(high_value_links))) {
      cat("    ", i, ".", high_value_links$url[i], "\n")
    }
  }
  
  if (nrow(strategy_links) == 0) {
    cat("  [DEPTH-1] No strategic plan pages found on main page\n")
    return(list(found = FALSE, urls = NULL, high_value_links = high_value_links, error = "No strategy keywords on main page"))
  }
  
  cat("  [DEPTH-1] Found", nrow(strategy_links), "potential strategy pages:\n")
  for (i in 1:nrow(strategy_links)) {
    cat("    ", i, ". (Tier", strategy_links$tier[i], ")", strategy_links$url[i], "\n")
    cat("       Link text:", strategy_links$text[i], "\n")
  }
  
  if (nrow(strategy_links) > CONFIG$max_pages_to_check) {
    strategy_links <- strategy_links[1:CONFIG$max_pages_to_check, ]
    cat("  Limiting to first", CONFIG$max_pages_to_check, "pages\n")
  }
  
  return(list(found = TRUE, urls = strategy_links, high_value_links = high_value_links, error = NULL))
}

# Search high-value sections at depth-2
search_depth_2 <- function(high_value_links, base_url) {
  cat("  [DEPTH-2] Searching high-value sections for strategic plan links...\n")
  
  if (is.null(high_value_links) || nrow(high_value_links) == 0) {
    cat("  [DEPTH-2] No high-value sections to search\n")
    return(list(found = FALSE, urls = NULL, error = "No high-value sections"))
  }
  
  pages_to_check <- min(nrow(high_value_links), CONFIG$max_depth2_pages)
  high_value_links <- high_value_links[1:pages_to_check, ]
  
  cat("  [DEPTH-2] Checking", pages_to_check, "high-value section(s):\n")
  
  all_strategy_links <- data.frame()
  
  for (i in 1:nrow(high_value_links)) {
    section_url <- high_value_links$url[i]
    cat("    ", i, ". Checking:", section_url, "\n")
    
    response <- safe_get(section_url)
    
    if (!response$success) {
      cat("       ERROR:", response$error, "\n")
      next
    }
    
    section_links <- get_page_links(content(response$response), base_url)
    
    if (nrow(section_links) == 0) {
      cat("       No links found\n")
      next
    }
    
    match_results <- lapply(1:nrow(section_links), function(j) {
      matches_strategy_keywords(section_links$url[j], section_links$text[j])
    })
    
    strategy_matches <- section_links %>%
      mutate(
        is_match = sapply(match_results, function(x) x$match),
        tier = sapply(match_results, function(x) x$tier)
      ) %>%
      filter(is_match == TRUE) %>%
      arrange(tier, url) %>%
      select(url, text, tier)
    
    if (nrow(strategy_matches) > 0) {
      cat("       ✓ Found", nrow(strategy_matches), "strategic plan link(s)!\n")
      all_strategy_links <- bind_rows(all_strategy_links, strategy_matches)
    } else {
      cat("       No strategic plan links found\n")
    }
  }
  
  if (nrow(all_strategy_links) == 0) {
    cat("  [DEPTH-2] No strategic plan pages found in high-value sections\n")
    return(list(found = FALSE, urls = NULL, error = "No strategy keywords in depth-2 search"))
  }
  
  all_strategy_links <- all_strategy_links %>%
    distinct(url, .keep_all = TRUE) %>%
    arrange(tier, url)
  
  cat("  [DEPTH-2] Total unique strategy pages found:", nrow(all_strategy_links), "\n")
  for (i in 1:min(3, nrow(all_strategy_links))) {
    cat("    ", i, ". (Tier", all_strategy_links$tier[i], ")", all_strategy_links$url[i], "\n")
  }
  
  if (nrow(all_strategy_links) > CONFIG$max_pages_to_check) {
    all_strategy_links <- all_strategy_links[1:CONFIG$max_pages_to_check, ]
  }
  
  return(list(found = TRUE, urls = all_strategy_links, error = NULL))
}

# Find PDF on a strategy page
find_strategy_pdf <- function(strategy_url, base_url) {
  cat("  Checking:", strategy_url, "\n")
  
  response <- safe_get(strategy_url)
  
  if (!response$success) {
    cat("    ERROR:", response$error, "\n")
    return(list(found = FALSE, pdf_url = NULL, confidence = NULL, error = response$error))
  }
  
  page_links <- get_page_links(content(response$response), base_url)
  
  if (nrow(page_links) == 0) {
    cat("    No links found on page\n")
    return(list(found = FALSE, pdf_url = NULL, confidence = NULL, error = "No links on page"))
  }
  
  all_pdfs <- page_links %>%
    filter(is_pdf_url(url))
  
  if (nrow(all_pdfs) == 0) {
    cat("    No PDFs found on page\n")
    return(list(found = FALSE, pdf_url = NULL, confidence = NULL, error = "No PDF on page"))
  }
  
  cat("    Found", nrow(all_pdfs), "PDF(s) on page\n")
  
  match_results <- lapply(1:nrow(all_pdfs), function(i) {
    matches_strategy_keywords(all_pdfs$url[i], all_pdfs$text[i])
  })
  
  keyword_pdfs <- all_pdfs %>%
    mutate(
      is_match = sapply(match_results, function(x) x$match),
      tier = sapply(match_results, function(x) x$tier)
    ) %>%
    filter(is_match == TRUE) %>%
    arrange(tier, url)
  
  if (nrow(keyword_pdfs) > 0) {
    cat("    ✓ PDF found with strategy keywords:", basename(keyword_pdfs$url[1]), "\n")
    return(list(found = TRUE, pdf_url = keyword_pdfs$url[1], confidence = "high", error = NULL))
  }
  
  cat("    No keyword match in PDFs, taking first PDF (medium confidence)\n")
  cat("    ✓ PDF found:", basename(all_pdfs$url[1]), "\n")
  return(list(found = TRUE, pdf_url = all_pdfs$url[1], confidence = "medium", error = NULL))
}

# Download PDF
download_pdf <- function(pdf_url, fac, hospital_name) {
  safe_name <- clean_hospital_name(hospital_name)
  folder_name <- paste0(fac, "_", safe_name)
  folder_path <- file.path(CONFIG$pdf_path, folder_name)
  
  dir.create(folder_path, showWarnings = FALSE, recursive = TRUE)
  
  year_month <- format(Sys.time(), "%Y%m")
  filename <- paste0("Strategy_", year_month, "_", fac, ".pdf")
  filepath <- file.path(folder_path, filename)
  
  cat("  Downloading PDF...\n")
  cat("    Destination:", file.path(folder_name, filename), "\n")
  
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
    download_confidence = "",
    content_type = "",
    local_folder = "",
    local_filename = "",
    manual_pdf_url = "",
    requires_manual_review = FALSE,
    strategy_notes = ""
  )
  
  # ENHANCED: Check for manual URLs first
  manual_pdf <- NULL
  manual_strategy <- NULL
  
  if (!is.null(hospital$strategy_search$manual_pdf_url) && 
      hospital$strategy_search$manual_pdf_url != "") {
    manual_pdf <- hospital$strategy_search$manual_pdf_url
  }
  
  if (!is.null(hospital$strategy_search$strategy_url) && 
      hospital$strategy_search$strategy_url != "") {
    manual_strategy <- hospital$strategy_search$strategy_url
  }
  
  # If manual URLs provided, use them
  if (!is.null(manual_pdf)) {
    cat("  [MANUAL] Found manual PDF URL from previous review\n")
    cat("  [MANUAL] PDF URL:", manual_pdf, "\n")
    
    if (!is.null(manual_strategy)) {
      cat("  [MANUAL] Strategy URL:", manual_strategy, "\n")
      result$strategy_url <- manual_strategy
    } else {
      result$strategy_url <- manual_pdf  # Default to PDF URL
    }
    
    result$strategy_url_found <- TRUE
    result$pdf_found <- TRUE
    result$pdf_url <- manual_pdf
    result$download_confidence <- "manual"
    
    # Download the manually specified PDF
    download_result <- download_pdf(manual_pdf, fac, name)
    
    if (download_result$success) {
      result$pdf_downloaded <- TRUE
      result$local_folder <- download_result$folder
      result$local_filename <- download_result$filename
      result$requires_manual_review <- FALSE
      result$strategy_notes <- "Downloaded from manually provided URL"
      return(result)
    } else {
      result$requires_manual_review <- TRUE
      result$strategy_notes <- paste("Manual PDF URL provided but download failed:", download_result$error)
      return(result)
    }
  }
  
  # No manual URLs - proceed with automated search
  # DEPTH-1: Search main page for strategy pages
  search_result <- search_for_strategy_page(base_url)
  
  if (search_result$found) {
    result$strategy_url_found <- TRUE
    
    for (i in 1:nrow(search_result$urls)) {
      strategy_url <- search_result$urls$url[i]
      result$strategy_url <- strategy_url
      # NEW: Check if the strategy URL itself is a direct PDF link
      if (is_pdf_url(strategy_url)) {
        cat("  [PDF] Strategy URL is a direct PDF link!\n")
        result$pdf_found <- TRUE
        result$pdf_url <- strategy_url
        result$download_confidence <- "high"
        
        download_result <- download_pdf(strategy_url, fac, name)
        
        if (download_result$success) {
          result$pdf_downloaded <- TRUE
          result$local_folder <- download_result$folder
          result$local_filename <- download_result$filename
          result$strategy_notes <- paste0("Direct PDF link found (depth-1): ", strategy_url, " (confidence: high)")
          result$requires_manual_review <- FALSE
          return(result)
        } else {
          result$manual_pdf_url <- strategy_url
          result$requires_manual_review <- TRUE
          result$strategy_notes <- paste("Direct PDF URL found but download failed:", download_result$error, "- PDF URL saved for manual review")
          return(result)
        }
      }
      
      pdf_result <- find_strategy_pdf(strategy_url, base_url)
      
      if (pdf_result$found) {
        result$pdf_found <- TRUE
        result$pdf_url <- pdf_result$pdf_url
        result$download_confidence <- pdf_result$confidence
        
        download_result <- download_pdf(pdf_result$pdf_url, fac, name)
        
        if (download_result$success) {
          result$pdf_downloaded <- TRUE
          result$local_folder <- download_result$folder
          result$local_filename <- download_result$filename
          result$strategy_notes <- paste0("Found on page (depth-1): ", strategy_url, 
                                          " (confidence: ", pdf_result$confidence, ")")
          
          if (pdf_result$confidence == "medium") {
            result$requires_manual_review <- TRUE
            result$strategy_notes <- paste0(result$strategy_notes, 
                                            " - Please verify this is the correct strategic plan")
          }
          
          return(result)
        } else {
          result$manual_pdf_url <- pdf_result$pdf_url
          result$requires_manual_review <- TRUE
          result$strategy_notes <- paste("PDF found but download failed:", download_result$error, 
                                         "- PDF URL saved for manual review")
          return(result)
        }
      }
    }
    
    result$requires_manual_review <- TRUE
    result$strategy_notes <- paste("Strategy page found (depth-1) but no PDF. Checked", 
                                   nrow(search_result$urls), "pages. URL:", result$strategy_url)
    return(result)
  }
  
  # DEPTH-2: Not found at depth-1, try searching high-value sections
  cat("  [DEPTH-1] No strategy pages found, trying depth-2 search...\n")
  
  depth2_result <- search_depth_2(search_result$high_value_links, base_url)
  
  if (!depth2_result$found) {
    result$strategy_notes <- paste("No strategy pages found after depth-1 and depth-2 search.", 
                                   "Checked main page and", 
                                   ifelse(is.null(search_result$high_value_links), 0, nrow(search_result$high_value_links)),
                                   "high-value sections")
    result$requires_manual_review <- TRUE
    return(result)
  }
  
  result$strategy_url_found <- TRUE
  
  for (i in 1:nrow(depth2_result$urls)) {
    strategy_url <- depth2_result$urls$url[i]
    result$strategy_url <- strategy_url
    
    # NEW: Check if the strategy URL itself is a direct PDF link (DEPTH-2)
    if (is_pdf_url(strategy_url)) {
      cat("  [PDF] Strategy URL is a direct PDF link (depth-2)!\n")
      result$pdf_found <- TRUE
      result$pdf_url <- strategy_url
      result$download_confidence <- "high"
      
      download_result <- download_pdf(strategy_url, fac, name)
      
      if (download_result$success) {
        result$pdf_downloaded <- TRUE
        result$local_folder <- download_result$folder
        result$local_filename <- download_result$filename
        result$strategy_notes <- paste0("Direct PDF link found (depth-2): ", strategy_url, " (confidence: high)")
        result$requires_manual_review <- FALSE
        return(result)
      } else {
        result$manual_pdf_url <- strategy_url
        result$requires_manual_review <- TRUE
        result$strategy_notes <- paste("Direct PDF URL found (depth-2) but download failed:", download_result$error, "- PDF URL saved for manual review")
        return(result)
      }
    }
    
    # Continue with existing logic
    pdf_result <- find_strategy_pdf(strategy_url, base_url)
    
    
    pdf_result <- find_strategy_pdf(strategy_url, base_url)
    
    if (pdf_result$found) {
      result$pdf_found <- TRUE
      result$pdf_url <- pdf_result$pdf_url
      result$download_confidence <- pdf_result$confidence
      
      download_result <- download_pdf(pdf_result$pdf_url, fac, name)
      
      if (download_result$success) {
        result$pdf_downloaded <- TRUE
        result$local_folder <- download_result$folder
        result$local_filename <- download_result$filename
        result$strategy_notes <- paste0("Found on page (depth-2): ", strategy_url, 
                                        " (confidence: ", pdf_result$confidence, ")")
        
        if (pdf_result$confidence == "medium") {
          result$requires_manual_review <- TRUE
          result$strategy_notes <- paste0(result$strategy_notes, 
                                          " - Please verify this is the correct strategic plan")
        }
        
        return(result)
      } else {
        result$manual_pdf_url <- pdf_result$pdf_url
        result$requires_manual_review <- TRUE
        result$strategy_notes <- paste("PDF found (depth-2) but download failed:", download_result$error, 
                                       "- PDF URL saved for manual review")
        return(result)
      }
    }
  }
  
  result$requires_manual_review <- TRUE
  result$strategy_notes <- paste("Strategy page found (depth-2) but no PDF. Checked", 
                                 nrow(depth2_result$urls), "pages. URL:", result$strategy_url)
  
  return(result)
}

# Main Processing Function ----
run_phase2 <- function() {
  initialize_project()
  
  # Read hospitals (handles first-run automatically)
  hospitals <- read_yaml_hospitals()
  
  # Test mode - limit sample
  if (CONFIG$test_mode) {
    hospitals <- hospitals[1:min(CONFIG$test_sample_size, length(hospitals))]
    cat("[FILTER] Processing", length(hospitals), "of total hospitals\n\n")
  } else {
    cat("[FULL RUN] Processing all", length(hospitals), "hospitals\n\n")
  }
  
  # Process each hospital
  cat("Starting processing...\n")
  
  for (i in seq_along(hospitals)) {
    hospital <- hospitals[[i]]
    
    result <- process_hospital(hospital)
    
    hospitals[[i]]$strategy_search <- result
    
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
  
  if (CONFIG$diagnostic_mode) {
    sink()
    cat("\nDiagnostic log saved to:", CONFIG$log_file, "\n")
  }
  
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
        download_confidence = "",
        local_folder = "",
        local_filename = "",
        manual_pdf_url = "",
        requires_manual_review = FALSE,
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
      Download_Confidence = ifelse(is.null(search$download_confidence), "", search$download_confidence),
      Local_Folder = search$local_folder,
      Local_Filename = search$local_filename,
      Manual_PDF_URL = ifelse(is.null(search$manual_pdf_url), "", search$manual_pdf_url),
      Requires_Manual_Review = ifelse(is.null(search$requires_manual_review), FALSE, search$requires_manual_review),
      Notes = search$strategy_notes,
      Search_Date = search$search_date,
      stringsAsFactors = FALSE
    )
  })
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  csv_file <- file.path(CONFIG$output_path, paste0("phase2_summary_", timestamp, ".csv"))
  write.csv(summary_data, csv_file, row.names = FALSE)
  
  cat("Summary CSV saved:", csv_file, "\n")
  
  manual_review <- summary_data %>%
    filter(Requires_Manual_Review == TRUE)
  
  if (nrow(manual_review) > 0) {
    manual_file <- file.path(CONFIG$output_path, paste0("manual_review_needed_", timestamp, ".csv"))
    write.csv(manual_review, manual_file, row.names = FALSE)
    cat("Manual review CSV saved:", manual_file, "\n")
    cat("  ->", nrow(manual_review), "hospitals require manual review\n")
  }
  
  return(summary_data)
}

# Print final summary
print_final_summary <- function(hospitals) {
  cat("\n", strrep("=", 70), "\n")
  cat("FINAL SUMMARY\n")
  cat(strrep("=", 70), "\n")
  
  total <- length(hospitals)
  
  attempted <- 0
  url_found <- 0
  pdf_found <- 0
  downloaded <- 0
  high_confidence <- 0
  medium_confidence <- 0
  manual_confidence <- 0
  needs_review <- 0
  
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
        
        conf <- search$download_confidence
        if (!is.null(conf)) {
          if (conf == "high") high_confidence <- high_confidence + 1
          if (conf == "medium") medium_confidence <- medium_confidence + 1
          if (conf == "manual") manual_confidence <- manual_confidence + 1
        }
      }
      if (!is.null(search$requires_manual_review) && (search$requires_manual_review == TRUE || search$requires_manual_review == "true")) {
        needs_review <- needs_review + 1
      }
    }
  }
  
  not_found <- attempted - url_found
  
  cat("Total hospitals:", total, "\n")
  cat("Search attempted:", attempted, "\n")
  cat("Strategy URL found:", url_found, "\n")
  cat("PDF found:", pdf_found, "\n")
  cat("PDF downloaded:", downloaded, "\n")
  cat("  - High confidence:", high_confidence, "\n")
  cat("  - Medium confidence:", medium_confidence, "(verify correct plan)\n")
  cat("  - Manual URL provided:", manual_confidence, "\n")
  cat("Not found:", not_found, "\n")
  cat("Requires manual review:", needs_review, "\n")
  cat("\n")
  if (attempted > 0) {
    success_rate <- round(downloaded / attempted * 100, 0)
    cat("Success rate:", success_rate, "%\n")
    cat("\nActions:\n")
    if (success_rate >= 80) {
      cat("✓ Excellent! Ready for full deployment or expand to 25+ hospitals\n")
    } else if (success_rate >= 60) {
      cat("✓ Good progress! Consider expanding to 25 hospitals\n")
    } else if (success_rate >= 40) {
      cat("→ Review diagnostic logs and adjust keywords/search depth\n")
    } else {
      cat("→ Review failures and refine search strategy\n")
    }
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