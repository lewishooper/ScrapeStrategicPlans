# ============================================================================
# STRATEGIC PDF DOWNLOAD, TEXT EXTRACTION, AND COMPARISON SCRIPT
# HYBRID APPROACH: Text-Based Change Detection + Image-Based API Processing
# ============================================================================
# Purpose: Download hospital strategic plan PDFs, extract text, detect changes
#          Supports baseline mode (fresh start) and change detection mode
# Author: Skip
# Created: 2026-01-19
# Version: 1.2
# Updated: 2026-01-22 - Filter removal, robust NA checking, skip tracking
#
# Usage: 
#   Baseline Mode (first run):    CONFIG$baseline_mode <- TRUE; main()
#   Change Detection (ongoing):   CONFIG$baseline_mode <- FALSE; main()
# ============================================================================

# REQUIRED LIBRARIES ----
library(yaml)
library(httr)
library(dplyr)
library(stringr)
library(purrr)
library(lubridate)
library(pdftools)
library(tools)      # For md5sum() fallback
library(rvest)      # For HTML extraction
library(xml2)       # For html node removal (compatibility)

# CONFIGURATION ----
CONFIG <- list(
  # Paths
  base_dir = "E:/Hospital_Strategic_Plans",
  yaml_file = "code/Hospital_strategy.yaml",
  pdf_folder = "strategic_plans",
  log_folder = "Outputs/download_logs",
  
  # Operating modes
  baseline_mode = TRUE,  # TRUE = Download all, no comparison (first run)
  # FALSE = Compare and only save if changed
  
  # Comparison settings (only used when baseline_mode = FALSE)
  comparison_method = "text",  # Primary: "text", Fallback: "binary"
  fallback_to_binary = TRUE,   # Use binary if text extraction fails
  
  # File management
  delete_unchanged_files = TRUE,  # Delete temp files if no change detected
  
  # Text extraction
  text_encoding = "UTF-8",
  min_text_chars = 50,  # Minimum characters to consider valid extraction
  
  # File validation
  min_pdf_size_kb = 10,
  max_pdf_size_kb = 100000,  # 100 MB
  
  # Network settings
  user_agent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
  request_timeout = 30,
  delay_between_requests = 2,
  retry_attempts = 1,
  retry_delay = 5,
  
  # Disk space
  min_free_space_gb = 1,
  
  # Processing options
  skip_previously_checked_today = FALSE,  # Set TRUE to skip hospitals checked in last 24h
  create_backup_before_yaml_update = TRUE,
  verbose = TRUE,
  
  # Testing/Development
  test_mode = FALSE,
  test_fac_codes = c("592", "593", "597")
)

# INITIALIZATION ----
initialize_environment <- function() {
  cat("\n")
  cat(strrep("=", 75), "\n")
  cat("STRATEGIC PDF DOWNLOAD & COMPARISON SCRIPT\n")
  cat("HYBRID APPROACH: Text-Based Change Detection\n")
  cat(strrep("=", 75), "\n\n")
  
  # Display mode
  if (CONFIG$baseline_mode) {
    cat("[MODE] BASELINE MODE - Creating fresh baseline (no comparison)\n")
    cat("       All PDFs will be downloaded and saved\n")
  } else {
    cat("[MODE] CHANGE DETECTION MODE - Comparing against previous versions\n")
    cat("       Only changed PDFs will be saved\n")
  }
  cat("\n")
  
  # Check base directory
  if (!dir.exists(CONFIG$base_dir)) {
    stop("Base directory not found: ", CONFIG$base_dir)
  }
  
  # Create directories
  pdf_path <- file.path(CONFIG$base_dir, CONFIG$pdf_folder)
  log_path <- file.path(CONFIG$base_dir, CONFIG$log_folder)
  
  dir.create(pdf_path, showWarnings = FALSE, recursive = TRUE)
  dir.create(log_path, showWarnings = FALSE, recursive = TRUE)
  
  cat("[CONFIG] Base directory:", CONFIG$base_dir, "\n")
  cat("[CONFIG] YAML file:", CONFIG$yaml_file, "\n")
  cat("[CONFIG] PDF folder:", pdf_path, "\n")
  cat("[CONFIG] Log folder:", log_path, "\n")
  
  # Check disk space
  tryCatch({
    disk_info <- as.numeric(system(paste0("wmic logicaldisk where \"DeviceID='", substr(CONFIG$base_dir, 1, 2), "'\" get FreeSpace"), intern = TRUE)[2]) / (1024^3)
    if (!is.na(disk_info) && disk_info < CONFIG$min_free_space_gb) {
      warning(paste0("Low disk space: ", round(disk_info, 2), " GB available"))
    } else if (!is.na(disk_info)) {
      cat("[DISK] Available space:", round(disk_info, 2), "GB\n")
    }
  }, error = function(e) {
    cat("[DISK] Could not check disk space\n")
  })
  
  if (CONFIG$test_mode) {
    cat("\n*** TEST MODE: Processing only", length(CONFIG$test_fac_codes), "hospitals ***\n")
  }
  
  cat("\n")
  
  # Store paths globally
  CONFIG$pdf_path <<- pdf_path
  CONFIG$log_path <<- log_path
}

# UTILITY FUNCTIONS ----

#' Get current timestamp
get_timestamp <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
}

#' Get timestamp for filenames
get_file_timestamp <- function() {
  format(Sys.time(), "%Y%m%d_%H%M%S")
}

#' Format FAC code with leading zeros
format_fac <- function(fac) {
  sprintf("%03d", as.numeric(fac))
}

#' Sanitize text for filenames
sanitize_filename <- function(text) {
  text %>%
    str_replace_all("[^a-zA-Z0-9_-]", "_") %>%
    str_replace_all("_{2,}", "_") %>%
    str_sub(1, 100)
}

# YAML OPERATIONS ----

#' Read hospitals from YAML
read_yaml_hospitals <- function() {
  yaml_path <- file.path(CONFIG$base_dir, CONFIG$yaml_file)
  
  cat("[LOAD] Reading YAML file:", CONFIG$yaml_file, "\n")
  
  if (!file.exists(yaml_path)) {
    stop("YAML file not found: ", yaml_path)
  }
  
  data <- read_yaml(yaml_path)
  
  # Handle both structures
  if (is.null(data$hospitals)) {
    hospitals <- data
  } else {
    hospitals <- data$hospitals
  }
  
  cat("[LOAD] Total hospitals in YAML:", length(hospitals), "\n\n")
  
  return(hospitals)
}

#' Write updated YAML with backup
write_yaml_hospitals <- function(hospitals) {
  yaml_path <- file.path(CONFIG$base_dir, CONFIG$yaml_file)
  
  # Create backup
  if (CONFIG$create_backup_before_yaml_update) {
    backup_path <- paste0(yaml_path, ".backup_", get_file_timestamp())
    file.copy(yaml_path, backup_path)
    cat("[BACKUP] Created:", basename(backup_path), "\n")
  }
  
  # Write YAML
  write_yaml(hospitals, yaml_path)
  cat("[SAVE] Updated YAML file\n\n")
}

# FILE SYSTEM FUNCTIONS ----

#' Get hospital folder path
get_hospital_folder <- function(hospital) {
  local_folder <- hospital$strategy_search$local_folder
  
  if (is.null(local_folder) || local_folder == "") {
    fac <- format_fac(hospital$FAC)
    name <- sanitize_filename(hospital$name)
    local_folder <- paste0(fac, "_", name)
  }
  
  folder_path <- file.path(CONFIG$pdf_path, local_folder)
  return(folder_path)
}

#' Generate PDF filename
generate_pdf_filename <- function(fac) {
  yyyymm <- format(Sys.time(), "%Y%m")
  fac_formatted <- format_fac(fac)
  paste0("Strategy_", yyyymm, "_", fac_formatted, ".pdf")
}

#' Generate TXT filename from PDF filename
pdf_to_txt_filename <- function(pdf_filename) {
  str_replace(pdf_filename, "\\.pdf$", ".txt")
}

#' Get most recent PDF file for a hospital
get_most_recent_pdf <- function(folder_path) {
  if (!dir.exists(folder_path)) {
    return(NULL)
  }
  
  pdf_files <- list.files(folder_path, pattern = "\\.pdf$", full.names = TRUE, ignore.case = TRUE)
  
  if (length(pdf_files) == 0) {
    return(NULL)
  }
  
  # Sort by modification time, newest first
  file_info <- file.info(pdf_files)
  pdf_files <- pdf_files[order(file_info$mtime, decreasing = TRUE)]
  
  return(pdf_files[1])
}

#' Get most recent TXT file for a hospital
get_most_recent_txt <- function(folder_path) {
  if (!dir.exists(folder_path)) {
    return(NULL)
  }
  
  txt_files <- list.files(folder_path, pattern = "\\.txt$", full.names = TRUE, ignore.case = TRUE)
  
  if (length(txt_files) == 0) {
    return(NULL)
  }
  
  # Sort by modification time, newest first
  file_info <- file.info(txt_files)
  txt_files <- txt_files[order(file_info$mtime, decreasing = TRUE)]
  
  return(txt_files[1])
}

# DOWNLOAD FUNCTIONS ----

#' Download PDF with retry logic
#' @param url Character. URL to download
#' @param dest_path Character. Destination file path
#' @return List with status, http_code, file_size_kb, error_message
download_pdf_with_retry <- function(url, dest_path) {
  
  result <- list(
    success = FALSE,
    http_code = 0,
    file_size_kb = 0,
    error_message = ""
  )
  
  max_attempts <- CONFIG$retry_attempts + 1
  
  for (attempt in 1:max_attempts) {
    
    tryCatch({
      # Download file
      response <- GET(
        url,
        user_agent(CONFIG$user_agent),
        timeout(CONFIG$request_timeout),
        write_disk(dest_path, overwrite = TRUE)
      )
      
      result$http_code <- status_code(response)
      
      if (result$http_code == 200) {
        # Success
        if (file.exists(dest_path)) {
          result$success <- TRUE
          result$file_size_kb <- round(file.size(dest_path) / 1024, 2)
        } else {
          result$error_message <- "File not created despite HTTP 200"
        }
        break
        
      } else {
        result$error_message <- paste0("HTTP ", result$http_code)
        if (attempt < max_attempts) {
          Sys.sleep(CONFIG$retry_delay)
        }
      }
      
    }, error = function(e) {
      result$error_message <<- paste("Download error:", e$message)
      if (attempt < max_attempts) {
        Sys.sleep(CONFIG$retry_delay)
      }
    })
  }
  
  return(result)
}

#' Validate downloaded PDF
#' @param pdf_path Character. Path to PDF file
#' @return List with valid (logical), error_message
validate_pdf <- function(pdf_path) {
  
  result <- list(
    valid = FALSE,
    error_message = ""
  )
  
  # Check file exists
  if (!file.exists(pdf_path)) {
    result$error_message <- "File does not exist"
    return(result)
  }
  
  # Check file size
  file_size_kb <- file.size(pdf_path) / 1024
  
  if (file_size_kb < CONFIG$min_pdf_size_kb) {
    result$error_message <- paste0("File too small (", round(file_size_kb, 2), " KB)")
    return(result)
  }
  
  if (file_size_kb > CONFIG$max_pdf_size_kb) {
    result$error_message <- paste0("File too large (", round(file_size_kb, 2), " KB)")
    return(result)
  }
  
  # Check PDF magic bytes
  tryCatch({
    con <- file(pdf_path, "rb")
    magic_bytes <- readBin(con, "raw", n = 4)
    close(con)
    
    # PDF files start with "%PDF"
    if (!identical(magic_bytes[1:4], as.raw(c(0x25, 0x50, 0x44, 0x46)))) {
      result$error_message <- "Not a valid PDF file (wrong magic bytes)"
      return(result)
    }
  }, error = function(e) {
    result$error_message <- paste("Could not validate PDF:", e$message)
    return(result)
  })
  
  result$valid <- TRUE
  return(result)
}

# TEXT EXTRACTION FUNCTIONS ----

#' Extract text from PDF
#' @param pdf_path Character. Path to PDF file
#' @return List with success, text, pages, total_chars, error_message
extract_text_from_pdf <- function(pdf_path) {
  
  result <- list(
    success = FALSE,
    text = NULL,
    pages = 0,
    total_chars = 0,
    error_message = ""
  )
  
  tryCatch({
    extracted_text <- pdf_text(pdf_path)
    
    if (is.null(extracted_text) || length(extracted_text) == 0) {
      result$error_message <- "No text extracted (empty result)"
      return(result)
    }
    
    pages <- length(extracted_text)
    total_chars <- sum(nchar(extracted_text))
    
    if (total_chars < CONFIG$min_text_chars) {
      # Still mark as success but with warning
      result$success <- TRUE
      result$text <- extracted_text
      result$pages <- pages
      result$total_chars <- total_chars
      result$error_message <- paste0("Warning: Only ", total_chars, " chars (possible image-only PDF)")
      return(result)
    }
    
    result$success <- TRUE
    result$text <- extracted_text
    result$pages <- pages
    result$total_chars <- total_chars
    
  }, error = function(e) {
    result$error_message <- paste("Text extraction failed:", e$message)
  })
  
  return(result)
}

#' Save text to file
#' @param text Character vector. Text to save
#' @param txt_path Character. Destination path
#' @return Logical. Success
save_text_file <- function(text, txt_path) {
  tryCatch({
    writeLines(text, txt_path, useBytes = FALSE)
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

# HTML EXTRACTION FUNCTIONS ----

#' Extract text directly from HTML strategic plan page
#' @param url Character. URL of HTML page
#' @param txt_path Character. Destination text file path
#' @return List with success, text, total_chars, error_message
extract_html_to_text <- function(url, txt_path) {
  
  result <- list(
    success = FALSE,
    text = "",
    total_chars = 0,
    pages = 1,  # HTML counts as single "page"
    error_message = ""
  )
  
  tryCatch({
    # Read HTML page
    page <- read_html(url)
    
    # Remove unwanted elements (compatible with older rvest)
    unwanted <- page %>% 
      html_nodes("script, style, nav, footer, header, .menu, .navigation, .navbar, .sidebar")
    
    if (length(unwanted) > 0) {
      xml2::xml_remove(unwanted)
    }
    
    # Try to find main content area first
    main_content <- page %>% 
      html_nodes("main, article, .content, #content, .main-content, [role='main']")
    
    if (length(main_content) == 0) {
      # Fallback: get body text
      main_content <- page %>% html_nodes("body")
    }
    
    # Extract text
    text <- main_content %>%
      html_text() %>%
      str_squish() %>%  # Remove extra whitespace
      paste(collapse = "\n\n")  # Join multiple sections
    
    # Validate extraction
    if (nchar(text) < CONFIG$min_text_chars) {
      result$error_message <- paste0("Only ", nchar(text), " characters extracted from HTML (min: ", CONFIG$min_text_chars, ")")
      return(result)
    }
    
    # Save text file
    tryCatch({
      writeLines(text, txt_path)
    }, error = function(e) {
      result$error_message <<- paste("Failed to save text file:", e$message)
      return(result)
    })
    
    result$success <- TRUE
    result$text <- text
    result$total_chars <- nchar(text)
    result$pages <- 1
    
  }, error = function(e) {
    result$error_message <<- paste("HTML extraction failed:", e$message)
  })
  
  return(result)
}

# COMPARISON FUNCTIONS ----

#' Compare text files for changes
#' @param new_txt Character. Path to new text file
#' @param old_txt Character. Path to old text file
#' @return List with identical (logical), method, details
compare_text_files <- function(new_txt, old_txt) {
  
  result <- list(
    identical = FALSE,
    method = "text",
    details = ""
  )
  
  if (!file.exists(old_txt)) {
    result$details <- "No previous version to compare"
    return(result)
  }
  
  tryCatch({
    new_text <- readLines(new_txt, warn = FALSE)
    old_text <- readLines(old_txt, warn = FALSE)
    
    result$identical <- identical(new_text, old_text)
    
    if (result$identical) {
      result$details <- "Text content identical"
    } else {
      result$details <- "Text content differs"
    }
    
  }, error = function(e) {
    result$details <- paste("Comparison error:", e$message)
  })
  
  return(result)
}

#' Compare PDFs using binary hash (fallback)
#' @param new_pdf Character. Path to new PDF
#' @param old_pdf Character. Path to old PDF
#' @return List with identical (logical), method, details
compare_pdfs_binary <- function(new_pdf, old_pdf) {
  
  result <- list(
    identical = FALSE,
    method = "binary",
    details = ""
  )
  
  if (!file.exists(old_pdf)) {
    result$details <- "No previous version to compare"
    return(result)
  }
  
  tryCatch({
    new_hash <- md5sum(new_pdf)
    old_hash <- md5sum(old_pdf)
    
    result$identical <- (new_hash == old_hash)
    
    if (result$identical) {
      result$details <- "Binary hash identical"
    } else {
      result$details <- "Binary hash differs"
    }
    
  }, error = function(e) {
    result$details <- paste("Hash comparison error:", e$message)
  })
  
  return(result)
}

# CORE PROCESSING ----

#' Process a single hospital
#' @param hospital List. Hospital data from YAML
#' @param index Integer. Hospital index for display
#' @param total Integer. Total hospitals for display
#' @return List with processing results and log entry
process_hospital <- function(hospital, index, total) {
  
  fac <- hospital$FAC
  name <- hospital$name
  
  if (CONFIG$verbose) {
    cat("[", index, "/", total, "] FAC ", fac, ": ", name, "\n", sep = "")
  }
  
  # Initialize result
  result <- list(
    fac = fac,
    name = name,
    action = "not_processed",
    success = FALSE,
    download_attempted = FALSE,
    download_success = FALSE,
    text_extracted = FALSE,
    comparison_result = NA,
    files_saved = FALSE,
    error_message = ""
  )
  
  # Initialize log entry
  log_entry <- data.frame(
    timestamp = get_timestamp(),
    FAC = fac,
    hospital_name = name,
    action = NA_character_,
    pdf_url = NA_character_,
    http_status = NA_integer_,
    pdf_file_size_kb = NA_real_,
    txt_file_size_kb = NA_real_,
    txt_pages_extracted = NA_integer_,
    txt_total_chars = NA_integer_,
    comparison_method = NA_character_,
    comparison_result = NA_character_,
    local_pdf_saved = NA_character_,
    local_txt_saved = NA_character_,
    local_pdf_previous = NA_character_,
    local_txt_previous = NA_character_,
    error_message = NA_character_,
    processing_time_sec = NA_real_,
    notes = NA_character_,
    stringsAsFactors = FALSE
  )
  
  start_time <- Sys.time()
  
  # Check if hospital has required data
  search <- hospital$strategy_search
  
  if (is.null(search) || is.null(search$pdf_url) || search$pdf_url == "") {
    result$action <- "skipped_no_url"
    result$error_message <- "No PDF URL available"
    
    if (CONFIG$verbose) {
      cat("  └─ SKIP: No PDF URL\n\n")
    }
    
    log_entry$action <- "skipped_no_url"
    log_entry$error_message <- result$error_message
    log_entry$processing_time_sec <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    
    return(list(result = result, log_entry = log_entry))
  }
  
  if (!isTRUE(search$pdf_downloaded) && search$pdf_downloaded != "yes") {
    result$action <- "skipped_not_downloaded"
    result$error_message <- "pdf_downloaded flag is FALSE"
    
    if (CONFIG$verbose) {
      cat("  └─ SKIP: pdf_downloaded = FALSE\n\n")
    }
    
    log_entry$action <- "skipped_not_downloaded"
    log_entry$error_message <- result$error_message
    log_entry$processing_time_sec <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    
    return(list(result = result, log_entry = log_entry))
  }
  
  pdf_url <- search$pdf_url
  
  # Check content type
  content_type <- "pdf"  # Default
  if (!is.null(search$content_type) && search$content_type != "") {
    content_type <- search$content_type
  }
  
  # Get folder path and create if needed
  folder_path <- get_hospital_folder(hospital)
  dir.create(folder_path, showWarnings = FALSE, recursive = TRUE)
  
  # Generate filenames
  pdf_filename <- generate_pdf_filename(fac)
  txt_filename <- pdf_to_txt_filename(pdf_filename)
  
  temp_pdf_path <- file.path(folder_path, paste0("temp_", pdf_filename))
  temp_txt_path <- file.path(folder_path, paste0("temp_", txt_filename))
  
  final_pdf_path <- file.path(folder_path, pdf_filename)
  final_txt_path <- file.path(folder_path, txt_filename)
  
  # ROUTE BASED ON CONTENT TYPE
  if (content_type == "html") {
    # HTML-ONLY STRATEGIC PLAN - Extract text directly, no PDF
    if (CONFIG$verbose) {
      cat("  └─ HTML content detected... ", sep = "")
    }
    
    result$download_attempted <- TRUE
    html_extraction <- extract_html_to_text(pdf_url, temp_txt_path)
    
    log_entry$pdf_url <- pdf_url
    log_entry$http_status <- 200  # Assume success if no error
    
    if (!html_extraction$success) {
      result$action <- "html_extraction_failed"
      result$error_message <- html_extraction$error_message
      
      if (CONFIG$verbose) {
        cat("FAILED (", html_extraction$error_message, ")\n\n", sep = "")
      }
      
      log_entry$action <- "html_extraction_failed"
      log_entry$error_message <- result$error_message
      log_entry$processing_time_sec <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      
      if (file.exists(temp_txt_path)) file.remove(temp_txt_path)
      return(list(result = result, log_entry = log_entry))
    }
    
    # HTML extraction succeeded
    result$text_extracted <- TRUE
    result$download_success <- TRUE  # Mark as successful "download"
    
    log_entry$txt_pages_extracted <- 1
    log_entry$txt_total_chars <- html_extraction$total_chars
    log_entry$txt_file_size_kb <- round(file.size(temp_txt_path) / 1024, 2)
    log_entry$pdf_file_size_kb <- 0  # No PDF for HTML
    
    if (CONFIG$verbose) {
      cat(format(html_extraction$total_chars, big.mark = ","), " chars [OK]\n", sep = "")
    }
    
    # COMPARISON LOGIC for HTML (same as PDF below)
    if (CONFIG$baseline_mode) {
      result$action <- "baseline_saved"
      result$success <- TRUE
      result$files_saved <- TRUE
      result$comparison_result <- "first_download"
      
      file.rename(temp_txt_path, final_txt_path)
      
      if (CONFIG$verbose) {
        cat("  └─ BASELINE MODE: Saved as ", txt_filename, "\n\n", sep = "")
      }
      
      log_entry$action <- "baseline_saved"
      log_entry$comparison_method <- "none"
      log_entry$comparison_result <- "first_download"
      log_entry$local_pdf_saved <- ""  # No PDF
      log_entry$local_txt_saved <- final_txt_path
      log_entry$notes <- "Baseline mode - HTML extraction - no PDF"
      
    } else {
      # Change detection mode
      if (CONFIG$verbose) {
        cat("  └─ Comparing... ", sep = "")
      }
      
      prev_txt <- get_most_recent_txt(folder_path)
      
      if (is.null(prev_txt)) {
        # First download
        result$action <- "first_download"
        result$success <- TRUE
        result$files_saved <- TRUE
        result$comparison_result <- "first_download"
        
        file.rename(temp_txt_path, final_txt_path)
        
        if (CONFIG$verbose) {
          cat("FIRST DOWNLOAD - Saved\n\n")
        }
        
        log_entry$action <- "first_download"
        log_entry$comparison_method <- "none"
        log_entry$comparison_result <- "first_download"
        log_entry$local_pdf_saved <- ""
        log_entry$local_txt_saved <- final_txt_path
        
      } else {
        # Compare with previous
        comparison <- compare_text_files(temp_txt_path, prev_txt)
        log_entry$comparison_method <- "text"
        log_entry$local_txt_previous <- prev_txt
        
        if (comparison$identical) {
          result$action <- "no_change"
          result$success <- TRUE
          result$files_saved <- FALSE
          result$comparison_result <- "identical"
          
          if (CONFIG$verbose) {
            cat("IDENTICAL - No change detected\n\n")
          }
          
          log_entry$action <- "no_change"
          log_entry$comparison_result <- "identical"
          log_entry$notes <- comparison$details
          
          if (CONFIG$delete_unchanged_files) {
            file.remove(temp_txt_path)
          }
          
        } else {
          result$action <- "change_detected"
          result$success <- TRUE
          result$files_saved <- TRUE
          result$comparison_result <- "changed"
          
          file.rename(temp_txt_path, final_txt_path)
          
          if (CONFIG$verbose) {
            cat("CHANGED - Saved as ", txt_filename, "\n\n", sep = "")
          }
          
          log_entry$action <- "change_detected"
          log_entry$comparison_result <- "changed"
          log_entry$local_pdf_saved <- ""
          log_entry$local_txt_saved <- final_txt_path
          log_entry$notes <- comparison$details
        }
      }
    }
    
    log_entry$processing_time_sec <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    
    # Delay before next request
    if (index < total) {
      Sys.sleep(CONFIG$delay_between_requests)
    }
    
    return(list(result = result, log_entry = log_entry))
  }
  
  # CONTINUE WITH PDF PROCESSING (existing code)
  # Download PDF
  if (CONFIG$verbose) {
    cat("  └─ Downloading... ", sep = "")
  }
  
  result$download_attempted <- TRUE
  download_result <- download_pdf_with_retry(pdf_url, temp_pdf_path)
  
  log_entry$pdf_url <- pdf_url
  log_entry$http_status <- download_result$http_code
  
  if (!download_result$success) {
    result$action <- "download_failed"
    result$error_message <- download_result$error_message
    
    if (CONFIG$verbose) {
      cat("FAILED (", download_result$error_message, ")\n\n", sep = "")
    }
    
    log_entry$action <- "download_failed"
    log_entry$error_message <- result$error_message
    log_entry$processing_time_sec <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    
    # Clean up temp file if exists
    if (file.exists(temp_pdf_path)) {
      file.remove(temp_pdf_path)
    }
    
    return(list(result = result, log_entry = log_entry))
  }
  
  result$download_success <- TRUE
  log_entry$pdf_file_size_kb <- download_result$file_size_kb
  
  if (CONFIG$verbose) {
    cat(download_result$file_size_kb, " KB [OK]\n", sep = "")
  }
  
  # Validate PDF
  validation <- validate_pdf(temp_pdf_path)
  
  if (!validation$valid) {
    result$action <- "validation_failed"
    result$error_message <- validation$error_message
    
    if (CONFIG$verbose) {
      cat("  └─ Validation FAILED: ", validation$error_message, "\n\n", sep = "")
    }
    
    log_entry$action <- "validation_failed"
    log_entry$error_message <- result$error_message
    log_entry$processing_time_sec <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    
    file.remove(temp_pdf_path)
    return(list(result = result, log_entry = log_entry))
  }
  
  # Extract text
  if (CONFIG$verbose) {
    cat("  └─ Extracting text... ", sep = "")
  }
  
  extraction <- extract_text_from_pdf(temp_pdf_path)
  
  if (!extraction$success) {
    # Text extraction failed - use binary comparison if allowed
    if (CONFIG$fallback_to_binary && !CONFIG$baseline_mode) {
      if (CONFIG$verbose) {
        cat("FAILED\n")
        cat("  └─ Falling back to binary comparison... ", sep = "")
      }
      
      prev_pdf <- get_most_recent_pdf(folder_path)
      
      if (is.null(prev_pdf)) {
        # First download, no comparison possible, save it
        result$action <- "first_download"
        result$success <- TRUE
        result$files_saved <- TRUE
        result$comparison_result <- "first_download"
        
        file.rename(temp_pdf_path, final_pdf_path)
        
        if (CONFIG$verbose) {
          cat("FIRST DOWNLOAD - Saved (text extraction failed)\n\n")
        }
        
        log_entry$action <- "first_download"
        log_entry$comparison_method <- "none"
        log_entry$comparison_result <- "first_download"
        log_entry$local_pdf_saved <- final_pdf_path
        log_entry$error_message <- extraction$error_message
        log_entry$notes <- "Text extraction failed but PDF saved as first download"
        
      } else {
        # Compare binary
        comparison <- compare_pdfs_binary(temp_pdf_path, prev_pdf)
        log_entry$comparison_method <- "binary"
        log_entry$local_pdf_previous <- prev_pdf
        
        if (comparison$identical) {
          result$action <- "no_change"
          result$success <- TRUE
          result$files_saved <- FALSE
          result$comparison_result <- "identical"
          
          if (CONFIG$verbose) {
            cat("IDENTICAL\n\n")
          }
          
          log_entry$action <- "no_change"
          log_entry$comparison_result <- "identical"
          log_entry$error_message <- extraction$error_message
          log_entry$notes <- paste("Text extraction failed. Binary comparison:", comparison$details)
          
          if (CONFIG$delete_unchanged_files) {
            file.remove(temp_pdf_path)
          }
          
        } else {
          result$action <- "change_detected"
          result$success <- TRUE
          result$files_saved <- TRUE
          result$comparison_result <- "changed"
          
          file.rename(temp_pdf_path, final_pdf_path)
          
          if (CONFIG$verbose) {
            cat("CHANGED - Saved\n\n")
          }
          
          log_entry$action <- "change_detected"
          log_entry$comparison_result <- "changed"
          log_entry$local_pdf_saved <- final_pdf_path
          log_entry$error_message <- extraction$error_message
          log_entry$notes <- paste("Text extraction failed. Binary comparison:", comparison$details)
        }
      }
      
      log_entry$processing_time_sec <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      
      if (index < total) {
        Sys.sleep(CONFIG$delay_between_requests)
      }
      
      return(list(result = result, log_entry = log_entry))
      
    } else {
      # Can't compare without text, mark as failed
      result$action <- "text_extraction_failed"
      result$error_message <- extraction$error_message
      
      if (CONFIG$verbose) {
        cat("FAILED (", extraction$error_message, ")\n\n", sep = "")
      }
      
      log_entry$action <- "text_extraction_failed"
      log_entry$error_message <- result$error_message
      log_entry$processing_time_sec <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      
      file.remove(temp_pdf_path)
      return(list(result = result, log_entry = log_entry))
    }
  }
  
  # Text extraction successful
  result$text_extracted <- TRUE
  
  log_entry$txt_pages_extracted <- extraction$pages
  log_entry$txt_total_chars <- extraction$total_chars
  
  if (CONFIG$verbose) {
    cat(extraction$pages, " pages, ", format(extraction$total_chars, big.mark = ","), " chars [OK]\n", sep = "")
  }
  
  # Save text file
  save_success <- save_text_file(extraction$text, temp_txt_path)
  
  if (save_success) {
    log_entry$txt_file_size_kb <- round(file.size(temp_txt_path) / 1024, 2)
  }
  
  # COMPARISON LOGIC
  if (CONFIG$baseline_mode) {
    # BASELINE MODE: Save everything, no comparison
    result$action <- "baseline_saved"
    result$success <- TRUE
    result$files_saved <- TRUE
    result$comparison_result <- "first_download"
    
    # Move files to final location
    file.rename(temp_pdf_path, final_pdf_path)
    if (save_success) {
      file.rename(temp_txt_path, final_txt_path)
    }
    
    if (CONFIG$verbose) {
      cat("  └─ BASELINE MODE: Saved as ", pdf_filename, "\n\n", sep = "")
    }
    
    log_entry$action <- "baseline_saved"
    log_entry$comparison_method <- "none"
    log_entry$comparison_result <- "first_download"
    log_entry$local_pdf_saved <- final_pdf_path
    log_entry$local_txt_saved <- if (save_success) final_txt_path else NA_character_
    log_entry$notes <- "Baseline mode - no comparison performed"
    
  } else {
    # CHANGE DETECTION MODE: Compare with previous version
    if (CONFIG$verbose) {
      cat("  └─ Comparing... ", sep = "")
    }
    
    prev_txt <- get_most_recent_txt(folder_path)
    prev_pdf <- get_most_recent_pdf(folder_path)
    
    if (is.null(prev_txt) || !save_success) {
      # First download or text save failed - use binary comparison
      if (is.null(prev_pdf)) {
        # Truly first download
        result$action <- "first_download"
        result$success <- TRUE
        result$files_saved <- TRUE
        result$comparison_result <- "first_download"
        
        file.rename(temp_pdf_path, final_pdf_path)
        if (save_success) {
          file.rename(temp_txt_path, final_txt_path)
        }
        
        if (CONFIG$verbose) {
          cat("FIRST DOWNLOAD - Saved\n\n")
        }
        
        log_entry$action <- "first_download"
        log_entry$comparison_method <- "none"
        log_entry$comparison_result <- "first_download"
        log_entry$local_pdf_saved <- final_pdf_path
        log_entry$local_txt_saved <- if (save_success) final_txt_path else NA_character_
        
      } else {
        # Text comparison not possible, use binary
        comparison <- compare_pdfs_binary(temp_pdf_path, prev_pdf)
        log_entry$comparison_method <- "binary"
        log_entry$local_pdf_previous <- prev_pdf
        
        if (comparison$identical) {
          result$action <- "no_change"
          result$success <- TRUE
          result$files_saved <- FALSE
          result$comparison_result <- "identical"
          
          if (CONFIG$verbose) {
            cat("IDENTICAL (binary) - No change\n\n")
          }
          
          log_entry$action <- "no_change"
          log_entry$comparison_result <- "identical"
          log_entry$notes <- paste("Binary comparison:", comparison$details)
          
          if (CONFIG$delete_unchanged_files) {
            file.remove(temp_pdf_path)
            if (save_success && file.exists(temp_txt_path)) {
              file.remove(temp_txt_path)
            }
          }
          
        } else {
          result$action <- "change_detected"
          result$success <- TRUE
          result$files_saved <- TRUE
          result$comparison_result <- "changed"
          
          file.rename(temp_pdf_path, final_pdf_path)
          if (save_success) {
            file.rename(temp_txt_path, final_txt_path)
          }
          
          if (CONFIG$verbose) {
            cat("CHANGED (binary) - Saved\n\n")
          }
          
          log_entry$action <- "change_detected"
          log_entry$comparison_result <- "changed"
          log_entry$local_pdf_saved <- final_pdf_path
          log_entry$local_txt_saved <- if (save_success) final_txt_path else NA_character_
          log_entry$notes <- paste("Binary comparison:", comparison$details)
        }
      }
      
    } else {
      # Text comparison available
      comparison <- compare_text_files(temp_txt_path, prev_txt)
      log_entry$comparison_method <- "text"
      log_entry$local_txt_previous <- prev_txt
      log_entry$local_pdf_previous <- prev_pdf
      
      if (comparison$identical) {
        result$action <- "no_change"
        result$success <- TRUE
        result$files_saved <- FALSE
        result$comparison_result <- "identical"
        
        if (CONFIG$verbose) {
          cat("IDENTICAL - No change detected\n\n")
        }
        
        log_entry$action <- "no_change"
        log_entry$comparison_result <- "identical"
        log_entry$notes <- comparison$details
        
        # Delete temp files
        if (CONFIG$delete_unchanged_files) {
          file.remove(temp_pdf_path)
          file.remove(temp_txt_path)
        }
        
      } else {
        result$action <- "change_detected"
        result$success <- TRUE
        result$files_saved <- TRUE
        result$comparison_result <- "changed"
        
        # Save both PDF and TXT
        file.rename(temp_pdf_path, final_pdf_path)
        file.rename(temp_txt_path, final_txt_path)
        
        if (CONFIG$verbose) {
          cat("CHANGED - Saved as ", pdf_filename, "\n\n", sep = "")
        }
        
        log_entry$action <- "change_detected"
        log_entry$comparison_result <- "changed"
        log_entry$local_pdf_saved <- final_pdf_path
        log_entry$local_txt_saved <- final_txt_path
        log_entry$notes <- comparison$details
      }
    }
  }
  
  log_entry$processing_time_sec <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  
  # Delay before next request
  if (index < total) {
    Sys.sleep(CONFIG$delay_between_requests)
  }
  
  return(list(result = result, log_entry = log_entry))
}

# MAIN EXECUTION ----

#' Main processing function
main <- function() {
  
  # Initialize
  initialize_environment()
  
  # Read hospitals
  hospitals <- read_yaml_hospitals()
  
  # Process all hospitals - let process_hospital() handle skips and log everything
  hospitals_to_process <- hospitals
  
  cat("[TOTAL] Processing all hospitals:", length(hospitals_to_process), "\n")
  
  # Apply test mode filter
  if (CONFIG$test_mode) {
    hospitals_to_process <- hospitals_to_process[sapply(hospitals_to_process, function(h) {
      h$FAC %in% CONFIG$test_fac_codes
    })]
    cat("[TEST] Filtered to test hospitals:", length(hospitals_to_process), "\n")
  }
  
  if (length(hospitals_to_process) == 0) {
    cat("\n[COMPLETE] No hospitals to process\n")
    return(invisible(NULL))
  }
  
  cat("\n[START] Processing", length(hospitals_to_process), "hospitals...\n\n")
  
  # Initialize logging
  log_data <- tibble(
    timestamp = character(),
    FAC = character(),
    hospital_name = character(),
    action = character(),
    pdf_url = character(),
    http_status = integer(),
    pdf_file_size_kb = numeric(),
    txt_file_size_kb = numeric(),
    txt_pages_extracted = integer(),
    txt_total_chars = integer(),
    comparison_method = character(),
    comparison_result = character(),
    local_pdf_saved = character(),
    local_txt_saved = character(),
    local_pdf_previous = character(),
    local_txt_previous = character(),
    error_message = character(),
    processing_time_sec = numeric(),
    notes = character()
  )
  
  # Initialize summary
  summary <- list(
    total_hospitals = length(hospitals_to_process),
    skipped_no_url = 0,              # NEW: Track hospitals without URLs
    skipped_not_downloaded = 0,      # NEW: Track hospitals with pdf_downloaded=FALSE
    download_attempted = 0,
    download_success = 0,
    download_failed = 0,
    text_extracted = 0,
    text_failed = 0,
    html_extracted = 0,
    html_failed = 0,
    comparison_identical = 0,
    comparison_changed = 0,
    first_downloads = 0,
    baseline_saved = 0,
    files_saved = 0,
    api_cost_saved = 0,
    api_cost_incurred = 0,
    start_time = Sys.time()
  )
  
  # Process each hospital
  for (i in seq_along(hospitals_to_process)) {
    hospital <- hospitals_to_process[[i]]
    
    hospital_output <- process_hospital(hospital, i, length(hospitals_to_process))
    result <- hospital_output$result
    
    # Collect log
    log_data <- bind_rows(log_data, hospital_output$log_entry)
    
    # Update summary with robust NA checking
    # NEW: Track skipped hospitals
    if (result$action == "skipped_no_url") summary$skipped_no_url <- summary$skipped_no_url + 1
    if (result$action == "skipped_not_downloaded") summary$skipped_not_downloaded <- summary$skipped_not_downloaded + 1
    
    if (result$download_attempted) summary$download_attempted <- summary$download_attempted + 1
    if (result$download_success) summary$download_success <- summary$download_success + 1
    if (result$action == "download_failed") summary$download_failed <- summary$download_failed + 1
    if (result$text_extracted) summary$text_extracted <- summary$text_extracted + 1
    if (result$action == "text_extraction_failed") summary$text_failed <- summary$text_failed + 1
    if (result$action == "html_extraction_failed") summary$html_failed <- summary$html_failed + 1
    if (grepl("html", log_data$notes[nrow(log_data)], ignore.case = TRUE) && result$text_extracted) {
      summary$html_extracted <- summary$html_extracted + 1
    }
    
    # ROBUST NA CHECKING: Only check comparison_result if it's not NA
    if (!is.na(result$comparison_result)) {
      if (result$comparison_result == "identical") summary$comparison_identical <- summary$comparison_identical + 1
      if (result$comparison_result == "changed") summary$comparison_changed <- summary$comparison_changed + 1
      if (result$comparison_result == "first_download") summary$first_downloads <- summary$first_downloads + 1
    }
    
    if (result$action == "baseline_saved") summary$baseline_saved <- summary$baseline_saved + 1
    if (result$files_saved) summary$files_saved <- summary$files_saved + 1
  }
  
  summary$end_time <- Sys.time()
  summary$elapsed_time <- difftime(summary$end_time, summary$start_time, units = "secs")
  
  # Calculate API cost savings (only in change detection mode)
  if (!CONFIG$baseline_mode) {
    summary$api_cost_saved <- summary$comparison_identical * 0.08
    summary$api_cost_incurred <- (summary$comparison_changed + summary$first_downloads) * 0.08
  }
  
  # Save logs
  save_logs(log_data, summary)
  
  # Print summary
  print_summary(summary)
  
  return(invisible(list(
    log_data = log_data,
    summary = summary
  )))
}

# LOGGING FUNCTIONS ----

#' Save log files
save_logs <- function(log_data, summary) {
  
  timestamp <- get_file_timestamp()
  
  # Save detailed CSV
  csv_file <- file.path(CONFIG$log_path, paste0("download_log_", timestamp, ".csv"))
  write.csv(log_data, csv_file, row.names = FALSE)
  cat("\n[LOG] Detailed log saved:", csv_file, "\n")
  
  # Save summary TXT
  txt_file <- file.path(CONFIG$log_path, paste0("download_summary_", timestamp, ".txt"))
  
  sink(txt_file)
  cat(strrep("=", 75), "\n")
  cat("STRATEGIC PDF DOWNLOAD & COMPARISON SUMMARY\n")
  if (CONFIG$baseline_mode) {
    cat("BASELINE MODE: Fresh Download (No Comparison)\n")
  } else {
    cat("CHANGE DETECTION MODE: Text-Based Comparison\n")
  }
  cat("Run Date:", format(summary$start_time, "%Y-%m-%d %H:%M:%S"), "\n")
  cat(strrep("=", 75), "\n\n")
  
  cat("Configuration:\n")
  cat("  Base Directory:", CONFIG$base_dir, "\n")
  cat("  Mode:", if (CONFIG$baseline_mode) "Baseline" else "Change Detection", "\n")
  cat("  Comparison Method:", CONFIG$comparison_method, "\n")
  cat("  Test Mode:", CONFIG$test_mode, "\n\n")
  
  cat("Results:\n")
  cat("  Total Hospitals:", summary$total_hospitals, "\n")
  
  # NEW: Show skipped hospitals
  total_skipped <- summary$skipped_no_url + summary$skipped_not_downloaded
  if (total_skipped > 0) {
    cat("  Skipped Hospitals:", total_skipped, "\n")
    if (summary$skipped_no_url > 0) {
      cat("    - No URL available:", summary$skipped_no_url, "\n")
    }
    if (summary$skipped_not_downloaded > 0) {
      cat("    - pdf_downloaded flag not set:", summary$skipped_not_downloaded, "\n")
    }
    cat("\n")
  }
  
  cat("  Download Attempts:", summary$download_attempted, "\n")
  cat("    - Successful:", summary$download_success, "\n")
  cat("    - Failed:", summary$download_failed, "\n\n")
  
  cat("  Text Extraction:\n")
  cat("    - Successful:", summary$text_extracted, "\n")
  cat("    - Failed:", summary$text_failed, "\n")
  if (summary$html_extracted > 0) {
    cat("    - From HTML pages:", summary$html_extracted, "\n")
  }
  if (summary$html_failed > 0) {
    cat("    - HTML extraction failed:", summary$html_failed, "\n")
  }
  cat("\n")
  
  if (CONFIG$baseline_mode) {
    cat("  Baseline Results:\n")
    cat("    - Files Saved:", summary$baseline_saved, "\n\n")
  } else {
    cat("  Comparison Results:\n")
    cat("    - Identical (No Change):", summary$comparison_identical, "\n")
    cat("    - Changed:", summary$comparison_changed, "\n")
    cat("    - First Downloads:", summary$first_downloads, "\n")
    cat("    - Files Saved:", summary$files_saved, "\n\n")
    
    cat("  API Cost Analysis:\n")
    cat("    - Cost Saved (unchanged):", sprintf("$%.2f", summary$api_cost_saved), 
        "(", summary$comparison_identical, " × $0.08)\n")
    cat("    - Future Cost (processing):", sprintf("$%.2f", summary$api_cost_incurred),
        "(", summary$comparison_changed + summary$first_downloads, " × $0.08)\n")
    cat("    - Net Benefit:", sprintf("$%.2f", summary$api_cost_saved - summary$api_cost_incurred), "\n\n")
  }
  
  cat("Processing Time:", round(summary$elapsed_time, 1), "seconds\n")
  cat("Average per Hospital:", round(summary$elapsed_time / summary$total_hospitals, 2), "seconds\n")
  
  cat(strrep("=", 75), "\n")
  sink()
  
  cat("[LOG] Summary report saved:", txt_file, "\n")
  
  # Save changes CSV (only in change detection mode)
  if (!CONFIG$baseline_mode) {
    changes <- log_data %>%
      filter(action %in% c("change_detected", "first_download", "baseline_saved"))
    
    if (nrow(changes) > 0) {
      changes_file <- file.path(CONFIG$log_path, paste0("changes_detected_", timestamp, ".csv"))
      write.csv(changes, changes_file, row.names = FALSE)
      cat("[LOG] Changes report saved:", changes_file, "\n")
      cat("      (INPUT FOR PHASE 2 API PROCESSING)\n")
    }
  }
}

#' Print summary to console
print_summary <- function(summary) {
  cat("\n")
  cat(strrep("=", 75), "\n")
  cat("PROCESSING COMPLETE\n")
  cat(strrep("=", 75), "\n")
  
  cat("Total Hospitals:", summary$total_hospitals, "\n")
  
  # NEW: Show skipped hospitals
  total_skipped <- summary$skipped_no_url + summary$skipped_not_downloaded
  if (total_skipped > 0) {
    cat("Skipped:", total_skipped, "\n")
  }
  
  cat("Downloads Successful:", summary$download_success, "\n")
  cat("Downloads Failed:", summary$download_failed, "\n")
  cat("Text Extracted:", summary$text_extracted, "\n")
  if (summary$html_extracted > 0) {
    cat("  - From HTML pages:", summary$html_extracted, "\n")
  }
  
  if (CONFIG$baseline_mode) {
    cat("\nBaseline Files Saved:", summary$baseline_saved, "\n")
  } else {
    cat("\nComparison Results:\n")
    cat("  - Identical (no change):", summary$comparison_identical, "\n")
    cat("  - Changed:", summary$comparison_changed, "\n")
    cat("  - First downloads:", summary$first_downloads, "\n")
    cat("  - Total files saved:", summary$files_saved, "\n")
    
    if (summary$api_cost_saved > 0) {
      cat("\nAPI Cost Savings: $", sprintf("%.2f", summary$api_cost_saved), 
          " (", summary$comparison_identical, " unchanged hospitals)\n", sep = "")
    }
  }
  
  cat("\nProcessing Time:", round(summary$elapsed_time, 1), "seconds\n")
  
  cat(strrep("=", 75), "\n")
  
  if (!CONFIG$baseline_mode && summary$files_saved > 0) {
    cat("\n✓ ", summary$files_saved, " changed/new strategic plans detected\n", sep = "")
    cat("  Ready for Phase 2 API processing\n")
  } else if (CONFIG$baseline_mode) {
    cat("\n✓ Baseline created successfully\n")
    cat("  Ready for future change detection runs\n")
  }
}

# SCRIPT EXECUTION ----
if (!interactive()) {
  main()
} else {
  cat("\n", strrep("=", 75), "\n")
  cat("STRATEGIC PDF DOWNLOAD & COMPARISON SCRIPT LOADED\n")
  cat(strrep("=", 75), "\n\n")
  cat("Script loaded. Run with: results <- main()\n\n")
  cat("MODES:\n")
  cat("  Baseline Mode (first run):     CONFIG$baseline_mode <- TRUE\n")
  cat("  Change Detection (ongoing):    CONFIG$baseline_mode <- FALSE\n\n")
  cat("OPTIONS:\n")
  cat("  Test mode:                     CONFIG$test_mode <- TRUE/FALSE\n")
  cat("  Test hospitals:                CONFIG$test_fac_codes <- c('592', '593')\n\n")
  cat("CURRENT SETTINGS:\n")
  cat("  Mode:", if (CONFIG$baseline_mode) "Baseline" else "Change Detection", "\n")
  cat("  Test Mode:", CONFIG$test_mode, "\n\n")
}