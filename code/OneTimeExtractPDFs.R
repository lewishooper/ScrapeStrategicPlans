# ============================================================================
# ONE-TIME TEXT EXTRACTION SCRIPT FOR EXISTING STRATEGIC PLAN PDFs
# ============================================================================
# Purpose: Extract text from all existing strategic plan PDFs
#          Create companion .txt files for future change detection
#          Update YAML with text extraction metadata
# Author: Skip
# Created: 2026-01-19
# Version: 1.1 (Fixed logging issue)
#
# Usage: Run once to process all existing PDFs before deploying the
#        main download_strategic_pdfs.R script
# ============================================================================

# REQUIRED LIBRARIES ----
library(yaml)
library(pdftools)
library(dplyr)
library(stringr)
library(purrr)
library(lubridate)
library(tibble)

# CONFIGURATION ----
CONFIG <- list(
  # Paths
  base_dir = "E:/Hospital_Strategic_Plans",
  yaml_file = "code/Hospital_strategy.yaml",
  pdf_folder = "strategic_plans",
  log_folder = "Outputs/extraction_logs",
  
  # Text extraction
  text_encoding = "UTF-8",
  
  # File validation
  min_text_chars = 50,  # Minimum characters to consider valid extraction
  
  # Processing options
  skip_existing_txt = TRUE,  # Skip if .txt file already exists
  create_backup_before_yaml_update = TRUE,
  verbose = TRUE,
  
  # Testing
  test_mode = FALSE,
  test_fac_codes = c("592", "593", "597")  # Only process these in test mode
)

# INITIALIZATION ----
initialize_extraction <- function() {
  cat("\n")
  cat(strrep("=", 75), "\n")
  cat("ONE-TIME TEXT EXTRACTION FOR EXISTING STRATEGIC PLAN PDFs\n")
  cat(strrep("=", 75), "\n\n")
  
  # Check base directory
  if (!dir.exists(CONFIG$base_dir)) {
    stop("Base directory not found: ", CONFIG$base_dir)
  }
  
  # Create log directory
  log_path <- file.path(CONFIG$base_dir, CONFIG$log_folder)
  dir.create(log_path, showWarnings = FALSE, recursive = TRUE)
  
  cat("[CONFIG] Base directory:", CONFIG$base_dir, "\n")
  cat("[CONFIG] YAML file:", CONFIG$yaml_file, "\n")
  cat("[CONFIG] Log directory:", log_path, "\n")
  
  if (CONFIG$test_mode) {
    cat("\n*** TEST MODE: Processing only", length(CONFIG$test_fac_codes), "hospitals ***\n")
  } else {
    cat("\n*** FULL RUN: Processing all hospitals with downloaded PDFs ***\n")
  }
  
  cat("\n")
  
  # Store paths globally
  CONFIG$log_path <<- log_path
  CONFIG$pdf_path <<- file.path(CONFIG$base_dir, CONFIG$pdf_folder)
}

# UTILITY FUNCTIONS ----

#' Get current timestamp in standard format
get_timestamp <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
}

#' Get timestamp for filenames
get_file_timestamp <- function() {
  format(Sys.time(), "%Y%m%d_%H%M%S")
}

#' Sanitize filename for cross-platform compatibility
sanitize_filename <- function(text) {
  text %>%
    str_replace_all("[^a-zA-Z0-9_-]", "_") %>%
    str_replace_all("_{2,}", "_") %>%
    str_sub(1, 100)  # Limit length
}

# YAML OPERATIONS ----

#' Read hospitals from YAML file
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

#' Write updated YAML file with backup
write_yaml_hospitals <- function(hospitals) {
  yaml_path <- file.path(CONFIG$base_dir, CONFIG$yaml_file)
  
  # Create backup if requested
  if (CONFIG$create_backup_before_yaml_update) {
    backup_path <- paste0(yaml_path, ".backup_", get_file_timestamp())
    file.copy(yaml_path, backup_path)
    cat("[BACKUP] Created backup:", basename(backup_path), "\n")
  }
  
  # Write updated YAML
  write_yaml(hospitals, yaml_path)
  cat("[SAVE] Updated YAML file:", CONFIG$yaml_file, "\n\n")
}

# FILE SYSTEM FUNCTIONS ----

#' Get the path to a hospital's folder
get_hospital_folder <- function(hospital) {
  local_folder <- hospital$strategy_search$local_folder
  if (is.null(local_folder) || local_folder == "") {
    # Fallback: create folder name from FAC and name
    fac <- hospital$FAC
    name <- sanitize_filename(hospital$name)
    local_folder <- paste0(fac, "_", name)
  }
  
  folder_path <- file.path(CONFIG$pdf_path, local_folder)
  return(folder_path)
}

#' Get all PDF files for a hospital (sorted by date, newest first)
get_hospital_pdfs <- function(hospital) {
  folder_path <- get_hospital_folder(hospital)
  
  if (!dir.exists(folder_path)) {
    return(character(0))
  }
  
  # Find all PDF files
  pdf_files <- list.files(folder_path, pattern = "\\.pdf$", full.names = TRUE, ignore.case = TRUE)
  
  if (length(pdf_files) == 0) {
    return(character(0))
  }
  
  # Sort by modification time (newest first)
  file_info <- file.info(pdf_files)
  pdf_files <- pdf_files[order(file_info$mtime, decreasing = TRUE)]
  
  return(pdf_files)
}

#' Generate text filename from PDF filename
pdf_to_txt_filename <- function(pdf_path) {
  str_replace(pdf_path, "\\.pdf$", ".txt")
}

# TEXT EXTRACTION FUNCTIONS ----

#' Extract text from PDF file
#' @param pdf_path Character. Full path to PDF file
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
    # Extract text using pdftools
    extracted_text <- pdf_text(pdf_path)
    
    # Validate extraction
    if (is.null(extracted_text) || length(extracted_text) == 0) {
      result$error_message <- "No text extracted (empty result)"
      return(result)
    }
    
    # Calculate metadata
    pages <- length(extracted_text)
    total_chars <- sum(nchar(extracted_text))
    
    # Check minimum character threshold
    if (total_chars < CONFIG$min_text_chars) {
      result$success <- TRUE  # Still successful, just minimal text
      result$text <- extracted_text
      result$pages <- pages
      result$total_chars <- total_chars
      result$error_message <- paste0("Warning: Only ", total_chars, " characters extracted (possible image-only PDF)")
      return(result)
    }
    
    # Success
    result$success <- TRUE
    result$text <- extracted_text
    result$pages <- pages
    result$total_chars <- total_chars
    
  }, error = function(e) {
    result$error_message <<- paste("Extraction failed:", e$message)
  })
  
  return(result)
}

#' Save extracted text to file
#' @param text Character vector. Text extracted from PDF (one element per page)
#' @param txt_path Character. Full path for output text file
#' @return Logical. TRUE if successful
save_text_file <- function(text, txt_path) {
  tryCatch({
    writeLines(text, txt_path, useBytes = FALSE)
    return(TRUE)
  }, error = function(e) {
    cat("  [ERROR] Failed to save text file:", e$message, "\n")
    return(FALSE)
  })
}

# CORE PROCESSING ----

#' Process a single PDF: extract text and save
#' @param pdf_path Character. Full path to PDF file
#' @param hospital List. Hospital data for logging
#' @param fac Character. FAC code for logging
#' @param name Character. Hospital name for logging
#' @return List with processing results
process_single_pdf <- function(pdf_path, hospital, fac, name) {
  
  result <- list(
    success = FALSE,
    txt_path = "",
    pages = 0,
    total_chars = 0,
    error_message = "",
    skipped = FALSE
  )
  
  # Generate text file path
  txt_path <- pdf_to_txt_filename(pdf_path)
  
  # Check if text file already exists
  if (CONFIG$skip_existing_txt && file.exists(txt_path)) {
    result$skipped <- TRUE
    result$txt_path <- txt_path
    
    # Try to read existing file for metadata
    tryCatch({
      existing_text <- readLines(txt_path, warn = FALSE)
      result$pages <- length(existing_text)
      result$total_chars <- sum(nchar(existing_text))
    }, error = function(e) {
      # Ignore errors reading existing file
    })
    
    return(result)
  }
  
  # Extract text
  extraction <- extract_text_from_pdf(pdf_path)
  
  if (!extraction$success) {
    result$error_message <- extraction$error_message
    return(result)
  }
  
  # Save text file
  save_success <- save_text_file(extraction$text, txt_path)
  
  if (!save_success) {
    result$error_message <- "Failed to save text file"
    return(result)
  }
  
  # Success
  result$success <- TRUE
  result$txt_path <- txt_path
  result$pages <- extraction$pages
  result$total_chars <- extraction$total_chars
  result$error_message <- extraction$error_message  # May contain warnings
  
  return(result)
}

#' Process a single hospital: find and extract all PDFs
#' @param hospital List. Hospital data from YAML
#' @return List with hospital processing results and log entries
process_hospital <- function(hospital) {
  
  fac <- hospital$FAC
  name <- hospital$name
  
  # Initialize local log collection
  local_log_entries <- tibble(
    timestamp = character(),
    FAC = character(),
    hospital_name = character(),
    pdf_file = character(),
    pdf_path = character(),
    txt_file = character(),
    txt_path = character(),
    action = character(),
    pages = integer(),
    total_chars = integer(),
    error_message = character()
  )
  
  if (CONFIG$verbose) {
    cat("[", fac, "] ", name, "\n", sep = "")
  }
  
  result <- list(
    fac = fac,
    name = name,
    pdfs_found = 0,
    pdfs_processed = 0,
    pdfs_skipped = 0,
    pdfs_failed = 0,
    total_pages = 0,
    total_chars = 0,
    errors = character(0)
  )
  
  # Get all PDFs for this hospital
  pdf_files <- get_hospital_pdfs(hospital)
  
  if (length(pdf_files) == 0) {
    if (CONFIG$verbose) {
      cat("  └─ No PDFs found\n\n")
    }
    return(list(
      result = result,
      log_entries = local_log_entries
    ))
  }
  
  result$pdfs_found <- length(pdf_files)
  
  if (CONFIG$verbose) {
    cat("  └─ Found ", length(pdf_files), " PDF(s)\n", sep = "")
  }
  
  # Process each PDF
  for (pdf_path in pdf_files) {
    pdf_name <- basename(pdf_path)
    
    if (CONFIG$verbose) {
      cat("     Processing: ", pdf_name, "... ", sep = "")
    }
    
    # Process this PDF
    pdf_result <- process_single_pdf(pdf_path, hospital, fac, name)
    
    # Update counters
    if (pdf_result$skipped) {
      result$pdfs_skipped <- result$pdfs_skipped + 1
      if (CONFIG$verbose) {
        cat("SKIPPED (txt exists)\n")
      }
    } else if (pdf_result$success) {
      result$pdfs_processed <- result$pdfs_processed + 1
      result$total_pages <- result$total_pages + pdf_result$pages
      result$total_chars <- result$total_chars + pdf_result$total_chars
      
      if (CONFIG$verbose) {
        cat("OK (", pdf_result$pages, " pages, ", 
            format(pdf_result$total_chars, big.mark = ","), " chars)\n", sep = "")
        if (pdf_result$error_message != "") {
          cat("     ", pdf_result$error_message, "\n", sep = "")
        }
      }
    } else {
      result$pdfs_failed <- result$pdfs_failed + 1
      result$errors <- c(result$errors, pdf_result$error_message)
      
      if (CONFIG$verbose) {
        cat("FAILED\n")
        cat("     Error: ", pdf_result$error_message, "\n", sep = "")
      }
    }
    
    # Log this PDF
    log_entry <- data.frame(
      timestamp = get_timestamp(),
      FAC = fac,
      hospital_name = name,
      pdf_file = pdf_name,
      pdf_path = pdf_path,
      txt_file = basename(pdf_result$txt_path),
      txt_path = pdf_result$txt_path,
      action = if (pdf_result$skipped) "skipped" else if (pdf_result$success) "extracted" else "failed",
      pages = pdf_result$pages,
      total_chars = pdf_result$total_chars,
      error_message = pdf_result$error_message,
      stringsAsFactors = FALSE
    )
    
    # Append to local log collection
    local_log_entries <- bind_rows(local_log_entries, log_entry)
  }
  
  if (CONFIG$verbose) {
    cat("\n")
  }
  
  # Return both result and log entries
  return(list(
    result = result,
    log_entries = local_log_entries
  ))
}

#' Main processing function
main <- function() {
  
  # Initialize
  initialize_extraction()
  
  # Read hospitals
  hospitals <- read_yaml_hospitals()
  
  # Filter to hospitals with downloaded PDFs
  hospitals_with_pdfs <- hospitals[sapply(hospitals, function(h) {
    search <- h$strategy_search
    !is.null(search) && 
      !is.null(search$pdf_downloaded) && 
      (search$pdf_downloaded == TRUE || search$pdf_downloaded == "yes")
  })]
  
  cat("[FILTER] Hospitals with pdf_downloaded=TRUE:", length(hospitals_with_pdfs), "\n")
  
  # Apply test mode filter if enabled
  if (CONFIG$test_mode) {
    hospitals_with_pdfs <- hospitals_with_pdfs[sapply(hospitals_with_pdfs, function(h) {
      h$FAC %in% CONFIG$test_fac_codes
    })]
    cat("[TEST] Filtered to test hospitals:", length(hospitals_with_pdfs), "\n")
  }
  
  if (length(hospitals_with_pdfs) == 0) {
    cat("\n[COMPLETE] No hospitals to process.\n")
    return(invisible(NULL))
  }
  
  cat("\n[START] Processing", length(hospitals_with_pdfs), "hospitals...\n\n")
  
  # Initialize logging
  log_data <- list(
    entries = tibble(
      timestamp = character(),
      FAC = character(),
      hospital_name = character(),
      pdf_file = character(),
      pdf_path = character(),
      txt_file = character(),
      txt_path = character(),
      action = character(),
      pages = integer(),
      total_chars = integer(),
      error_message = character()
    )
  )
  
  # Initialize summary statistics
  summary <- list(
    total_hospitals = length(hospitals_with_pdfs),
    hospitals_processed = 0,
    hospitals_skipped = 0,
    hospitals_with_errors = 0,
    total_pdfs_found = 0,
    total_pdfs_processed = 0,
    total_pdfs_skipped = 0,
    total_pdfs_failed = 0,
    total_pages = 0,
    total_chars = 0,
    start_time = Sys.time()
  )
  
  # Process each hospital
  for (i in seq_along(hospitals_with_pdfs)) {
    hospital <- hospitals_with_pdfs[[i]]
    
    if (CONFIG$verbose) {
      cat("[", i, "/", length(hospitals_with_pdfs), "] ", sep = "")
    }
    
    # Get both result and log entries
    hospital_output <- process_hospital(hospital)
    result <- hospital_output$result
    
    # Collect log entries
    log_data$entries <- bind_rows(log_data$entries, hospital_output$log_entries)
    
    # Update summary
    summary$total_pdfs_found <- summary$total_pdfs_found + result$pdfs_found
    summary$total_pdfs_processed <- summary$total_pdfs_processed + result$pdfs_processed
    summary$total_pdfs_skipped <- summary$total_pdfs_skipped + result$pdfs_skipped
    summary$total_pdfs_failed <- summary$total_pdfs_failed + result$pdfs_failed
    summary$total_pages <- summary$total_pages + result$total_pages
    summary$total_chars <- summary$total_chars + result$total_chars
    
    if (result$pdfs_processed > 0) {
      summary$hospitals_processed <- summary$hospitals_processed + 1
    }
    if (result$pdfs_skipped > 0 && result$pdfs_processed == 0) {
      summary$hospitals_skipped <- summary$hospitals_skipped + 1
    }
    if (length(result$errors) > 0) {
      summary$hospitals_with_errors <- summary$hospitals_with_errors + 1
    }
  }
  
  summary$end_time <- Sys.time()
  summary$elapsed_time <- difftime(summary$end_time, summary$start_time, units = "secs")
  
  # Save logs
  save_logs(log_data$entries, summary)
  
  # Print summary
  print_summary(summary)
  
  # Optionally update YAML
  if (summary$total_pdfs_processed > 0) {
    cat("\n[OPTION] YAML file can be updated with text_extracted flags\n")
    cat("         Run update_yaml_with_extraction_flags() if desired\n")
  }
  
  return(invisible(list(
    log_data = log_data$entries,
    summary = summary
  )))
}

# LOGGING FUNCTIONS ----

#' Save log files
save_logs <- function(log_entries, summary) {
  
  timestamp <- get_file_timestamp()
  
  # Save detailed CSV log
  csv_file <- file.path(CONFIG$log_path, paste0("extraction_log_", timestamp, ".csv"))
  write.csv(log_entries, csv_file, row.names = FALSE)
  cat("\n[LOG] Detailed log saved:", csv_file, "\n")
  
  # Save summary report
  txt_file <- file.path(CONFIG$log_path, paste0("extraction_summary_", timestamp, ".txt"))
  
  sink(txt_file)
  cat(strrep("=", 75), "\n")
  cat("ONE-TIME TEXT EXTRACTION SUMMARY\n")
  cat("Run Date:", format(summary$start_time, "%Y-%m-%d %H:%M:%S"), "\n")
  cat(strrep("=", 75), "\n\n")
  
  cat("Configuration:\n")
  cat("  Base Directory:", CONFIG$base_dir, "\n")
  cat("  PDF Folder:", CONFIG$pdf_folder, "\n")
  cat("  Skip Existing TXT:", CONFIG$skip_existing_txt, "\n")
  cat("  Test Mode:", CONFIG$test_mode, "\n\n")
  
  cat("Results:\n")
  cat("  Hospitals Processed:", summary$total_hospitals, "\n")
  cat("    - With Extractions:", summary$hospitals_processed, "\n")
  cat("    - Fully Skipped:", summary$hospitals_skipped, "\n")
  cat("    - With Errors:", summary$hospitals_with_errors, "\n\n")
  
  cat("  PDFs Found:", summary$total_pdfs_found, "\n")
  cat("    - Text Extracted:", summary$total_pdfs_processed, "\n")
  cat("    - Skipped (txt exists):", summary$total_pdfs_skipped, "\n")
  cat("    - Failed:", summary$total_pdfs_failed, "\n\n")
  
  cat("  Text Extraction Stats:\n")
  cat("    - Total Pages:", format(summary$total_pages, big.mark = ","), "\n")
  cat("    - Total Characters:", format(summary$total_chars, big.mark = ","), "\n")
  cat("    - Avg Pages per PDF:", 
      if (summary$total_pdfs_processed > 0) 
        round(summary$total_pages / summary$total_pdfs_processed, 1) 
      else 0, "\n")
  cat("    - Avg Chars per PDF:", 
      if (summary$total_pdfs_processed > 0) 
        format(round(summary$total_chars / summary$total_pdfs_processed, 0), big.mark = ",") 
      else 0, "\n\n")
  
  cat("Processing Time:", round(summary$elapsed_time, 1), "seconds\n")
  cat("Avg Time per Hospital:", 
      if (summary$total_hospitals > 0)
        round(summary$elapsed_time / summary$total_hospitals, 2)
      else 0, 
      "seconds\n\n")
  
  if (summary$total_pdfs_failed > 0) {
    cat("ERRORS:\n")
    failed_entries <- log_entries %>% filter(action == "failed")
    for (i in 1:nrow(failed_entries)) {
      cat("  FAC", failed_entries$FAC[i], ":", failed_entries$pdf_file[i], "\n")
      cat("    Error:", failed_entries$error_message[i], "\n")
    }
    cat("\n")
  }
  
  cat(strrep("=", 75), "\n")
  cat("NEXT STEPS:\n")
  cat("  1. Review extraction log for any errors\n")
  cat("  2. Verify .txt files created alongside PDFs\n")
  cat("  3. Run download_strategic_pdfs.R for future updates\n")
  cat(strrep("=", 75), "\n")
  
  sink()
  
  cat("[LOG] Summary report saved:", txt_file, "\n")
}

#' Print summary to console
print_summary <- function(summary) {
  cat("\n")
  cat(strrep("=", 75), "\n")
  cat("EXTRACTION COMPLETE\n")
  cat(strrep("=", 75), "\n")
  
  cat("Hospitals:", summary$total_hospitals, "\n")
  cat("PDFs Found:", summary$total_pdfs_found, "\n")
  cat("Text Extracted:", summary$total_pdfs_processed, "\n")
  cat("Skipped:", summary$total_pdfs_skipped, "\n")
  cat("Failed:", summary$total_pdfs_failed, "\n")
  cat("\n")
  cat("Total Pages:", format(summary$total_pages, big.mark = ","), "\n")
  cat("Total Characters:", format(summary$total_chars, big.mark = ","), "\n")
  cat("\n")
  cat("Processing Time:", round(summary$elapsed_time, 1), "seconds\n")
  
  if (summary$total_pdfs_processed > 0) {
    success_rate <- round(summary$total_pdfs_processed / summary$total_pdfs_found * 100, 1)
    cat("Success Rate:", success_rate, "%\n")
  }
  
  cat(strrep("=", 75), "\n")
  
  if (summary$total_pdfs_failed > 0) {
    cat("\n⚠ Some PDFs failed to extract. Review log for details.\n")
  }
  
  if (summary$total_pdfs_processed > 0) {
    cat("\n✓ Text extraction complete!\n")
    cat("  Companion .txt files created for all PDFs.\n")
    cat("  Ready to run download_strategic_pdfs.R for future updates.\n")
  }
}

# SCRIPT EXECUTION ----
if (!interactive()) {
  # Run automatically if sourced as script
  main()
} else {
  # Provide instructions if loaded interactively
  cat("\n", strrep("=", 75), "\n")
  cat("ONE-TIME TEXT EXTRACTION SCRIPT LOADED\n")
  cat(strrep("=", 75), "\n\n")
  cat("Script loaded. Run with: results <- main()\n")
  cat("To change test mode: CONFIG$test_mode <- FALSE\n")
  cat("To change skip existing: CONFIG$skip_existing_txt <- FALSE\n\n")
  cat("This script will:\n")
  cat("  1. Find all existing strategic plan PDFs\n")
  cat("  2. Extract text using pdftools::pdf_text()\n")
  cat("  3. Save companion .txt files alongside PDFs\n")
  cat("  4. Create detailed extraction log\n")
  cat("  5. Generate summary report\n\n")
}
#source("E:/Hospital_Strategic_Plans/code/OneTimeExtractPDFs.R")
main()
