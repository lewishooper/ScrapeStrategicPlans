# ============================================================================
# HOSPITAL DISCOVERY MODULE - YAML-BASED
# ============================================================================
# Purpose: Discover hospitals and PDFs from folder structure using YAML metadata
# Replaces directory-scanning approach with authoritative YAML source
# Author: Skip & Claude
# Created: 2026-01-24
# Version: 2.0 (Refactored for folder structure)
# ============================================================================

library(yaml)
library(dplyr)
library(tibble)
library(tools)

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

#' Format FAC code with leading zeros
format_fac <- function(fac) {
  sprintf("%03d", as.numeric(fac))
}

#' Check if folder contains "none.txt" indicator
has_no_strategic_plan <- function(folder_path) {
  none_file <- file.path(folder_path, "none.txt")
  file.exists(none_file)
}

#' Get all PDFs in a folder matching strategy naming pattern
#' Get all PDFs in a folder matching strategy naming pattern
#' Matches: Strategy_YYYYMM_FAC.pdf and Strategy_YYYYMM_FAC_Manual.pdf
get_strategy_pdfs <- function(folder_path) {
  if (!dir.exists(folder_path)) {
    return(character(0))
  }
  
  # Look for Strategy_YYYYMM_FAC.pdf or Strategy_YYYYMM_FAC_Manual.pdf
  # Pattern: Strategy_[6 digits]_[3 digits].pdf or Strategy_[6 digits]_[3 digits]_Manual.pdf
  pdf_files <- list.files(
    folder_path, 
    pattern = "^Strategy_[0-9]{6}_[0-9]{3}(_Manual)?\\.pdf$", 
    full.names = TRUE,
    ignore.case = FALSE  # Case-sensitive: _Manual only, not _manual
  )
  
  return(pdf_files)
}

#' Select most recent PDF from multiple options
#' Assumes filename pattern: Strategy_YYYYMM_FAC.pdf or Strategy_YYYYMM_FAC_Manual.pdf
#' Selects based on YYYYMM date in filename, not alphabetic sort
select_most_recent_pdf <- function(pdf_files) {
  if (length(pdf_files) == 0) return(NULL)
  if (length(pdf_files) == 1) return(pdf_files[1])
  
  # Extract YYYYMM date from filename
  extract_date <- function(filename) {
    # Match Strategy_YYYYMM_FAC.pdf or Strategy_YYYYMM_FAC_Manual.pdf
    # Extract the 6-digit date after "Strategy_"
    match <- regexpr("Strategy_([0-9]{6})_", basename(filename))
    if (match > 0) {
      date_str <- substr(basename(filename), match[1] + 9, match[1] + 14)
      return(as.numeric(date_str))
    }
    return(0)  # Return 0 if pattern doesn't match
  }
  
  # Get dates for all files
  dates <- sapply(pdf_files, extract_date)
  
  # Return file with most recent date (highest number)
  return(pdf_files[which.max(dates)])
}

#' Validate PDF is readable
validate_pdf <- function(pdf_path) {
  if (!file.exists(pdf_path)) {
    return(list(valid = FALSE, error = "File not found"))
  }
  
  # Check file size (minimum 10KB, maximum 100MB)
  file_size_kb <- file.size(pdf_path) / 1024
  
  if (file_size_kb < 10) {
    return(list(valid = FALSE, error = "File too small (< 10KB)"))
  }
  
  if (file_size_kb > 100000) {
    return(list(valid = FALSE, error = "File too large (> 100MB)"))
  }
  
  # Try to get PDF info
  tryCatch({
    info <- pdftools::pdf_info(pdf_path)
    
    if (info$pages == 0) {
      return(list(valid = FALSE, error = "PDF has 0 pages"))
    }
    
    return(list(
      valid = TRUE, 
      error = NA,
      pages = info$pages,
      size_kb = round(file_size_kb, 2)
    ))
  }, error = function(e) {
    return(list(valid = FALSE, error = paste("PDF read error:", e$message)))
  })
}

# ============================================================================
# MAIN DISCOVERY FUNCTION
# ============================================================================

#' Get hospital list from YAML with PDF paths and validation
#' 
#' @param yaml_path Path to Hospital_strategy.yaml file
#' @param strategic_plans_dir Path to strategic_plans folder
#' @param filter_needs_extraction If TRUE, only return hospitals needing extraction
#' @param verbose Print detailed progress messages
#' 
#' @return Tibble with columns:
#'   - fac_code: FAC with leading zeros
#'   - hospital_name: Hospital name from YAML
#'   - content_type: Type from YAML (pdf, html)
#'   - folder_path: Full path to hospital folder
#'   - pdf_path: Full path to selected PDF (or NA)
#'   - pdf_filename: Just the filename (or NA)
#'   - pdf_valid: TRUE/FALSE
#'   - pdf_pages: Number of pages (or NA)
#'   - pdf_size_kb: File size (or NA)
#'   - skip_reason: Reason for skipping (or NA)
#'   - needs_extraction: TRUE/FALSE from YAML
#'   - baseline_extracted: TRUE/FALSE from YAML
get_hospital_list_from_yaml <- function(
    yaml_path,
    strategic_plans_dir,
    filter_needs_extraction = FALSE,
    verbose = TRUE
) {
  
  if (verbose) {
    cat("\n")
    cat(strrep("=", 80), "\n")
    cat("HOSPITAL DISCOVERY FROM YAML\n")
    cat(strrep("=", 80), "\n")
  }
  
  # Validate inputs
  if (!file.exists(yaml_path)) {
    stop("YAML file not found: ", yaml_path)
  }
  
  if (!dir.exists(strategic_plans_dir)) {
    stop("Strategic plans directory not found: ", strategic_plans_dir)
  }
  
  # Read YAML
  if (verbose) cat("Reading YAML file...\n")
  yaml_data <- read_yaml(yaml_path)
  
  # Handle both structures (with or without 'hospitals' wrapper)
  if (is.null(yaml_data$hospitals)) {
    hospitals <- yaml_data
  } else {
    hospitals <- yaml_data$hospitals
  }
  
  if (verbose) {
    cat(sprintf("  Total hospitals in YAML: %d\n", length(hospitals)))
  }
  
  # Initialize results
  hospital_list <- tibble()
  
  # Tracking counters
  counts <- list(
    total = length(hospitals),
    no_strategic_plan = 0,
    no_pdf_found = 0,
    html_only = 0,
    multiple_pdfs = 0,
    pdf_invalid = 0,
    pdf_valid = 0,
    needs_extraction = 0,
    already_extracted = 0
  )
  
  # Process each hospital
  if (verbose) cat("\nProcessing hospitals...\n")
  
  for (i in seq_along(hospitals)) {
    hospital <- hospitals[[i]]
    
    # Extract basic info
    fac <- format_fac(hospital$FAC)
    name <- hospital$name
    
    # Get folder path
    local_folder <- hospital$strategy_search$local_folder
    
    if (is.null(local_folder) || local_folder == "") {
      # Fallback: construct from FAC and name
      name_clean <- gsub("[^a-zA-Z0-9_-]", "_", name)
      name_clean <- gsub("_{2,}", "_", name_clean)
      name_clean <- substr(name_clean, 1, 100)
      local_folder <- paste0(fac, "_", name_clean)
    }
    
    folder_path <- file.path(strategic_plans_dir, local_folder)
    
    # Get content type
    content_type <- hospital$strategy_search$content_type
    if (is.null(content_type) || content_type == "") {
      content_type <- "unknown"
    }
    
    # Get extraction flags
    # Get extraction flags Revised for comparison_result
    comparison_result <- hospital$strategy_search$comparison_result
    baseline_extracted <- hospital$strategy_search$baseline_extracted
    
    # Default to "changed" if comparison_result not set (conservative approach)
    if (is.null(comparison_result) || comparison_result == "" || is.na(comparison_result)) {
      comparison_result <- "changed"
    }
    
    # Determine if extraction needed
    # Extract if: (1) file changed, (2) first download, or (3) baseline never done
    needs_extraction_flag <- (comparison_result %in% c("changed", "first_download")) ||
      is.null(baseline_extracted) ||
      is.na(baseline_extracted) ||
      !isTRUE(baseline_extracted) && 
      !identical(baseline_extracted, "yes")
    
    baseline_extracted_flag <- isTRUE(baseline_extracted) || 
      identical(baseline_extracted, "yes")
    
    # Initialize row
    row <- tibble(
      fac_code = fac,
      hospital_name = name,
      content_type = content_type,
      folder_path = folder_path,
      pdf_path = NA_character_,
      pdf_filename = NA_character_,
      pdf_valid = FALSE,
      pdf_pages = NA_integer_,
      pdf_size_kb = NA_real_,
      skip_reason = NA_character_,
      needs_extraction = needs_extraction_flag,
      baseline_extracted = baseline_extracted_flag
    )
    
    # Check if folder exists
    if (!dir.exists(folder_path)) {
      row$skip_reason <- "folder_not_found"
      counts$no_pdf_found <- counts$no_pdf_found + 1
      hospital_list <- bind_rows(hospital_list, row)
      next
    }
    
    # Check for none.txt
    if (has_no_strategic_plan(folder_path)) {
      row$skip_reason <- "no_strategic_plan"
      counts$no_strategic_plan <- counts$no_strategic_plan + 1
      hospital_list <- bind_rows(hospital_list, row)
      next
    }
    
    # Check for HTML-only
    if (content_type == "html") {
      row$skip_reason <- "html_only_no_pdf"
      counts$html_only <- counts$html_only + 1
      hospital_list <- bind_rows(hospital_list, row)
      next
    }
    
    # Get PDFs
    pdf_files <- get_strategy_pdfs(folder_path)
    
    if (length(pdf_files) == 0) {
      row$skip_reason <- "no_pdf_found"
      counts$no_pdf_found <- counts$no_pdf_found + 1
      hospital_list <- bind_rows(hospital_list, row)
      next
    }
    
    # Select PDF
    if (length(pdf_files) > 1) {
      counts$multiple_pdfs <- counts$multiple_pdfs + 1
      if (verbose) {
        cat(sprintf("  FAC %s: Multiple PDFs found (%d), selecting most recent\n", 
                    fac, length(pdf_files)))
      }
    }
    
    selected_pdf <- select_most_recent_pdf(pdf_files)
    
    # Validate PDF
    validation <- validate_pdf(selected_pdf)
    
    if (!validation$valid) {
      row$skip_reason <- paste0("pdf_invalid: ", validation$error)
      row$pdf_path <- selected_pdf
      row$pdf_filename <- basename(selected_pdf)
      counts$pdf_invalid <- counts$pdf_invalid + 1
      hospital_list <- bind_rows(hospital_list, row)
      next
    }
    
    # PDF is valid
    row$pdf_path <- selected_pdf
    row$pdf_filename <- basename(selected_pdf)
    row$pdf_valid <- TRUE
    row$pdf_pages <- validation$pages
    row$pdf_size_kb <- validation$size_kb
    row$skip_reason <- NA_character_
    counts$pdf_valid <- counts$pdf_valid + 1
    
    if (needs_extraction_flag) {
      counts$needs_extraction <- counts$needs_extraction + 1
    } else {
      counts$already_extracted <- counts$already_extracted + 1
    }
    
    hospital_list <- bind_rows(hospital_list, row)
  }
  
  # Filter if requested
  if (filter_needs_extraction) {
    original_count <- nrow(hospital_list)
    hospital_list <- hospital_list %>%
      filter(pdf_valid & needs_extraction)
    
    if (verbose) {
      cat(sprintf("\nFiltered to hospitals needing extraction: %d → %d\n",
                  original_count, nrow(hospital_list)))
    }
  }
  
  # Print summary
  if (verbose) {
    cat("\n")
    cat(strrep("=", 80), "\n")
    cat("DISCOVERY SUMMARY\n")
    cat(strrep("=", 80), "\n")
    cat(sprintf("Total hospitals:           %d\n", counts$total))
    cat(sprintf("Valid PDFs found:          %d\n", counts$pdf_valid))
    cat(sprintf("\nSkipped:\n"))
    cat(sprintf("  No strategic plan:       %d\n", counts$no_strategic_plan))
    cat(sprintf("  No PDF found:            %d\n", counts$no_pdf_found))
    cat(sprintf("  HTML only:               %d\n", counts$html_only))
    cat(sprintf("  Invalid PDF:             %d\n", counts$pdf_invalid))
    cat(sprintf("\nExtraction Status:\n"))
    cat(sprintf("  Needs extraction:        %d\n", counts$needs_extraction))
    cat(sprintf("  Already extracted:       %d\n", counts$already_extracted))
    
    if (counts$multiple_pdfs > 0) {
      cat(sprintf("\nMultiple PDFs found:       %d hospitals\n", counts$multiple_pdfs))
    }
    cat(strrep("=", 80), "\n\n")
  }
  
  return(hospital_list)
}

# ============================================================================
# LOGGING FUNCTIONS
# ============================================================================

#' Create skipped hospitals log
create_skipped_log <- function(hospital_list, output_path) {
  
  skipped <- hospital_list %>%
    filter(!is.na(skip_reason)) %>%
    select(fac_code, hospital_name, content_type, skip_reason, folder_path)
  
  if (nrow(skipped) == 0) {
    cat("No hospitals skipped - no log file created\n")
    return(invisible(NULL))
  }
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  log_file <- file.path(output_path, paste0("skipped_hospitals_", timestamp, ".csv"))
  
  write.csv(skipped, log_file, row.names = FALSE)
  
  cat(sprintf("Skipped hospitals log created: %s\n", basename(log_file)))
  cat(sprintf("  Total skipped: %d\n", nrow(skipped)))
  
  # Breakdown by reason
  cat("\nBreakdown by reason:\n")
  summary <- skipped %>%
    group_by(skip_reason) %>%
    summarise(count = n(), .groups = "drop") %>%
    arrange(desc(count))
  
  print(summary, n = Inf)
  
  return(invisible(log_file))
}

# ============================================================================
# INSTRUCTIONS
# ============================================================================

if (interactive()) {
  cat("\n")
  cat(strrep("=", 80), "\n")
  cat("HOSPITAL DISCOVERY MODULE - YAML-BASED\n")
  cat(strrep("=", 80), "\n")
  cat("\nUSAGE:\n")
  cat("  # Get all hospitals with valid PDFs\n")
  cat("  hospitals <- get_hospital_list_from_yaml(\n")
  cat("    yaml_path = 'E:/Hospital_Strategic_Plans/code/Hospital_strategy.yaml',\n")
  cat("    strategic_plans_dir = 'E:/Hospital_Strategic_Plans/strategic_plans'\n")
  cat("  )\n\n")
  cat("  # Get only hospitals needing extraction\n")
  cat("  hospitals <- get_hospital_list_from_yaml(\n")
  cat("    yaml_path = 'E:/Hospital_Strategic_Plans/code/Hospital_strategy.yaml',\n")
  cat("    strategic_plans_dir = 'E:/Hospital_Strategic_Plans/strategic_plans',\n")
  cat("    filter_needs_extraction = TRUE\n")
  cat("  )\n\n")
  cat("  # Create log of skipped hospitals\n")
  cat("  create_skipped_log(hospitals, output_path = 'E:/Hospital_Strategic_Plans/outputs')\n")
  cat(strrep("=", 80), "\n\n")
}
