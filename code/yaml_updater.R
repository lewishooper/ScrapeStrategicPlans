# ============================================================================
# YAML UPDATE MODULE - CHANGE DETECTION INTEGRATION
# ============================================================================
# Purpose: Update Hospital_strategy.yaml with extraction tracking flags
#          after download_strategic_pdfs.R runs comparison
# Author: Skip & Claude
# Created: 2026-01-24
# Version: 1.0
# ============================================================================

library(yaml)
library(dplyr)

# ============================================================================
# YAML FIELD SCHEMA FOR EXTRACTION TRACKING
# ============================================================================
# These fields will be added/updated in strategy_search section:
#
# last_checked_date:        Date when PDF was last checked/downloaded
# last_changed_date:        Date when PDF content actually changed
# comparison_result:        'identical', 'changed', or 'first_download'
# baseline_extracted:       Has this been extracted via API at least once? (yes/no)
# baseline_extraction_date: Date of first API extraction
# needs_api_extraction:     Does current PDF need API extraction? (yes/no)
# extraction_batch:         Which batch number was it extracted in
# extraction_notes:         Any notes about extraction status
# ============================================================================

#' Update YAML with comparison results and extraction flags
#' 
#' This function should be called AFTER download_strategic_pdfs.R runs
#' It reads the download log and updates the YAML file with:
#' - Comparison results (changed/identical)
#' - Extraction flags (needs_api_extraction)
#' - Timestamps
#' 
#' @param yaml_path Path to Hospital_strategy.yaml
#' @param log_data Download log dataframe from download_strategic_pdfs.R
#' @param create_backup Create backup before updating (default TRUE)
#' @return Updated hospital list (invisibly)
update_yaml_with_comparison_results <- function(
    yaml_path,
    log_data,
    create_backup = TRUE
) {
  
  cat("\n")
  cat(strrep("=", 80), "\n")
  cat("UPDATING YAML WITH COMPARISON RESULTS\n")
  cat(strrep("=", 80), "\n")
  
  # Read current YAML
  cat("Reading YAML file...\n")
  yaml_data <- read_yaml(yaml_path)
  
  if (is.null(yaml_data$hospitals)) {
    hospitals <- yaml_data
  } else {
    hospitals <- yaml_data$hospitals
  }
  
  cat(sprintf("  Total hospitals in YAML: %d\n", length(hospitals)))
  cat(sprintf("  Log entries to process: %d\n", nrow(log_data)))
  
  # Create backup if requested
  if (create_backup) {
    backup_path <- paste0(yaml_path, ".backup_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    file.copy(yaml_path, backup_path)
    cat(sprintf("  ✓ Backup created: %s\n", basename(backup_path)))
  }
  
  # Track updates
  updates <- list(
    total_processed = 0,
    changed_flagged = 0,
    identical_cleared = 0,
    first_downloads = 0,
    errors = 0
  )
  
  # Process each log entry
  for (i in 1:nrow(log_data)) {
    log_entry <- log_data[i, ]
    
    # Find matching hospital in YAML
    fac <- sprintf("%03d", as.numeric(log_entry$FAC))
    
    hospital_idx <- NULL
    for (j in seq_along(hospitals)) {
      if (sprintf("%03d", as.numeric(hospitals[[j]]$FAC)) == fac) {
        hospital_idx <- j
        break
      }
    }
    
    if (is.null(hospital_idx)) {
      cat(sprintf("  ⚠ Warning: FAC %s not found in YAML\n", fac))
      updates$errors <- updates$errors + 1
      next
    }
    
    # Get current hospital
    hospital <- hospitals[[hospital_idx]]
    
    # Ensure strategy_search exists
    if (is.null(hospital$strategy_search)) {
      hospital$strategy_search <- list()
    }
    
    # Update last_checked_date
    hospital$strategy_search$last_checked_date <- as.character(Sys.Date())
    
    # Update based on comparison result
    comparison_result <- log_entry$comparison_result
    
    if (!is.na(comparison_result)) {
      hospital$strategy_search$comparison_result <- comparison_result
      
      if (comparison_result == "changed") {
        # PDF has changed - needs extraction
        hospital$strategy_search$needs_api_extraction <- "yes"
        hospital$strategy_search$last_changed_date <- as.character(Sys.Date())
        updates$changed_flagged <- updates$changed_flagged + 1
        
      } else if (comparison_result == "identical") {
        # PDF unchanged - clear extraction flag if baseline exists
        if (isTRUE(hospital$strategy_search$baseline_extracted == "yes")) {
          hospital$strategy_search$needs_api_extraction <- "no"
        } else {
          # No baseline yet - still needs extraction
          hospital$strategy_search$needs_api_extraction <- "yes"
        }
        updates$identical_cleared <- updates$identical_cleared + 1
        
      } else if (comparison_result == "first_download") {
        # First time downloading - definitely needs extraction
        hospital$strategy_search$needs_api_extraction <- "yes"
        hospital$strategy_search$last_changed_date <- as.character(Sys.Date())
        updates$first_downloads <- updates$first_downloads + 1
      }
    }
    
    # Update action info
    if (!is.na(log_entry$action)) {
      hospital$strategy_search$last_download_action <- log_entry$action
    }
    
    # Save updated hospital back to list
    hospitals[[hospital_idx]] <- hospital
    updates$total_processed <- updates$total_processed + 1
  }
  
  # Write updated YAML
  cat("\nWriting updated YAML...\n")
  write_yaml(hospitals, yaml_path)
  
  # Summary
  cat("\n")
  cat(strrep("=", 80), "\n")
  cat("UPDATE SUMMARY\n")
  cat(strrep("=", 80), "\n")
  cat(sprintf("Hospitals processed:       %d\n", updates$total_processed))
  cat(sprintf("Flagged for extraction:    %d (changed)\n", updates$changed_flagged))
  cat(sprintf("Cleared (unchanged):       %d (identical)\n", updates$identical_cleared))
  cat(sprintf("First downloads:           %d (new)\n", updates$first_downloads))
  if (updates$errors > 0) {
    cat(sprintf("Errors:                    %d\n", updates$errors))
  }
  cat(strrep("=", 80), "\n\n")
  
  cat("✓ YAML file updated with extraction flags\n\n")
  
  return(invisible(hospitals))
}

#' Mark hospital as baseline extracted after successful API extraction
#' 
#' Call this after successful API extraction to update YAML
#' 
#' @param yaml_path Path to Hospital_strategy.yaml
#' @param fac_code FAC code of hospital (with or without leading zeros)
#' @param batch_number Batch number it was extracted in
#' @param create_backup Create backup before updating (default TRUE)
mark_hospital_as_extracted <- function(
    yaml_path,
    fac_code,
    batch_number,
    create_backup = TRUE
) {
  
  # Format FAC
  fac <- sprintf("%03d", as.numeric(fac_code))
  
  # Read YAML
  yaml_data <- read_yaml(yaml_path)
  
  if (is.null(yaml_data$hospitals)) {
    hospitals <- yaml_data
  } else {
    hospitals <- yaml_data$hospitals
  }
  
  # Find hospital
  hospital_idx <- NULL
  for (j in seq_along(hospitals)) {
    if (sprintf("%03d", as.numeric(hospitals[[j]]$FAC)) == fac) {
      hospital_idx <- j
      break
    }
  }
  
  if (is.null(hospital_idx)) {
    warning(sprintf("FAC %s not found in YAML", fac))
    return(invisible(FALSE))
  }
  
  # Create backup if requested
  if (create_backup) {
    backup_path <- paste0(yaml_path, ".backup_extraction_", 
                          format(Sys.time(), "%Y%m%d_%H%M%S"))
    file.copy(yaml_path, backup_path, overwrite = FALSE)
  }
  
  # Update hospital
  hospital <- hospitals[[hospital_idx]]
  
  if (is.null(hospital$strategy_search)) {
    hospital$strategy_search <- list()
  }
  
  # Mark as baseline extracted
  hospital$strategy_search$baseline_extracted <- "yes"
  hospital$strategy_search$baseline_extraction_date <- as.character(Sys.Date())
  hospital$strategy_search$needs_api_extraction <- "no"
  hospital$strategy_search$extraction_batch <- batch_number
  hospital$strategy_search$last_extraction_date <- as.character(Sys.Date())
  
  # Save back
  hospitals[[hospital_idx]] <- hospital
  
  # Write YAML
  write_yaml(hospitals, yaml_path)
  
  cat(sprintf("✓ FAC %s marked as extracted (Batch %d)\n", fac, batch_number))
  
  return(invisible(TRUE))
}

#' Batch update extraction status for multiple hospitals
#' 
#' More efficient than calling mark_hospital_as_extracted repeatedly
#' 
#' @param yaml_path Path to Hospital_strategy.yaml  
#' @param fac_codes Vector of FAC codes that were successfully extracted
#' @param batch_number Batch number
#' @param create_backup Create backup (default TRUE)
batch_mark_as_extracted <- function(
    yaml_path,
    fac_codes,
    batch_number,
    create_backup = TRUE
) {
  
  if (length(fac_codes) == 0) {
    cat("No hospitals to mark as extracted\n")
    return(invisible(NULL))
  }
  
  cat(sprintf("\nMarking %d hospitals as extracted...\n", length(fac_codes)))
  
  # Format FACs
  facs <- sprintf("%03d", as.numeric(fac_codes))
  
  # Read YAML
  yaml_data <- read_yaml(yaml_path)
  
  if (is.null(yaml_data$hospitals)) {
    hospitals <- yaml_data
  } else {
    hospitals <- yaml_data$hospitals
  }
  
  # Create backup
  if (create_backup) {
    backup_path <- paste0(yaml_path, ".backup_batch_extraction_", 
                          format(Sys.time(), "%Y%m%d_%H%M%S"))
    file.copy(yaml_path, backup_path, overwrite = FALSE)
    cat(sprintf("  ✓ Backup created: %s\n", basename(backup_path)))
  }
  
  # Update each hospital
  updated_count <- 0
  
  for (fac in facs) {
    # Find hospital
    hospital_idx <- NULL
    for (j in seq_along(hospitals)) {
      if (sprintf("%03d", as.numeric(hospitals[[j]]$FAC)) == fac) {
        hospital_idx <- j
        break
      }
    }
    
    if (is.null(hospital_idx)) {
      cat(sprintf("  ⚠ FAC %s not found in YAML\n", fac))
      next
    }
    
    # Update
    hospital <- hospitals[[hospital_idx]]
    
    if (is.null(hospital$strategy_search)) {
      hospital$strategy_search <- list()
    }
    
    hospital$strategy_search$baseline_extracted <- "yes"
    hospital$strategy_search$baseline_extraction_date <- as.character(Sys.Date())
    hospital$strategy_search$needs_api_extraction <- "no"
    hospital$strategy_search$extraction_batch <- batch_number
    hospital$strategy_search$last_extraction_date <- as.character(Sys.Date())
    
    hospitals[[hospital_idx]] <- hospital
    updated_count <- updated_count + 1
  }
  
  # Write YAML
  write_yaml(hospitals, yaml_path)
  
  cat(sprintf("  ✓ Marked %d hospitals as extracted (Batch %d)\n", 
              updated_count, batch_number))
  
  return(invisible(TRUE))
}

#' Get summary of extraction status from YAML
#' 
#' @param yaml_path Path to Hospital_strategy.yaml
#' @return Summary dataframe
get_extraction_status_summary <- function(yaml_path) {
  
  # Read YAML
  yaml_data <- read_yaml(yaml_path)
  
  if (is.null(yaml_data$hospitals)) {
    hospitals <- yaml_data
  } else {
    hospitals <- yaml_data$hospitals
  }
  
  cat("\n")
  cat(strrep("=", 80), "\n")
  cat("EXTRACTION STATUS SUMMARY\n")
  cat(strrep("=", 80), "\n")
  
  status_summary <- data.frame(
    total = length(hospitals),
    baseline_extracted = 0,
    needs_extraction = 0,
    up_to_date = 0,
    no_pdf = 0,
    stringsAsFactors = FALSE
  )
  
  for (hospital in hospitals) {
    search <- hospital$strategy_search
    
    if (is.null(search)) next
    
    baseline_extracted <- isTRUE(search$baseline_extracted == "yes")
    needs_extraction <- isTRUE(search$needs_api_extraction == "yes")
    has_pdf <- !is.null(search$pdf_downloaded) && 
               (isTRUE(search$pdf_downloaded == "yes") || isTRUE(search$pdf_downloaded))
    
    if (baseline_extracted) {
      status_summary$baseline_extracted <- status_summary$baseline_extracted + 1
    }
    
    if (needs_extraction) {
      status_summary$needs_extraction <- status_summary$needs_extraction + 1
    }
    
    if (baseline_extracted && !needs_extraction) {
      status_summary$up_to_date <- status_summary$up_to_date + 1
    }
    
    if (!has_pdf) {
      status_summary$no_pdf <- status_summary$no_pdf + 1
    }
  }
  
  cat(sprintf("Total hospitals:          %d\n", status_summary$total))
  cat(sprintf("Baseline extracted:       %d\n", status_summary$baseline_extracted))
  cat(sprintf("Needs extraction:         %d\n", status_summary$needs_extraction))
  cat(sprintf("Up to date:               %d\n", status_summary$up_to_date))
  cat(sprintf("No PDF available:         %d\n", status_summary$no_pdf))
  cat(strrep("=", 80), "\n\n")
  
  return(invisible(status_summary))
}

# ============================================================================
# INSTRUCTIONS
# ============================================================================

if (interactive()) {
  cat("\n")
  cat(strrep("=", 80), "\n")
  cat("YAML UPDATE MODULE - CHANGE DETECTION INTEGRATION\n")
  cat(strrep("=", 80), "\n")
  cat("\nUSAGE:\n\n")
  cat("1. AFTER DOWNLOAD SCRIPT RUNS:\n")
  cat("   # Assuming download script returns log_data\n")
  cat("   update_yaml_with_comparison_results(\n")
  cat("     yaml_path = 'E:/Hospital_Strategic_Plans/code/Hospital_strategy.yaml',\n")
  cat("     log_data = download_log_data\n")
  cat("   )\n\n")
  cat("2. AFTER SUCCESSFUL API EXTRACTION:\n")
  cat("   mark_hospital_as_extracted(\n")
  cat("     yaml_path = 'E:/Hospital_Strategic_Plans/code/Hospital_strategy.yaml',\n")
  cat("     fac_code = '592',\n")
  cat("     batch_number = 23\n")
  cat("   )\n\n")
  cat("3. BATCH UPDATE AFTER EXTRACTION:\n")
  cat("   batch_mark_as_extracted(\n")
  cat("     yaml_path = 'E:/Hospital_Strategic_Plans/code/Hospital_strategy.yaml',\n")
  cat("     fac_codes = c('592', '593', '597'),\n")
  cat("     batch_number = 23\n")
  cat("   )\n\n")
  cat("4. CHECK STATUS:\n")
  cat("   get_extraction_status_summary(\n")
  cat("     yaml_path = 'E:/Hospital_Strategic_Plans/code/Hospital_strategy.yaml'\n")
  cat("   )\n\n")
  cat(strrep("=", 80), "\n\n")
}
