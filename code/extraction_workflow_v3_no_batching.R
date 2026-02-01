# ============================================================================
# EXTRACTION WORKFLOW - SIMPLIFIED (NO BATCHING)
# ============================================================================
# Master script for processing hospitals using Claude API
# Simplified version - single output file per extraction run
# Author: Skip & Claude
# Version: 3.0 (No Batching)
# Last Updated: 2026-01-24
# ============================================================================

# Load required libraries
library(httr)
library(jsonlite)
library(pdftools)
library(dplyr)
library(yaml)

# Source all function files
source("config_v2.R")
source("hospital_discovery.R")
source("api_functions_with_images.R")
source("pdf_image_processor.R")
source("progress_functions.R")

# ============================================================================
# MAIN EXTRACTION FUNCTION
# ============================================================================

run_extraction_workflow <- function(
    test_mode = FALSE,
    max_hospitals = NULL,
    filter_facs = NULL  # Optional: only process specific FAC codes
) {
  
  cat("\n")
  cat(strrep("=", 80), "\n")
  cat("   HOSPITAL STRATEGIC PLAN EXTRACTION WORKFLOW V3.0\n")
  cat("   Simplified Single-File Output - No Batching\n")
  cat("   Protocol Version: V4.1\n")
  cat(strrep("=", 80), "\n")
  cat("\n")
  
  # =========================================================================
  # STEP 1: Validate configuration
  # =========================================================================
  cat("STEP 1: Validating configuration...\n")
  validate_config()
  cat(sprintf("  Mode: %s\n", get_mode_description()))
  
  # =========================================================================
  # STEP 2: Load protocol
  # =========================================================================
  cat("\nSTEP 2: Loading extraction protocol...\n")
  
  if (!file.exists(PROTOCOL_FILE)) {
    stop("Protocol file not found: ", PROTOCOL_FILE)
  }
  
  protocol_text <- readLines(PROTOCOL_FILE, warn = FALSE)
  protocol_text <- paste(protocol_text, collapse = "\n")
  cat(sprintf("  ✓ Loaded protocol (%d characters)\n", nchar(protocol_text)))
  
  # =========================================================================
  # STEP 3: Discover hospitals from YAML
  # =========================================================================
  cat("\nSTEP 3: Discovering hospitals from YAML...\n")
  
  # Determine filtering based on extraction mode
  # Determine filtering based on extraction mode
  # - baseline: Extract all hospitals without baseline extraction
  # - changes_only: Extract only hospitals with comparison_result = "changed"
  # - force_all: Extract all hospitals regardless of status
  filter_needs_extraction <- (EXTRACTION_MODE == "changes_only")
  
  all_hospitals <- get_hospital_list_from_yaml(
    yaml_path = YAML_FILE,
    strategic_plans_dir = STRATEGIC_PLANS_DIR,
    filter_needs_extraction = filter_needs_extraction,
    verbose = TRUE
  )
  
  # Create skipped log
  create_skipped_log(all_hospitals, LOG_DIR)
  
  # Filter to valid PDFs
  valid_hospitals <- all_hospitals %>%
    filter(pdf_valid)
  
  cat(sprintf("\nHospitals with valid PDFs: %d\n", nrow(valid_hospitals)))
  
  # Apply FAC filter if specified (for testing)
  if (!is.null(filter_facs)) {
    valid_hospitals <- valid_hospitals %>%
      filter(fac_code %in% sprintf("%03d", as.numeric(filter_facs)))
    cat(sprintf("Filtered to specific FACs: %d hospitals\n", nrow(valid_hospitals)))
  }
  
  # Apply skip list
  # Apply skip list
  if (length(SKIP_FACS) > 0) {
    before <- nrow(valid_hospitals)
    
    # Log which hospitals are being skipped from SKIP_FACS
    skipped_facs_hospitals <- valid_hospitals %>%
      filter(fac_code %in% SKIP_FACS)
    
    if (nrow(skipped_facs_hospitals) > 0) {
      # Create skip log file
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      skip_log_file <- file.path(LOG_DIR, paste0("skipped_by_config_", timestamp, ".csv"))
      
      skipped_facs_hospitals <- skipped_facs_hospitals %>%
        mutate(skip_reason = "pdf_too_large_for_api") %>%
        select(fac_code, hospital_name, pdf_filename, pdf_size_kb, skip_reason)
      
      write.csv(skipped_facs_hospitals, skip_log_file, row.names = FALSE)
      cat(sprintf("Created skip log: %s\n", basename(skip_log_file)))
    }
    
    # Now filter them out
    valid_hospitals <- valid_hospitals %>%
      filter(!(fac_code %in% SKIP_FACS))
    
    skipped_count <- before - nrow(valid_hospitals)
    if (skipped_count > 0) {
      cat(sprintf("Skipped %d hospitals from SKIP_FACS list (pdf_too_large_for_api)\n", skipped_count))
    }
  }
  
  if (nrow(valid_hospitals) == 0) {
    cat("\n")
    cat(strrep("=", 80), "\n")
    cat("NO HOSPITALS TO PROCESS\n")
    cat(strrep("=", 80), "\n")
    
    if (EXTRACTION_MODE == "changes_only") {
      cat("All hospitals are up to date - no changes detected!\n")
    } else {
      cat("No valid PDFs found for extraction.\n")
    }
    
    return(invisible(NULL))
  }
  
  # =========================================================================
  # STEP 4: Initialize progress tracking
  # =========================================================================
  cat("\nSTEP 4: Initializing progress tracking...\n")
  initialize_progress_file()
  
  # =========================================================================
  # STEP 5: Check for already processed hospitals
  # =========================================================================
  cat("\nSTEP 5: Checking extraction progress...\n")
  
  if (file.exists(PROGRESS_FILE)) {
    progress <- read.csv(PROGRESS_FILE, stringsAsFactors = FALSE)
    completed_facs <- progress$fac_code[progress$status %in% c("completed", "success")]
    
    already_processed <- valid_hospitals %>%
      filter(fac_code %in% completed_facs)
    
    if (nrow(already_processed) > 0) {
      cat(sprintf("  Previously completed: %d hospitals\n", nrow(already_processed)))
      
      if (EXTRACTION_MODE != "force_all") {
        valid_hospitals <- valid_hospitals %>%
          filter(!(fac_code %in% completed_facs))
        cat(sprintf("  Remaining to process: %d hospitals\n", nrow(valid_hospitals)))
      } else {
        cat("  Force mode: Will re-process all hospitals\n")
      }
    }
  }
  
  if (nrow(valid_hospitals) == 0) {
    cat("\n✓ All hospitals already processed!\n\n")
    print_progress_report()
    return(invisible(NULL))
  }
  
  # Apply max_hospitals limit if in test mode
  if (test_mode || !is.null(max_hospitals)) {
    original_count <- nrow(valid_hospitals)
    limit <- if (test_mode) 3 else max_hospitals
    
    if (nrow(valid_hospitals) > limit) {
      valid_hospitals <- valid_hospitals[1:limit, ]
      cat(sprintf("\n⚠ Limited to %d hospitals (test/max mode)\n", limit))
    }
  }
  
  # =========================================================================
  # STEP 6: Confirm before proceeding
  # =========================================================================
  cat("\n")
  cat(strrep("=", 80), "\n")
  cat("READY TO START EXTRACTION\n")
  cat(strrep("=", 80), "\n")
  cat(sprintf("Mode:                  %s\n", get_mode_description()))
  cat(sprintf("Hospitals to process:  %d\n", nrow(valid_hospitals)))
  
  # Estimate cost and time
  avg_cost_per_hospital <- 0.10
  total_cost <- nrow(valid_hospitals) * avg_cost_per_hospital
  total_time_min <- nrow(valid_hospitals) * 2  # ~2 min per hospital
  
  cat(sprintf("Estimated cost:        $%.2f - $%.2f\n", 
              total_cost * 0.7, total_cost * 1.3))
  cat(sprintf("Estimated time:        %d - %d minutes\n",
              ceiling(total_time_min * 0.8),
              ceiling(total_time_min * 1.2)))
  cat(strrep("=", 80), "\n\n")
  
  if (!test_mode && interactive()) {
    response <- readline("Continue with extraction? (yes/no): ")
    if (tolower(trimws(response)) != "yes") {
      cat("Extraction cancelled.\n")
      return(invisible(NULL))
    }
  }
  
  # =========================================================================
  # STEP 7: Process hospitals serially
  # =========================================================================
  cat("\n")
  cat(strrep("=", 80), "\n")
  cat("    BEGINNING EXTRACTION PROCESSING\n")
  cat(strrep("=", 80), "\n")
  cat("\n")
  
  overall_stats <- list(
    total_input_tokens = 0,
    total_output_tokens = 0,
    total_cost = 0,
    successful = 0,
    failed = 0,
    start_time = Sys.time()
  )
  
  all_extractions <- c()
  individual_outputs <- list()
  successful_facs <- c()  # Track for YAML update
  
  # Process each hospital
  for (i in 1:nrow(valid_hospitals)) {
    hospital <- valid_hospitals[i, ]
    
    cat(sprintf("\n--- Hospital %d/%d: FAC %s - %s ---\n", 
                i, nrow(valid_hospitals), hospital$fac_code, 
                substr(hospital$hospital_name, 1, 50)))
    
    # Verify PDF exists
    if (!file.exists(hospital$pdf_path)) {
      cat(sprintf("  ✗ ERROR: PDF not found at %s\n", hospital$pdf_path))
      
      log_error(hospital$fac_code, hospital$hospital_name, 
                0, "PDF file not found")
      
      update_progress(
        fac_code = hospital$fac_code,
        hospital_name = hospital$hospital_name,
        batch_number = 0,
        status = "failed",
        error_message = "PDF file not found"
      )
      
      overall_stats$failed <- overall_stats$failed + 1
      next
    }
    
    cat(sprintf("  PDF: %s (%d pages, %.1f KB)\n", 
                hospital$pdf_filename, hospital$pdf_pages, hospital$pdf_size_kb))
    cat("  Processing with image-based extraction...\n")
    
    # Get API key
    api_key <- API_KEY
    
    if (is.null(api_key) || api_key == "") {
      stop("API_KEY not set - check configuration")
    }
    
    # Extract using image-based approach
    extraction_result <- extract_hospital_with_images(
      pdf_path = hospital$pdf_path,
      protocol_text = protocol_text,
      api_key = api_key,
      batch_num = 0,  # No batching
      hospital_num = i,
      fac_code = hospital$fac_code,           # ADD THIS LINE
      hospital_name = NULL,  # ADD THIS LINE
      dpi = PDF_DPI,
      max_tokens = MAX_TOKENS
    )
    
    if (extraction_result$success) {
      # Create hospital header
      hospital_header <- sprintf(
        "HOSPITAL %d OF %d\nFAC: %s\n\n%s\n\n",
        i, nrow(valid_hospitals), hospital$fac_code,
        strrep("=", 60)
      )
      
      full_extraction <- paste0(hospital_header, extraction_result$extraction_text)
      
      # Add to master list
      if (i > 1) {
        separator <- paste0("\n\n", strrep("=", 60), "\n", strrep("=", 60), "\n\n")
        all_extractions <- c(all_extractions, separator)
      }
      all_extractions <- c(all_extractions, full_extraction)
      
      # Save individual file if enabled
      if (CREATE_INDIVIDUAL_FILES) {
        individual_file <- save_individual_extraction(
          extraction_text = extraction_result$extraction_text,
          fac_code = hospital$fac_code,
          hospital_name = hospital$hospital_name
        )
        individual_outputs[[hospital$fac_code]] <- individual_file
      }
      
      # Track successful FAC for YAML update
      successful_facs <- c(successful_facs, hospital$fac_code)
      
      # Update progress
      update_progress(
        fac_code = hospital$fac_code,
        hospital_name = hospital$hospital_name,
        batch_number = 0,
        status = "completed",
        input_tokens = extraction_result$input_tokens,
        output_tokens = extraction_result$output_tokens,
        cost = calculate_cost(extraction_result$input_tokens, 
                             extraction_result$output_tokens)$total_cost
      )
      
      # Update stats
      overall_stats$total_input_tokens <- overall_stats$total_input_tokens + 
        extraction_result$input_tokens
      overall_stats$total_output_tokens <- overall_stats$total_output_tokens + 
        extraction_result$output_tokens
      overall_stats$total_cost <- overall_stats$total_cost + 
        calculate_cost(extraction_result$input_tokens, 
                      extraction_result$output_tokens)$total_cost
      overall_stats$successful <- overall_stats$successful + 1
      
      cat(sprintf("  ✓ Success - Cost: $%.4f\n", 
                  calculate_cost(extraction_result$input_tokens, 
                                extraction_result$output_tokens)$total_cost))
      
   
      # Log failure
    } else {
      # Log failure
      # Handle NULL or empty error message
      error_msg <- if (!is.null(extraction_result$error) && length(extraction_result$error) > 0) {
        extraction_result$error
      } else {
        "Unknown error during extraction"
      }
      
      cat(sprintf("  ✗ Failed: %s\n", error_msg))
      
      log_error(hospital$fac_code, hospital$hospital_name, 
                0, error_msg)
      
      update_progress(
        fac_code = hospital$fac_code,
        hospital_name = hospital$hospital_name,
        batch_number = 0,
        status = "failed",
        error_message = error_msg
      )
      
      overall_stats$failed <- overall_stats$failed + 1
    }
    # Delay between hospitals (not after last one)
    if (i < nrow(valid_hospitals)) {
      cat(sprintf("  Pausing %d seconds before next hospital...\n", API_DELAY_SECONDS))
      Sys.sleep(API_DELAY_SECONDS)
    }
  }
  
  overall_stats$end_time <- Sys.time()
  overall_stats$total_time <- difftime(overall_stats$end_time, 
                                       overall_stats$start_time, units = "mins")
  
  # =========================================================================
  # STEP 8: Save master output file
  # =========================================================================
  
  if (length(all_extractions) > 0) {
    cat("\n")
    cat(strrep("=", 80), "\n")
    cat("SAVING EXTRACTION OUTPUT\n")
    cat(strrep("=", 80), "\n")
    
    # Create master file
    master_content <- paste(all_extractions, collapse = "")
    
    # Add header
    run_date <- format(Sys.time(), "%Y%m%d")
    header <- paste0(
      "PHASE 3 - LEVEL 1 EXTRACTION\n",
      sprintf("Extraction Date: %s\n", format(Sys.Date(), "%Y-%m-%d")),
      sprintf("Hospitals Extracted: %d\n", overall_stats$successful),
      "Protocol Version: V4.1\n",
      "Extraction Mode: ", get_mode_description(), "\n",
      strrep("=", 60), "\n\n"
    )
    
    full_content <- paste0(header, master_content)
    
    # Generate filename
    master_filename <- sprintf("Phase3_L1_Extraction_%s.txt", run_date)
    master_filepath <- file.path(OUTPUT_DIRECTORY, master_filename)
    
    # Save file
    writeLines(full_content, master_filepath)
    
    cat(sprintf("  ✓ Master file saved: %s\n", master_filename))
    cat(sprintf("    Hospitals: %d\n", overall_stats$successful))
    cat(sprintf("    Size: %.1f KB\n", file.size(master_filepath) / 1024))
    
    if (CREATE_INDIVIDUAL_FILES) {
      cat(sprintf("  ✓ Individual files: %d saved to outputs/\n", length(individual_outputs)))
    }
  }
  
  # =========================================================================
  # STEP 9: Update YAML with extraction status
  # =========================================================================
  
  if (length(successful_facs) > 0) {
    cat("\n")
    cat(strrep("=", 80), "\n")
    cat("UPDATING YAML WITH EXTRACTION STATUS\n")
    cat(strrep("=", 80), "\n")
    
    update_yaml_after_extraction(successful_facs)
  }
  
  # =========================================================================
  # STEP 10: Final summary
  # =========================================================================
  cat("\n")
  cat(strrep("=", 80), "\n")
  cat("    EXTRACTION COMPLETE\n")
  cat(strrep("=", 80), "\n")
  cat("\n")
  
  print_usage_summary(
    overall_stats$total_input_tokens,
    overall_stats$total_output_tokens,
    overall_stats$total_cost
  )
  
  cat(sprintf("Total time: %.1f minutes\n", as.numeric(overall_stats$total_time)))
  cat(sprintf("Successful: %d hospitals\n", overall_stats$successful))
  cat(sprintf("Failed: %d hospitals\n", overall_stats$failed))
  cat("\n")
  
  print_progress_report()
  
  cat("\n✓ Output saved to:", OUTPUT_DIRECTORY, "\n")
  cat("✓ Progress tracked in:", PROGRESS_FILE, "\n")
  cat("✓ Logs saved in:", LOG_DIR, "\n\n")
  
  return(invisible(overall_stats))
}

# ============================================================================
# FILE SAVING FUNCTIONS
# ============================================================================

#' Save individual hospital extraction
save_individual_extraction <- function(extraction_text, fac_code, hospital_name) {
  
  # Build filename
  run_date <- format(Sys.Date(), "%Y%m%d")
  filename <- sprintf("Phase3_L1_%s_%s.txt", fac_code, run_date)
  
  # Create individual folder if doesn't exist
  individual_dir <- file.path(OUTPUT_DIRECTORY, "individual")
  if (!dir.exists(individual_dir)) {
    dir.create(individual_dir, recursive = TRUE)
  }
  
  filepath <- file.path(individual_dir, filename)
  
  writeLines(extraction_text, filepath)
  
  return(filepath)
}

#' Update YAML after successful extractions
update_yaml_after_extraction <- function(fac_codes) {
  
  if (length(fac_codes) == 0) {
    return(invisible(NULL))
  }
  
  tryCatch({
    
    # Read YAML
    yaml_data <- read_yaml(YAML_FILE)
    
    if (is.null(yaml_data$hospitals)) {
      hospitals <- yaml_data
    } else {
      hospitals <- yaml_data$hospitals
    }
    
    # Create backup
    backup_path <- paste0(YAML_FILE, ".backup_extraction_", 
                          format(Sys.time(), "%Y%m%d_%H%M%S"))
    file.copy(YAML_FILE, backup_path, overwrite = FALSE)
    cat(sprintf("  Backup created: %s\n", basename(backup_path)))
    
    # Update each successful hospital
    updated_count <- 0
    
    for (fac in fac_codes) {
      # Find hospital
      hospital_idx <- NULL
      for (j in seq_along(hospitals)) {
        if (sprintf("%03d", as.numeric(hospitals[[j]]$FAC)) == fac) {
          hospital_idx <- j
          break
        }
      }
      
      if (!is.null(hospital_idx)) {
        hospital <- hospitals[[hospital_idx]]
        
        if (is.null(hospital$strategy_search)) {
          hospital$strategy_search <- list()
        }
        
        # Mark as extracted
        # Mark as extracted
        hospital$strategy_search$baseline_extracted <- "yes"
        hospital$strategy_search$last_extraction_date <- as.character(Sys.Date())
        # Note: comparison_result is NOT reset here - it will be updated by next download run
        
        
        hospitals[[hospital_idx]] <- hospital
        updated_count <- updated_count + 1
      }
    }
    
    # Write YAML
    write_yaml(hospitals, YAML_FILE)
    
    cat(sprintf("  ✓ Marked %d hospitals as extracted\n", updated_count))
    
  }, error = function(e) {
    cat(sprintf("  ⚠ Error updating YAML: %s\n", e$message))
  })
  
  return(invisible(TRUE))
}
# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

#' Print token usage and cost summary
#' @param input_tokens Total input tokens used
#' @param output_tokens Total output tokens used
#' @param total_cost Total cost in USD
print_usage_summary <- function(input_tokens, output_tokens, total_cost) {
  cat("\n")
  cat("TOKEN USAGE SUMMARY\n")
  cat(strrep("-", 40), "\n")
  cat(sprintf("Input tokens:  %s\n", format(input_tokens, big.mark = ",")))
  cat(sprintf("Output tokens: %s\n", format(output_tokens, big.mark = ",")))
  cat(sprintf("Total tokens:  %s\n", format(input_tokens + output_tokens, big.mark = ",")))
  cat(sprintf("Total cost:    $%.4f\n", total_cost))
  cat(strrep("-", 40), "\n")
}
# ============================================================================
# CONVENIENCE FUNCTIONS
# ============================================================================

#' Run test extraction with specific hospitals
test_extraction <- function(filter_facs = c("592", "593", "597")) {
  run_extraction_workflow(test_mode = TRUE, filter_facs = filter_facs)
}

#' Run extraction with limit
run_limited_extraction <- function(max_hospitals) {
  run_extraction_workflow(max_hospitals = max_hospitals)
}

#' Run full extraction
run_full_extraction <- function() {
  run_extraction_workflow()
}

# ============================================================================
# INSTRUCTIONS
# ============================================================================

if (interactive()) {
  cat("\n")
  cat(strrep("=", 80), "\n")
  cat("     HOSPITAL EXTRACTION WORKFLOW V3.0 - SIMPLIFIED (NO BATCHING)\n")
  cat(strrep("=", 80), "\n")
  cat("\n")
  cat("QUICK START:\n\n")
  cat("1. TEST WITH SPECIFIC HOSPITALS:\n")
  cat("   test_extraction(filter_facs = c('592', '593', '597'))\n\n")
  cat("2. RUN LIMITED EXTRACTION:\n")
  cat("   run_limited_extraction(max_hospitals = 10)\n\n")
  cat("3. RUN FULL EXTRACTION:\n")
  cat("   run_full_extraction()\n\n")
  cat("EXTRACTION MODES (set in config_v2.R):\n")
  cat("  - baseline:      Extract all hospitals without baseline extraction\n")
  cat("  - changes_only:  Extract only hospitals flagged for re-extraction\n")
  cat("  - force_all:     Force re-extraction of all hospitals\n\n")
  cat("OUTPUT:\n")
  cat("  - Master file:       Phase3_L1_Extraction_YYYYMMDD.txt\n")
  cat("  - Individual files:  individual/Phase3_L1_FAC###_YYYYMMDD.txt\n")
  cat("  - Progress:          extraction_progress.csv\n")
  cat("  - Logs:              Various logs in outputs/logs/\n\n")
  cat(strrep("=", 80), "\n\n")
}
