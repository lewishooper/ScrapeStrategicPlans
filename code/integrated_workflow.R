# ============================================================================
# INTEGRATED EXTRACTION WORKFLOW WITH CHANGE DETECTION
# ============================================================================
# Purpose: Complete workflow combining:
#          1. Download strategic PDFs (with change detection)
#          2. Update YAML with extraction flags
#          3. Run API extraction (only on changed hospitals)
#          4. Mark extracted hospitals in YAML
# 
# Author: Skip & Claude
# Created: 2026-01-24
# Version: 1.0
# ============================================================================

# Source required modules
source("config_v2.R")
source("hospital_discovery.R")
source("yaml_updater.R")

# Also need download script functions
# source("download_strategic_pdfs.R")  # User will source this separately

# ============================================================================
# INTEGRATED WORKFLOW FUNCTIONS
# ============================================================================

#' Run complete workflow: Download → Update YAML → Extract
#' 
#' @param run_download Should download script be run? (default TRUE)
#' @param run_extraction Should API extraction be run? (default TRUE)
#' @param test_mode Test mode (1 batch only)
#' @param yaml_path Path to YAML file
#' @return Summary of workflow
run_integrated_workflow <- function(
    run_download = TRUE,
    run_extraction = TRUE,
    test_mode = FALSE,
    yaml_path = YAML_FILE
) {
  
  cat("\n")
  cat(strrep("=", 90), "\n")
  cat("INTEGRATED STRATEGIC PLAN EXTRACTION WORKFLOW\n")
  cat("Download → Compare → Update YAML → Extract → Mark Complete\n")
  cat(strrep("=", 90), "\n")
  cat("\n")
  
  workflow_stats <- list(
    download_attempted = run_download,
    download_completed = FALSE,
    yaml_updated = FALSE,
    extraction_attempted = run_extraction,
    extraction_completed = FALSE,
    hospitals_downloaded = 0,
    hospitals_changed = 0,
    hospitals_extracted = 0,
    total_cost = 0
  )
  
  # =========================================================================
  # PHASE 1: DOWNLOAD AND COMPARE
  # =========================================================================
  
  if (run_download) {
    cat("\n")
    cat(strrep("=", 90), "\n")
    cat("PHASE 1: DOWNLOAD STRATEGIC PLANS & DETECT CHANGES\n")
    cat(strrep("=", 90), "\n\n")
    
    cat("Please run download_strategic_pdfs.R separately\n")
    cat("Then provide the log_data to this function\n\n")
    cat("Example:\n")
    cat("  CONFIG$baseline_mode <- FALSE  # Change detection mode\n")
    cat("  results <- main()  # From download_strategic_pdfs.R\n")
    cat("  log_data <- results$log_data\n\n")
    
    # In practice, user would run download script and capture results
    # For now, skip this phase
    cat("⚠ Skipping download phase - run download_strategic_pdfs.R manually\n")
    
  }
  
  # =========================================================================
  # PHASE 2: UPDATE YAML WITH COMPARISON RESULTS
  # =========================================================================
  
  cat("\n")
  cat(strrep("=", 90), "\n")
  cat("PHASE 2: UPDATE YAML WITH EXTRACTION FLAGS\n")
  cat(strrep("=", 90), "\n\n")
  
  cat("To update YAML after download, call:\n")
  cat("  update_yaml_with_comparison_results(yaml_path, log_data)\n\n")
  cat("⚠ Skipping YAML update - requires log_data from download phase\n")
  
  # =========================================================================
  # PHASE 3: EXTRACTION STATUS CHECK
  # =========================================================================
  
  cat("\n")
  cat(strrep("=", 90), "\n")
  cat("PHASE 3: CHECK EXTRACTION STATUS\n")
  cat(strrep("=", 90), "\n")
  
  status <- get_extraction_status_summary(yaml_path)
  
  if (status$needs_extraction == 0) {
    cat("\n✓ All hospitals are up to date - no extraction needed!\n\n")
    return(invisible(workflow_stats))
  }
  
  cat(sprintf("\n%d hospitals need extraction\n", status$needs_extraction))
  
  # =========================================================================
  # PHASE 4: API EXTRACTION
  # =========================================================================
  
  if (run_extraction) {
    cat("\n")
    cat(strrep("=", 90), "\n")
    cat("PHASE 4: API EXTRACTION\n")
    cat(strrep("=", 90), "\n")
    
    # This would call the extraction workflow
    cat("\nTo run extraction, call:\n")
    cat("  source('extraction_workflow_v2.R')\n")
    cat("  results <- run_extraction_workflow(test_mode = ", test_mode, ")\n\n")
    
    cat("⚠ Extraction not run - call extraction_workflow_v2.R separately\n")
    
  }
  
  # =========================================================================
  # WORKFLOW SUMMARY
  # =========================================================================
  
  cat("\n")
  cat(strrep("=", 90), "\n")
  cat("INTEGRATED WORKFLOW SUMMARY\n")
  cat(strrep("=", 90), "\n")
  cat("\nThis is a template workflow. To run the complete process:\n\n")
  cat("1. Download & Compare:\n")
  cat("   source('download_strategic_pdfs.R')\n")
  cat("   CONFIG$baseline_mode <- FALSE  # Change detection\n")
  cat("   results <- main()\n\n")
  cat("2. Update YAML:\n")
  cat("   source('yaml_updater.R')\n")
  cat("   update_yaml_with_comparison_results(YAML_FILE, results$log_data)\n\n")
  cat("3. Extract Changed Hospitals:\n")
  cat("   source('extraction_workflow_v2.R')\n")
  cat("   CONFIG$EXTRACTION_MODE <- 'changes_only'\n")
  cat("   extraction_results <- run_extraction_workflow()\n\n")
  cat("4. Mark as Extracted:\n")
  cat("   # Happens automatically in extraction_workflow_v2.R\n\n")
  cat(strrep("=", 90), "\n\n")
  
  return(invisible(workflow_stats))
}

#' Helper function to update YAML after extraction batch completes
#' Called automatically by extraction_workflow_v2.R after successful extraction
update_yaml_after_extraction <- function(fac_codes, batch_number, yaml_path = YAML_FILE) {
  
  cat("\n")
  cat(strrep("-", 80), "\n")
  cat("UPDATING YAML AFTER SUCCESSFUL EXTRACTION\n")
  cat(strrep("-", 80), "\n")
  
  batch_mark_as_extracted(
    yaml_path = yaml_path,
    fac_codes = fac_codes,
    batch_number = batch_number,
    create_backup = TRUE
  )
  
  cat(strrep("-", 80), "\n\n")
  
  return(invisible(TRUE))
}

# ============================================================================
# MONTHLY MAINTENANCE WORKFLOW
# ============================================================================

#' Run monthly maintenance: Check for changes and extract only what's new
#' 
#' This is the workflow you'll run each month after initial baseline:
#' 1. Download all hospital PDFs
#' 2. Compare against previous versions (automatic in download script)
#' 3. Update YAML with changes
#' 4. Extract only changed hospitals
#' 5. Mark as extracted
#' 
#' @param test_mode Run in test mode (first batch only)
run_monthly_maintenance <- function(test_mode = FALSE) {
  
  cat("\n")
  cat(strrep("=", 90), "\n")
  cat("MONTHLY STRATEGIC PLAN MAINTENANCE WORKFLOW\n")
  cat(strrep("=", 90), "\n")
  cat("\n")
  cat("This workflow will:\n")
  cat("  1. Download all hospital strategic plans\n")
  cat("  2. Compare against previous versions\n")
  cat("  3. Flag changed hospitals for extraction\n")
  cat("  4. Extract only changed hospitals via API\n")
  cat("  5. Update tracking in YAML\n")
  cat("\n")
  
  if (interactive() && !test_mode) {
    response <- readline("Continue with monthly maintenance? (yes/no): ")
    if (tolower(trimws(response)) != "yes") {
      cat("Maintenance cancelled.\n")
      return(invisible(NULL))
    }
  }
  
  # Step 1: Download and compare
  cat("\n")
  cat(strrep("=", 90), "\n")
  cat("STEP 1: DOWNLOAD & COMPARE STRATEGIC PLANS\n")
  cat(strrep("=", 90), "\n")
  cat("\nPlease run separately:\n")
  cat("  source('download_strategic_pdfs.R')\n")
  cat("  CONFIG$baseline_mode <- FALSE  # CRITICAL: Change detection mode\n")
  cat("  download_results <- main()\n\n")
  
  readline("Press ENTER after download completes...")
  
  # Step 2: Update YAML
  cat("\n")
  cat(strrep("=", 90), "\n")
  cat("STEP 2: UPDATE YAML WITH CHANGES\n")
  cat(strrep("=", 90), "\n")
  cat("\nUpdating YAML with comparison results...\n")
  
  cat("\nPlease run:\n")
  cat("  update_yaml_with_comparison_results(YAML_FILE, download_results$log_data)\n\n")
  
  readline("Press ENTER after YAML update completes...")
  
  # Step 3: Check status
  cat("\n")
  cat(strrep("=", 90), "\n")
  cat("STEP 3: CHECK EXTRACTION STATUS\n")
  cat(strrep("=", 90), "\n")
  
  status <- get_extraction_status_summary(YAML_FILE)
  
  if (status$needs_extraction == 0) {
    cat("\n✓ No changes detected - all hospitals up to date!\n")
    cat("Monthly maintenance complete - no extraction needed.\n\n")
    return(invisible(NULL))
  }
  
  cat(sprintf("\n%d hospitals have changes and need extraction\n", status$needs_extraction))
  estimated_cost <- status$needs_extraction * 0.10
  cat(sprintf("Estimated cost: $%.2f - $%.2f\n", estimated_cost * 0.7, estimated_cost * 1.3))
  
  if (interactive()) {
    response <- readline("Continue with extraction? (yes/no): ")
    if (tolower(trimws(response)) != "yes") {
      cat("Extraction cancelled.\n")
      return(invisible(NULL))
    }
  }
  
  # Step 4: Extract changed hospitals
  cat("\n")
  cat(strrep("=", 90), "\n")
  cat("STEP 4: EXTRACT CHANGED HOSPITALS\n")
  cat(strrep("=", 90), "\n")
  cat("\nPlease run:\n")
  cat("  source('extraction_workflow_v2.R')\n")
  cat("  # Ensure EXTRACTION_MODE is 'changes_only' in config_v2.R\n")
  cat("  extraction_results <- run_extraction_workflow(test_mode = ", test_mode, ")\n\n")
  
  cat("✓ Monthly maintenance workflow complete!\n\n")
  
  return(invisible(status))
}

# ============================================================================
# BASELINE EXTRACTION WORKFLOW  
# ============================================================================

#' Run initial baseline extraction for hospitals that have never been extracted
#' 
#' Use this for your February 2026 run to complete baseline
#' 
#' @param test_mode Test with specific FACs first
#' @param test_facs FAC codes to test with (optional)
run_baseline_extraction <- function(test_mode = FALSE, test_facs = NULL) {
  
  cat("\n")
  cat(strrep("=", 90), "\n")
  cat("BASELINE EXTRACTION WORKFLOW\n")
  cat(strrep("=", 90), "\n")
  cat("\n")
  cat("This will extract all hospitals that have not been baseline-extracted.\n")
  cat("\n")
  
  # Check status
  status <- get_extraction_status_summary(YAML_FILE)
  
  hospitals_needing_baseline <- status$total - status$baseline_extracted
  
  cat(sprintf("Hospitals needing baseline extraction: %d\n", hospitals_needing_baseline))
  
  if (hospitals_needing_baseline == 0) {
    cat("\n✓ Baseline extraction already complete!\n\n")
    return(invisible(NULL))
  }
  
  estimated_cost <- hospitals_needing_baseline * 0.10
  cat(sprintf("Estimated cost: $%.2f - $%.2f\n", estimated_cost * 0.7, estimated_cost * 1.3))
  cat(sprintf("Estimated time: %d - %d minutes\n\n", 
              ceiling(hospitals_needing_baseline * 1.5),
              ceiling(hospitals_needing_baseline * 3)))
  
  if (interactive() && !test_mode) {
    response <- readline("Continue with baseline extraction? (yes/no): ")
    if (tolower(trimws(response)) != "yes") {
      cat("Baseline extraction cancelled.\n")
      return(invisible(NULL))
    }
  }
  
  cat("\nTo run baseline extraction:\n")
  cat("  source('extraction_workflow_v2.R')\n")
  cat("  # Ensure EXTRACTION_MODE is 'baseline' in config_v2.R\n")
  
  if (!is.null(test_facs)) {
    cat("  extraction_results <- test_extraction(filter_facs = c(")
    cat(paste0("'", test_facs, "'", collapse = ", "))
    cat("))\n\n")
  } else if (test_mode) {
    cat("  extraction_results <- test_extraction()\n\n")
  } else {
    cat("  extraction_results <- run_full_extraction()\n\n")
  }
  
  return(invisible(status))
}

# ============================================================================
# QUICK STATUS CHECK
# ============================================================================

#' Quick status check across all systems
check_system_status <- function() {
  
  cat("\n")
  cat(strrep("=", 90), "\n")
  cat("SYSTEM STATUS CHECK\n")
  cat(strrep("=", 90), "\n")
  cat("\n")
  
  # Check YAML
  cat("1. YAML FILE STATUS:\n")
  cat(strrep("-", 90), "\n")
  status <- get_extraction_status_summary(YAML_FILE)
  
  # Check progress file
  cat("\n2. EXTRACTION PROGRESS:\n")
  cat(strrep("-", 90), "\n")
  if (file.exists(PROGRESS_FILE)) {
    progress <- read.csv(PROGRESS_FILE, stringsAsFactors = FALSE)
    completed <- sum(progress$status %in% c("completed", "success"))
    failed <- sum(progress$status == "failed")
    cat(sprintf("Total entries:     %d\n", nrow(progress)))
    cat(sprintf("Completed:         %d\n", completed))
    cat(sprintf("Failed:            %d\n", failed))
    
    if (nrow(progress) > 0) {
      latest <- progress[nrow(progress), ]
      cat(sprintf("Latest extraction: FAC %s (%s)\n", latest$fac_code, latest$timestamp))
    }
  } else {
    cat("No progress file found - no extractions have been run\n")
  }
  
  # Check outputs
  cat("\n3. OUTPUT FILES:\n")
  cat(strrep("-", 90), "\n")
  if (dir.exists(OUTPUT_DIRECTORY)) {
    batch_files <- list.files(OUTPUT_DIRECTORY, pattern = "Phase3_Batch.*\\.txt$")
    individual_files <- list.files(OUTPUT_DIRECTORY, pattern = "Phase3_L1_.*\\.txt$")
    cat(sprintf("Batch files:       %d\n", length(batch_files)))
    cat(sprintf("Individual files:  %d\n", length(individual_files)))
  } else {
    cat("Output directory not found\n")
  }
  
  # Summary
  cat("\n")
  cat(strrep("=", 90), "\n")
  cat("SUMMARY\n")
  cat(strrep("=", 90), "\n")
  
  completion_pct <- round(100 * status$baseline_extracted / status$total, 1)
  cat(sprintf("Baseline completion: %.1f%% (%d / %d hospitals)\n", 
              completion_pct, status$baseline_extracted, status$total))
  
  if (status$needs_extraction > 0) {
    cat(sprintf("\n⚠ %d hospitals need extraction\n", status$needs_extraction))
    cat("  Run: run_baseline_extraction() or run_monthly_maintenance()\n")
  } else {
    cat("\n✓ All hospitals up to date!\n")
  }
  
  cat(strrep("=", 90), "\n\n")
  
  return(invisible(status))
}

# ============================================================================
# INSTRUCTIONS
# ============================================================================

if (interactive()) {
  cat("\n")
  cat(strrep("=", 90), "\n")
  cat("INTEGRATED EXTRACTION WORKFLOW - QUICK START\n")
  cat(strrep("=", 90), "\n")
  cat("\n")
  cat("WORKFLOWS AVAILABLE:\n\n")
  cat("1. CHECK SYSTEM STATUS:\n")
  cat("   check_system_status()\n\n")
  cat("2. RUN BASELINE EXTRACTION (February 2026):\n")
  cat("   run_baseline_extraction(test_mode = TRUE)  # Test first\n")
  cat("   run_baseline_extraction()  # Full run\n\n")
  cat("3. RUN MONTHLY MAINTENANCE (After baseline):\n")
  cat("   run_monthly_maintenance(test_mode = TRUE)  # Test first\n")
  cat("   run_monthly_maintenance()  # Full run\n\n")
  cat("4. MANUAL WORKFLOW:\n")
  cat("   See run_integrated_workflow() for step-by-step instructions\n\n")
  cat(strrep("=", 90), "\n\n")
}
