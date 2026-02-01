# End-to-End Single Hospital Test
# Purpose: Test the complete PDF identification and download workflow on one hospital
# Created: 2025-12-03

library(yaml)
library(dplyr)

# Configuration - UPDATE THESE
TEST_FAC <- "597"  # ALMONTE GENERAL - change this to test different hospitals
BASE_DIR <- "E:/Hospital_Strategic_Plans"
WORKING_YAML <- "code/Hospital_strategy.yaml"
SCRIPT_PATH <- "code/identifyAndDownloadStrategicPDF.R"

# Create results directory if it doesn't exist
RESULTS_DIR <- file.path(BASE_DIR, "test_results")
if (!dir.exists(RESULTS_DIR)) {
  dir.create(RESULTS_DIR, recursive = TRUE)
}

# Function to extract one hospital's data
extract_single_hospital <- function(fac) {
  yaml_path <- file.path(BASE_DIR, WORKING_YAML)
  
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("EXTRACTING HOSPITAL DATA\n")
  cat(strrep("=", 70), "\n")
  
  if (!file.exists(yaml_path)) {
    stop("YAML file not found: ", yaml_path)
  }
  
  data <- read_yaml(yaml_path)
  
  # Find the specific hospital
  hospital_index <- which(sapply(data, function(h) h$FAC == fac))
  
  if (length(hospital_index) == 0) {
    stop("Hospital with FAC ", fac, " not found in YAML")
  }
  
  hospital <- data[[hospital_index]]
  
  cat("FAC:", hospital$FAC, "\n")
  cat("Name:", hospital$name, "\n")
  cat("Base URL:", hospital$base_url, "\n")
  
  # Show current strategy_search status
  if (!is.null(hospital$strategy_search)) {
    cat("\n=== CURRENT STATUS ===\n")
    cat("Search attempted:", hospital$strategy_search$search_attempted %||% "FALSE", "\n")
    cat("Strategy URL found:", hospital$strategy_search$strategy_url_found %||% "FALSE", "\n")
    cat("PDF found:", hospital$strategy_search$pdf_found %||% "FALSE", "\n")
    cat("PDF downloaded:", hospital$strategy_search$pdf_downloaded %||% "FALSE", "\n")
    if (!is.null(hospital$strategy_search$strategy_notes) && hospital$strategy_search$strategy_notes != "") {
      cat("Notes:", hospital$strategy_search$strategy_notes, "\n")
    }
  } else {
    cat("\n=== CURRENT STATUS ===\n")
    cat("No strategy_search data yet\n")
  }
  
  return(list(
    hospital = hospital,
    index = hospital_index,
    full_data = data
  ))
}

# Function to create a temporary YAML with just one hospital
create_temp_yaml <- function(hospital, temp_path) {
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("CREATING TEMPORARY YAML\n")
  cat(strrep("=", 70), "\n")
  cat("Temp file:", temp_path, "\n")
  
  # Write single hospital to temp file
  write_yaml(list(hospital), temp_path)
  
  cat("✓ Temporary YAML created\n")
  
  return(temp_path)
}

# Function to run the PDF identification script
run_pdf_identification <- function(temp_yaml_path) {
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("RUNNING PDF IDENTIFICATION SCRIPT\n")
  cat(strrep("=", 70), "\n")
  
  script_full_path <- file.path(BASE_DIR, SCRIPT_PATH)
  
  if (!file.exists(script_full_path)) {
    stop("Script not found: ", script_full_path)
  }
  
  cat("Script:", script_full_path, "\n")
  cat("Input YAML:", temp_yaml_path, "\n")
  cat("\n--- BEGIN SCRIPT OUTPUT ---\n\n")
  
  # Source the script in a new environment
  script_env <- new.env()
  
  # Set working directory to BASE_DIR so relative paths work
  old_wd <- getwd()
  setwd(BASE_DIR)
  
  # Try to source and run
  tryCatch({
    # Source the script
    source(script_full_path, local = script_env)
    
    # The script should define a main function - try common names
    if (exists("process_hospitals", envir = script_env)) {
      script_env$process_hospitals(temp_yaml_path)
    } else if (exists("main", envir = script_env)) {
      script_env$main(temp_yaml_path)
    } else if (exists("identify_and_download", envir = script_env)) {
      script_env$identify_and_download(temp_yaml_path)
    } else {
      # If no function found, the script may run on source
      cat("Script executed on source\n")
    }
    
  }, error = function(e) {
    setwd(old_wd)
    cat("\n✗ ERROR during script execution:\n")
    cat(conditionMessage(e), "\n")
    stop("Script execution failed")
  })
  
  setwd(old_wd)
  
  cat("\n--- END SCRIPT OUTPUT ---\n")
  
  return(TRUE)
}

# Function to read results and compare
compare_results <- function(original_hospital, temp_yaml_path, fac) {
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("COMPARING RESULTS\n")
  cat(strrep("=", 70), "\n")
  
  # Read the updated temp YAML
  updated_data <- read_yaml(temp_yaml_path)
  updated_hospital <- updated_data[[1]]
  
  cat("\n=== ORIGINAL STATE ===\n")
  if (!is.null(original_hospital$strategy_search)) {
    cat("Search attempted:", original_hospital$strategy_search$search_attempted %||% "FALSE", "\n")
    cat("Strategy URL found:", original_hospital$strategy_search$strategy_url_found %||% "FALSE", "\n")
    cat("PDF found:", original_hospital$strategy_search$pdf_found %||% "FALSE", "\n")
    cat("PDF downloaded:", original_hospital$strategy_search$pdf_downloaded %||% "FALSE", "\n")
  } else {
    cat("No strategy_search data\n")
  }
  
  cat("\n=== UPDATED STATE ===\n")
  if (!is.null(updated_hospital$strategy_search)) {
    cat("Search attempted:", updated_hospital$strategy_search$search_attempted %||% "FALSE", "\n")
    cat("Strategy URL found:", updated_hospital$strategy_search$strategy_url_found %||% "FALSE", "\n")
    cat("Strategy URL:", updated_hospital$strategy_search$strategy_url %||% "NONE", "\n")
    cat("PDF found:", updated_hospital$strategy_search$pdf_found %||% "FALSE", "\n")
    cat("PDF URL:", updated_hospital$strategy_search$pdf_url %||% "NONE", "\n")
    cat("PDF downloaded:", updated_hospital$strategy_search$pdf_downloaded %||% "FALSE", "\n")
    cat("Download confidence:", updated_hospital$strategy_search$download_confidence %||% "NONE", "\n")
    cat("Local folder:", updated_hospital$strategy_search$local_folder %||% "NONE", "\n")
    cat("Local filename:", updated_hospital$strategy_search$local_filename %||% "NONE", "\n")
    if (!is.null(updated_hospital$strategy_search$strategy_notes) && updated_hospital$strategy_search$strategy_notes != "") {
      cat("Notes:", updated_hospital$strategy_search$strategy_notes, "\n")
    }
    cat("Requires review:", updated_hospital$strategy_search$requires_manual_review %||% "FALSE", "\n")
  } else {
    cat("No strategy_search data (UNEXPECTED!)\n")
  }
  
  # Check if PDF file actually exists
  if (!is.null(updated_hospital$strategy_search) && 
      updated_hospital$strategy_search$pdf_downloaded == TRUE) {
    
    local_folder <- updated_hospital$strategy_search$local_folder
    local_filename <- updated_hospital$strategy_search$local_filename
    
    if (!is.null(local_folder) && !is.null(local_filename)) {
      pdf_path <- file.path(BASE_DIR, local_folder, local_filename)
      
      cat("\n=== PDF FILE CHECK ===\n")
      cat("Expected path:", pdf_path, "\n")
      
      if (file.exists(pdf_path)) {
        file_info <- file.info(pdf_path)
        cat("✓ PDF file exists\n")
        cat("  Size:", round(file_info$size / 1024, 2), "KB\n")
        cat("  Modified:", format(file_info$mtime), "\n")
      } else {
        cat("✗ PDF file NOT FOUND at expected location\n")
      }
    }
  }
  
  return(updated_hospital)
}

# Function to optionally merge results back to main YAML
merge_to_main_yaml <- function(updated_hospital, fac, create_backup = TRUE) {
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("MERGE TO MAIN YAML?\n")
  cat(strrep("=", 70), "\n")
  
  response <- readline("Merge results to main Hospital_strategy.yaml? (yes/no): ")
  
  if (tolower(response) != "yes") {
    cat("Merge cancelled. Results remain in temporary file only.\n")
    return(FALSE)
  }
  
  yaml_path <- file.path(BASE_DIR, WORKING_YAML)
  
  # Create backup if requested
  if (create_backup) {
    backup_dir <- file.path(BASE_DIR, "Backups")
    if (!dir.exists(backup_dir)) {
      dir.create(backup_dir, recursive = TRUE)
    }
    
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    backup_path <- file.path(backup_dir, paste0("pre_test_merge_", timestamp, ".yaml"))
    file.copy(yaml_path, backup_path)
    cat("✓ Backup created:", backup_path, "\n")
  }
  
  # Read main YAML
  main_data <- read_yaml(yaml_path)
  
  # Find and replace hospital
  hospital_index <- which(sapply(main_data, function(h) h$FAC == fac))
  
  if (length(hospital_index) == 0) {
    stop("Hospital with FAC ", fac, " not found in main YAML")
  }
  
  # Merge strategy_search data
  main_data[[hospital_index]]$strategy_search <- updated_hospital$strategy_search
  
  # Write back
  write_yaml(main_data, yaml_path)
  
  cat("✓ Results merged to main YAML\n")
  cat("✓ File saved:", yaml_path, "\n")
  
  return(TRUE)
}

# Main test execution function
run_end_to_end_test <- function(fac = TEST_FAC, merge_results = FALSE) {
  
  cat("\n")
  cat(strrep("=", 80), "\n")
  cat("END-TO-END SINGLE HOSPITAL TEST\n")
  cat(strrep("=", 80), "\n")
  cat("Testing FAC:", fac, "\n")
  cat("Started:", format(Sys.time()), "\n")
  cat(strrep("=", 80), "\n")
  
  # Step 1: Extract hospital data
  extraction <- extract_single_hospital(fac)
  
  # Step 2: Create temp YAML
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  temp_yaml <- file.path(RESULTS_DIR, paste0("temp_hospital_", fac, "_", timestamp, ".yaml"))
  create_temp_yaml(extraction$hospital, temp_yaml)
  
  # Step 3: Run PDF identification script
  tryCatch({
    run_pdf_identification(temp_yaml)
  }, error = function(e) {
    cat("\n✗ Script execution failed. Check output above.\n")
    return(NULL)
  })
  
  # Step 4: Compare results
  updated_hospital <- compare_results(extraction$hospital, temp_yaml, fac)
  
  # Step 5: Optional merge
  if (merge_results) {
    merged <- merge_to_main_yaml(updated_hospital, fac)
  } else {
    cat("\n")
    cat("Results saved to temporary file:\n")
    cat(temp_yaml, "\n")
    cat("\nTo merge results to main YAML later, run:\n")
    cat("  merge_to_main_yaml(updated_hospital, '", fac, "')\n", sep="")
  }
  
  cat("\n")
  cat(strrep("=", 80), "\n")
  cat("TEST COMPLETE\n")
  cat("Finished:", format(Sys.time()), "\n")
  cat(strrep("=", 80), "\n")
  
  return(list(
    original = extraction$hospital,
    updated = updated_hospital,
    temp_yaml = temp_yaml
  ))
}

# Quick test wrapper
test_hospital <- function(fac = TEST_FAC) {
  run_end_to_end_test(fac, merge_results = FALSE)
}

# Usage instructions
cat("\n=== END-TO-END SINGLE HOSPITAL TEST ===\n")
cat("Configured for FAC:", TEST_FAC, "\n")
cat("\nUsage:\n")
cat("  # Run test on configured hospital (", TEST_FAC, "):\n", sep="")
cat("  result <- test_hospital()\n")
cat("\n  # Run test on different hospital:\n")
cat("  result <- test_hospital('599')  # ARNPRIOR\n")
cat("\n  # Run test and auto-merge results:\n")
cat("  result <- run_end_to_end_test('597', merge_results = TRUE)\n")