# ============================================================================
# PROGRESS TRACKING FOR HOSPITAL EXTRACTION
# ============================================================================
# Functions for tracking extraction progress and logging results
# ============================================================================

# ----------------------------------------------------------------------------
# INITIALIZE PROGRESS FILE
# ----------------------------------------------------------------------------

initialize_progress_file <- function(progress_file = PROGRESS_FILE) {
  
  if (!file.exists(progress_file)) {
    # Create new progress file
    progress <- data.frame(
      fac_code = character(),
      hospital_name = character(),
      batch_number = integer(),
      status = character(),
      timestamp = character(),
      input_tokens = integer(),
      output_tokens = integer(),
      cost = numeric(),
      error_message = character(),
      stringsAsFactors = FALSE
    )
    
    write.csv(progress, progress_file, row.names = FALSE)
    message("Created progress file: ", progress_file)
  } else {
    message("Using existing progress file: ", progress_file)
  }
}

# ----------------------------------------------------------------------------
# UPDATE PROGRESS FOR A HOSPITAL
# ----------------------------------------------------------------------------

update_progress <- function(fac_code, 
                           hospital_name,
                           batch_number,
                           status,  # "completed", "failed", "skipped"
                           input_tokens = 0,
                           output_tokens = 0,
                           cost = 0,
                           error_message = "",
                           progress_file = PROGRESS_FILE) {
  
  # Read existing progress
  if (file.exists(progress_file)) {
    progress <- read.csv(progress_file, stringsAsFactors = FALSE)
  } else {
    progress <- data.frame(
      fac_code = character(),
      hospital_name = character(),
      batch_number = integer(),
      status = character(),
      timestamp = character(),
      input_tokens = integer(),
      output_tokens = integer(),
      cost = numeric(),
      error_message = character(),
      stringsAsFactors = FALSE
    )
  }
  
  # Create new entry
  new_entry <- data.frame(
    fac_code = fac_code,
    hospital_name = hospital_name,
    batch_number = batch_number,
    status = status,
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    input_tokens = input_tokens,
    output_tokens = output_tokens,
    cost = cost,
    error_message = error_message,
    stringsAsFactors = FALSE
  )
  
  # Remove any existing entry for this FAC code
  progress <- progress[progress$fac_code != fac_code, ]
  
  # Add new entry
  progress <- rbind(progress, new_entry)
  
  # Save updated progress
  write.csv(progress, progress_file, row.names = FALSE)
}

# ----------------------------------------------------------------------------
# GET PROGRESS SUMMARY
# ----------------------------------------------------------------------------

get_progress_summary <- function(progress_file = PROGRESS_FILE) {
  
  if (!file.exists(progress_file)) {
    return(list(
      completed = 0,
      failed = 0,
      total_cost = 0,
      total_tokens = 0
    ))
  }
  
  progress <- read.csv(progress_file, stringsAsFactors = FALSE)
  
  summary <- list(
    completed = sum(progress$status == "completed"),
    failed = sum(progress$status == "failed"),
    skipped = sum(progress$status == "skipped"),
    total_cost = sum(progress$cost, na.rm = TRUE),
    total_input_tokens = sum(progress$input_tokens, na.rm = TRUE),
    total_output_tokens = sum(progress$output_tokens, na.rm = TRUE),
    total_tokens = sum(progress$input_tokens, na.rm = TRUE) + 
                   sum(progress$output_tokens, na.rm = TRUE)
  )
  
  return(summary)
}

# ----------------------------------------------------------------------------
# PRINT PROGRESS REPORT
# ----------------------------------------------------------------------------

print_progress_report <- function() {
  cat("\n========================================\n")
  cat("EXTRACTION PROGRESS REPORT\n")
  cat("========================================\n")
  
  summary <- get_progress_summary()
  
  cat(sprintf("Completed: %d hospitals\n", summary$completed))
  cat(sprintf("Failed:    %d hospitals\n", summary$failed))
  
  if (summary$skipped > 0) {
    cat(sprintf("Skipped:   %d hospitals\n", summary$skipped))
  }
  
  cat(sprintf("\nTotal tokens: %s (%s input + %s output)\n",
             format(summary$total_tokens, big.mark = ","),
             format(summary$total_input_tokens, big.mark = ","),
             format(summary$total_output_tokens, big.mark = ",")))
  
  cat(sprintf("Total cost:   $%.4f\n", summary$total_cost))
  
  cat("========================================\n\n")
}

# ----------------------------------------------------------------------------
# SAVE BATCH OUTPUT FILE
# ----------------------------------------------------------------------------

save_batch_file <- function(batch_content, 
                           batch_number,
                           n_hospitals,
                           output_dir = OUTPUT_DIRECTORY) {
  
  # Create filename
  filename <- sprintf("Phase3_Batch%d_L1_Extraction.txt", batch_number)
  filepath <- file.path(output_dir, filename)
  
  # Build complete batch file with header and footer
  batch_file <- sprintf("PHASE 3 - LEVEL 1 EXTRACTION
BATCH %d - %d HOSPITALS
Extraction Date: %s
Protocol Version: V4.1

=====================================
=====================================

%s

=====================================
END OF BATCH %d
=====================================
Total Hospitals Extracted: %d
Protocol Version: V4.1
Extraction Complete: %s
",
    batch_number,
    n_hospitals,
    EXTRACTION_DATE,
    batch_content,
    batch_number,
    n_hospitals,
    EXTRACTION_DATE
  )
  
  # Write file
  writeLines(batch_file, filepath)
  
  message(sprintf("\n✓ Saved batch file: %s", filename))
  
  return(filepath)
}

# ----------------------------------------------------------------------------
# CREATE ERROR LOG ENTRY
# ----------------------------------------------------------------------------

log_error <- function(fac_code, hospital_name, batch_number, error_message) {
  # Handle NULL or missing parameters
  if (is.null(fac_code)) fac_code <- "unknown"
  if (is.null(hospital_name)) hospital_name <- "unknown"
  if (is.null(batch_number)) batch_number <- NA
  if (is.null(error_message)) error_message <- "Unknown error"
  
  # Ensure all are length 1
  fac_code <- as.character(fac_code)[1]
  hospital_name <- as.character(hospital_name)[1]
  batch_number <- as.numeric(batch_number)[1]
  error_message <- as.character(error_message)[1]
  
  error_record <- data.frame(
    fac_code = fac_code,
    hospital_name = hospital_name,
    batch_number = batch_number,
    error_message = error_message,
    timestamp = Sys.time(),
    stringsAsFactors = FALSE
  )
  
  # ... rest of your save logic
}

# ----------------------------------------------------------------------------
# BACKUP EXISTING FILES
# ----------------------------------------------------------------------------

backup_output_files <- function(output_dir = OUTPUT_DIRECTORY) {
  
  backup_dir <- file.path(output_dir, "backups", format(Sys.time(), "%Y%m%d_%H%M%S"))
  
  # Get existing output files
  existing_files <- list.files(output_dir, pattern = "^Phase3_Batch.*\\.txt$", full.names = TRUE)
  
  if (length(existing_files) > 0) {
    dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)
    
    file.copy(existing_files, backup_dir)
    
    message(sprintf("Backed up %d existing batch files to: %s", 
                   length(existing_files), backup_dir))
  }
}
