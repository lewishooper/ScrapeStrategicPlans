# Copy Strategic Plan PDFs to Processed Folder
# Purpose: Consolidate most recent PDFs with standardized naming
# Author: Hospital Strategic Plans Project
# Date: 2025-12-10
# 
# Output Format: {FAC}_{Hospital_Name}.pdf
# Selection Logic: Most recent by file modification date, then largest if tie
# Reporting: Comprehensive flags for manual review, text files, corrupt files

library(dplyr)
library(stringr)

# Configuration ----
BASE_DIR <- "E:/Hospital_Strategic_Plans"
SOURCE_DIR <- file.path(BASE_DIR, "strategic_plans")
DEST_DIR <- file.path(BASE_DIR, "Processed")
OUTPUT_DIR <- file.path(BASE_DIR, "Outputs")

# File size threshold for flagging corrupt files (1KB)
MIN_FILE_SIZE <- 1024

# Create directories if they don't exist
if (!dir.exists(DEST_DIR)) {
  dir.create(DEST_DIR, recursive = TRUE)
  cat("Created destination directory:", DEST_DIR, "\n")
}

if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}

# Helper Functions ----

# Parse folder name to extract FAC and Hospital_Name
parse_folder_name <- function(folder_name) {
  # Expected pattern: {FAC}_{Hospital_Name}
  # Example: 592_NAPANEE_LENNOX___ADDINGTON
  
  parts <- str_split(folder_name, "_", n = 2)[[1]]
  
  if (length(parts) < 2) {
    return(list(valid = FALSE, fac = NA, hospital_name = NA, error = "Malformed folder name"))
  }
  
  fac <- parts[1]
  hospital_name <- parts[2]
  
  # Clean hospital name - remove multiple underscores
  hospital_name <- str_replace_all(hospital_name, "_{2,}", "_")
  hospital_name <- str_trim(hospital_name)
  
  # Validate FAC is numeric
  if (!grepl("^[0-9]+$", fac)) {
    return(list(valid = FALSE, fac = fac, hospital_name = hospital_name, 
                error = "FAC is not numeric"))
  }
  
  return(list(valid = TRUE, fac = fac, hospital_name = hospital_name, error = NA))
}

# Get most recent PDF from a list of files based on modification date
get_most_recent_pdf <- function(pdf_files) {
  if (length(pdf_files) == 0) {
    return(list(file = NA, mod_date = NA, size = NA, count = 0))
  }
  
  # Get file info
  file_info <- data.frame(
    path = pdf_files,
    mod_date = file.mtime(pdf_files),
    size = file.size(pdf_files),
    stringsAsFactors = FALSE
  )
  
  # Sort by modification date (newest first), then by size (largest first)
  file_info <- file_info %>%
    arrange(desc(mod_date), desc(size))
  
  # Check for ties in modification date
  most_recent_date <- file_info$mod_date[1]
  ties <- sum(file_info$mod_date == most_recent_date)
  
  return(list(
    file = file_info$path[1],
    mod_date = file_info$mod_date[1],
    size = file_info$size[1],
    count = nrow(file_info),
    has_tie = ties > 1
  ))
}

# Main Function ----
copy_strategic_pdfs <- function() {
  cat("\n", strrep("=", 70), "\n")
  cat("Copy Strategic Plan PDFs to Processed Folder\n")
  cat("Output Format: {FAC}_{Hospital_Name}.pdf\n")
  cat(strrep("=", 70), "\n\n")
  
  cat("Source:", SOURCE_DIR, "\n")
  cat("Destination:", DEST_DIR, "\n")
  cat("Reports:", OUTPUT_DIR, "\n\n")
  
  # Get all subdirectories in strategic_plans
  subdirs <- list.dirs(SOURCE_DIR, full.names = TRUE, recursive = FALSE)
  
  if (length(subdirs) == 0) {
    cat("No subdirectories found in strategic_plans folder.\n")
    return(invisible(NULL))
  }
  
  cat("Found", length(subdirs), "hospital folder(s)\n\n")
  
  # Initialize tracking
  results <- list()
  copied <- 0
  updated <- 0
  skipped <- 0
  errors <- 0
  
  # Tracking for reports
  malformed_folders <- data.frame()
  text_files_found <- data.frame()
  corrupt_files <- data.frame()
  no_pdfs <- data.frame()
  duplicate_facs <- data.frame()
  multiple_pdfs <- data.frame()
  
  # Track FACs we've seen to detect duplicates
  seen_facs <- character()
  
  # Process each subdirectory
  for (subdir in subdirs) {
    folder_name <- basename(subdir)
    cat("[PROCESS]", folder_name, "\n")
    
    # Parse folder name
    parsed <- parse_folder_name(folder_name)
    
    if (!parsed$valid) {
      cat("  [ERROR] Malformed folder name:", parsed$error, "\n")
      malformed_folders <- rbind(malformed_folders, data.frame(
        folder = folder_name,
        error = parsed$error,
        stringsAsFactors = FALSE
      ))
      errors <- errors + 1
      next
    }
    
    fac <- parsed$fac
    hospital_name <- parsed$hospital_name
    
    # Check for duplicate FAC
    if (fac %in% seen_facs) {
      cat("  [WARNING] Duplicate FAC detected:", fac, "\n")
      duplicate_facs <- rbind(duplicate_facs, data.frame(
        FAC = fac,
        folder = folder_name,
        stringsAsFactors = FALSE
      ))
    }
    seen_facs <- c(seen_facs, fac)
    
    # Check for text files
    txt_files <- list.files(subdir, pattern = "\\.txt$", 
                            full.names = FALSE, ignore.case = TRUE)
    if (length(txt_files) > 0) {
      cat("  [TEXT] Found", length(txt_files), "text file(s)\n")
      text_files_found <- rbind(text_files_found, data.frame(
        FAC = fac,
        hospital_name = hospital_name,
        folder = folder_name,
        txt_count = length(txt_files),
        txt_files = paste(txt_files, collapse = "; "),
        stringsAsFactors = FALSE
      ))
    }
    
    # Find all PDFs
    pdf_files <- list.files(subdir, pattern = "\\.pdf$", 
                            full.names = TRUE, ignore.case = TRUE)
    
    if (length(pdf_files) == 0) {
      cat("  [SKIP] No PDFs found\n")
      no_pdfs <- rbind(no_pdfs, data.frame(
        FAC = fac,
        hospital_name = hospital_name,
        folder = folder_name,
        stringsAsFactors = FALSE
      ))
      skipped <- skipped + 1
      next
    }
    
    cat("  [FOUND]", length(pdf_files), "PDF(s)\n")
    
    # Flag if multiple PDFs
    if (length(pdf_files) > 1) {
      multiple_pdfs <- rbind(multiple_pdfs, data.frame(
        FAC = fac,
        hospital_name = hospital_name,
        folder = folder_name,
        pdf_count = length(pdf_files),
        stringsAsFactors = FALSE
      ))
    }
    
    # Get most recent PDF
    most_recent <- get_most_recent_pdf(pdf_files)
    selected_pdf <- most_recent$file
    
    cat("  [SELECT]", basename(selected_pdf), "\n")
    cat("    Mod date:", format(most_recent$mod_date, "%Y-%m-%d %H:%M:%S"), "\n")
    cat("    Size:", round(most_recent$size / 1024, 1), "KB\n")
    
    if (most_recent$has_tie) {
      cat("    [NOTE] Multiple PDFs with same modification date - selected largest\n")
    }
    
    # Check file size
    if (most_recent$size < MIN_FILE_SIZE) {
      cat("  [WARNING] File size < 1KB - potentially corrupt\n")
      corrupt_files <- rbind(corrupt_files, data.frame(
        FAC = fac,
        hospital_name = hospital_name,
        folder = folder_name,
        filename = basename(selected_pdf),
        size_bytes = most_recent$size,
        stringsAsFactors = FALSE
      ))
      errors <- errors + 1
      next
    }
    
    # Construct destination filename
    dest_filename <- paste0(fac, "_", hospital_name, ".pdf")
    dest_path <- file.path(DEST_DIR, dest_filename)
    
    # Check if destination exists
    if (file.exists(dest_path)) {
      dest_mod_date <- file.mtime(dest_path)
      
      if (most_recent$mod_date > dest_mod_date) {
        cat("  [UPDATE] Destination exists but source is newer - overwriting\n")
        success <- file.copy(selected_pdf, dest_path, overwrite = TRUE)
        
        if (success) {
          cat("  [SUCCESS] Updated:", dest_filename, "\n")
          updated <- updated + 1
        } else {
          cat("  [ERROR] Copy failed\n")
          errors <- errors + 1
        }
      } else {
        cat("  [SKIP] Destination is current\n")
        skipped <- skipped + 1
      }
    } else {
      # Copy new file
      success <- file.copy(selected_pdf, dest_path, overwrite = FALSE)
      
      if (success) {
        cat("  [SUCCESS] Copied:", dest_filename, "\n")
        copied <- copied + 1
      } else {
        cat("  [ERROR] Copy failed\n")
        errors <- errors + 1
      }
    }
    
    cat("\n")
  }
  
  # Generate Reports ----
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  # Report 1: Text files found
  if (nrow(text_files_found) > 0) {
    txt_report <- file.path(OUTPUT_DIR, paste0("text_files_found_", timestamp, ".csv"))
    write.csv(text_files_found, txt_report, row.names = FALSE)
    cat("[REPORT] Text files:", txt_report, "\n")
  }
  
  # Report 2: Manual review needed (consolidated)
  manual_review <- data.frame()
  
  if (nrow(malformed_folders) > 0) {
    manual_review <- rbind(manual_review, data.frame(
      issue = "Malformed folder name",
      FAC = NA,
      hospital_name = NA,
      folder = malformed_folders$folder,
      details = malformed_folders$error,
      stringsAsFactors = FALSE
    ))
  }
  
  if (nrow(corrupt_files) > 0) {
    manual_review <- rbind(manual_review, data.frame(
      issue = "File < 1KB (corrupt)",
      FAC = corrupt_files$FAC,
      hospital_name = corrupt_files$hospital_name,
      folder = corrupt_files$folder,
      details = paste0(corrupt_files$filename, " (", corrupt_files$size_bytes, " bytes)"),
      stringsAsFactors = FALSE
    ))
  }
  
  if (nrow(no_pdfs) > 0) {
    manual_review <- rbind(manual_review, data.frame(
      issue = "No PDFs found",
      FAC = no_pdfs$FAC,
      hospital_name = no_pdfs$hospital_name,
      folder = no_pdfs$folder,
      details = "Folder contains no PDF files",
      stringsAsFactors = FALSE
    ))
  }
  
  if (nrow(duplicate_facs) > 0) {
    manual_review <- rbind(manual_review, data.frame(
      issue = "Duplicate FAC",
      FAC = duplicate_facs$FAC,
      hospital_name = NA,
      folder = duplicate_facs$folder,
      details = "Multiple folders with same FAC",
      stringsAsFactors = FALSE
    ))
  }
  
  if (nrow(manual_review) > 0) {
    review_report <- file.path(OUTPUT_DIR, paste0("manual_review_needed_", timestamp, ".csv"))
    write.csv(manual_review, review_report, row.names = FALSE)
    cat("[REPORT] Manual review:", review_report, "\n")
  }
  
  # Report 3: Multiple PDFs (informational)
  if (nrow(multiple_pdfs) > 0) {
    multi_report <- file.path(OUTPUT_DIR, paste0("multiple_pdfs_", timestamp, ".csv"))
    write.csv(multiple_pdfs, multi_report, row.names = FALSE)
    cat("[REPORT] Multiple PDFs:", multi_report, "\n")
  }
  
  # Final Summary ----
  cat("\n", strrep("=", 70), "\n")
  cat("SUMMARY\n")
  cat(strrep("=", 70), "\n")
  cat("Files copied (new):", copied, "\n")
  cat("Files updated (newer source):", updated, "\n")
  cat("Files skipped (current):", skipped, "\n")
  cat("Errors:", errors, "\n")
  cat("\nIssues Found:\n")
  cat("  Malformed folder names:", nrow(malformed_folders), "\n")
  cat("  Text files found:", nrow(text_files_found), "\n")
  cat("  Corrupt files (< 1KB):", nrow(corrupt_files), "\n")
  cat("  No PDFs found:", nrow(no_pdfs), "\n")
  cat("  Duplicate FACs:", nrow(duplicate_facs), "\n")
  cat("  Multiple PDFs (informational):", nrow(multiple_pdfs), "\n")
  cat("\nAll PDFs now in:", DEST_DIR, "\n")
  cat(strrep("=", 70), "\n")
  
  return(invisible(list(
    copied = copied,
    updated = updated,
    skipped = skipped,
    errors = errors,
    reports = list(
      text_files = nrow(text_files_found),
      manual_review = nrow(manual_review),
      multiple_pdfs = nrow(multiple_pdfs)
    )
  )))
}

# Run the copy operation ----
if (!interactive()) {
  copy_strategic_pdfs()
} else {
  cat("\nScript loaded. Run with: copy_strategic_pdfs()\n")
  cat("\nOutput format: {FAC}_{Hospital_Name}.pdf\n")
  cat("Selection: Most recent by file modification date\n")
  cat("Reports: Text files, manual review issues, multiple PDFs\n\n")
}