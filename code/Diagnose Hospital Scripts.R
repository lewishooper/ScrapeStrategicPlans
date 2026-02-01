# ============================================================================
# DIAGNOSTIC SCRIPT: Inventory All Files in Hospital Folders
# ============================================================================
# Purpose: Examine all hospital folders and list their contents
# Output: Dataframe with FAC, Hospital Name, and Filenames
# Author: Skip & Claude
# Date: 2026-01-25
# ============================================================================
library(dplyr)
library(tibble)

# ============================================================================
# CONFIGURATION
# ============================================================================

STRATEGIC_PLANS_DIR <- "E:/Hospital_Strategic_Plans/strategic_plans"

# ============================================================================
# MAIN FUNCTION
# ============================================================================

diagnose_hospital_files <- function(strategic_plans_dir = STRATEGIC_PLANS_DIR) {
  
  cat("\n")
  cat(strrep("=", 80), "\n")
  cat("HOSPITAL FILES DIAGNOSTIC\n")
  cat(strrep("=", 80), "\n")
  cat(sprintf("Scanning: %s\n", strategic_plans_dir))
  cat("\n")
  
  # Check directory exists
  if (!dir.exists(strategic_plans_dir)) {
    stop("Strategic plans directory not found: ", strategic_plans_dir)
  }
  
  # Get all folders
  all_folders <- list.dirs(strategic_plans_dir, full.names = TRUE, recursive = FALSE)
  
  cat(sprintf("Found %d hospital folders\n\n", length(all_folders)))
  
  # Initialize results dataframe
  results <- tibble(
    FAC = character(),
    HospitalName = character(),
    Filename = character(),
    FileType = character(),
    FileSizeKB = numeric(),
    FolderPath = character()
  )
  
  # Process each folder
  for (folder_path in all_folders) {
    
    folder_name <- basename(folder_path)
    
    # Extract FAC from folder name (first 3 digits)
    fac_match <- regexpr("^[0-9]{3}", folder_name)
    if (fac_match > 0) {
      fac <- substr(folder_name, 1, 3)
    } else {
      fac <- "UNKNOWN"
    }
    
    # Extract hospital name (everything after FAC_)
    if (fac != "UNKNOWN") {
      hospital_name <- sub("^[0-9]{3}_", "", folder_name)
      # Clean up underscores for readability
      hospital_name <- gsub("_", " ", hospital_name)
    } else {
      hospital_name <- folder_name
    }
    
    # Get all files in folder
    files <- list.files(folder_path, full.names = FALSE, recursive = FALSE)
    
    if (length(files) == 0) {
      # Empty folder - add one row with NA filename
      results <- bind_rows(results, tibble(
        FAC = fac,
        HospitalName = hospital_name,
        Filename = NA_character_,
        FileType = "EMPTY_FOLDER",
        FileSizeKB = NA_real_,
        FolderPath = folder_path
      ))
    } else {
      # Add row for each file
      for (file in files) {
        file_path <- file.path(folder_path, file)
        file_ext <- tools::file_ext(file)
        if (file_ext == "") file_ext <- "NO_EXTENSION"
        
        file_size_kb <- round(file.size(file_path) / 1024, 2)
        
        results <- bind_rows(results, tibble(
          FAC = fac,
          HospitalName = hospital_name,
          Filename = file,
          FileType = toupper(file_ext),
          FileSizeKB = file_size_kb,
          FolderPath = folder_path
        ))
      }
    }
  }
  
  # Print summary
  cat(strrep("=", 80), "\n")
  cat("SUMMARY\n")
  cat(strrep("=", 80), "\n")
  cat(sprintf("Total folders: %d\n", length(all_folders)))
  cat(sprintf("Total files: %d\n", sum(!is.na(results$Filename))))
  cat(sprintf("Empty folders: %d\n", sum(results$FileType == "EMPTY_FOLDER", na.rm = TRUE)))
  cat("\n")
  
  # File type breakdown
  cat("Files by type:\n")
  file_type_summary <- results %>%
    filter(!is.na(Filename)) %>%
    group_by(FileType) %>%
    summarise(Count = n(), .groups = "drop") %>%
    arrange(desc(Count))
  
  print(file_type_summary)
  cat("\n")
  
  # PDFs with non-standard naming
  cat("Checking PDF naming patterns...\n")
  pdf_files <- results %>%
    filter(FileType == "PDF", !is.na(Filename))
  
  standard_pattern <- "^Strategy_[0-9]{6}_[0-9]{3}(_Manual)?\\.pdf$"
  
  non_standard_pdfs <- pdf_files %>%
    filter(!grepl(standard_pattern, Filename, ignore.case = FALSE))
  
  cat(sprintf("  Standard naming: %d PDFs\n", 
              sum(grepl(standard_pattern, pdf_files$Filename, ignore.case = FALSE))))
  cat(sprintf("  Non-standard naming: %d PDFs\n", nrow(non_standard_pdfs)))
  cat("\n")
  
  return(results)
}

# ============================================================================
# CONVENIENCE FUNCTIONS
# ============================================================================

#' Show only PDF files
show_pdfs <- function(df) {
  df %>%
    filter(FileType == "PDF") %>%
    select(FAC, HospitalName, Filename, FileSizeKB) %>%
    arrange(FAC)
}

#' Show files that need renaming
show_files_needing_rename <- function(df) {
  standard_pattern <- "^Strategy_[0-9]{6}_[0-9]{3}(_Manual)?\\.pdf$"
  
  df %>%
    filter(FileType == "PDF", !is.na(Filename)) %>%
    filter(!grepl(standard_pattern, Filename, ignore.case = FALSE)) %>%
    select(FAC, HospitalName, Filename) %>%
    arrange(FAC)
}

#' Show multiple PDFs per hospital
show_multiple_pdfs <- function(df) {
  df %>%
    filter(FileType == "PDF", !is.na(Filename)) %>%
    group_by(FAC, HospitalName) %>%
    summarise(
      NumPDFs = n(),
      Files = paste(Filename, collapse = " | "),
      .groups = "drop"
    ) %>%
    filter(NumPDFs > 1) %>%
    arrange(FAC)
}

#' Show empty folders
show_empty_folders <- function(df) {
  df %>%
    filter(FileType == "EMPTY_FOLDER") %>%
    select(FAC, HospitalName) %>%
    arrange(FAC)
}

#' Export to CSV
export_to_csv <- function(df, filename = "hospital_files_inventory.csv") {
  output_path <- file.path("E:/Hospital_Strategic_Plans", filename)
  write.csv(df, output_path, row.names = FALSE)
  cat(sprintf("\nExported to: %s\n", output_path))
  return(output_path)
}

# ============================================================================
# RUN DIAGNOSTIC
# ============================================================================

cat("\n")
cat("########################################\n")
cat("HOSPITAL FILES DIAGNOSTIC TOOL\n")
cat("########################################\n")

cat("\nUsage:\n")
cat("  # Run full diagnostic\n")
cat("  results <- diagnose_hospital_files()\n")
cat("\n")
cat("  # View subsets\n")
cat("  show_pdfs(results)\n")
cat("  show_files_needing_rename(results)\n")
cat("  show_multiple_pdfs(results)\n")
cat("  show_empty_folders(results)\n")
cat("\n")
cat("  # Export results\n")
cat("  export_to_csv(results)\n")
cat("\n")
cat(strrep("=", 80), "\n")
cat("\n")

# Uncomment to run automatically:
DiagnoseStrategyFiles <- diagnose_hospital_files()
saveRDS(DiagnoseStrategyFiles,"E:/Hospital_Strategic_Plans/outputs/ListStrategyFiles.rds")
DiagnoseStrategyFiles<-DiagnoseStrategyFiles %>%
  filter()