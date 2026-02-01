# ============================================================================
# YAML to Folder Audit Script
# ============================================================================
# Purpose: Verify that each hospital in Hospital_strategy.yaml has a 
#          corresponding folder with contents in the strategic_plans directory
# Author: Skip
# Created: 2026-01-24
# Output: E:/Hospital_Strategic_Plans/outputs/Yaml_strategicPlansReview.rds
# ============================================================================

# REQUIRED LIBRARIES ----
library(yaml)
library(dplyr)
library(tibble)

# CONFIGURATION ----
CONFIG <- list(
  base_dir = "E:/Hospital_Strategic_Plans",
  yaml_file = "code/Hospital_strategy.yaml",
  strategic_plans_folder = "strategic_plans",
  output_folder = "outputs",
  output_filename = "Yaml_strategicPlansReview.rds"
)

# FUNCTIONS ----

#' Format FAC code with leading zeros
format_fac <- function(fac) {
  sprintf("%03d", as.numeric(fac))
}

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

#' Get folder path for a hospital
get_expected_folder <- function(hospital) {
  # Try to get local_folder from yaml
  local_folder <- hospital$strategy_search$local_folder
  
  # If not present or empty, construct from FAC and name
  if (is.null(local_folder) || local_folder == "") {
    fac <- format_fac(hospital$FAC)
    name <- gsub("[^a-zA-Z0-9_-]", "_", hospital$name)
    name <- gsub("_{2,}", "_", name)
    name <- substr(name, 1, 100)
    local_folder <- paste0(fac, "_", name)
  }
  
  return(local_folder)
}

#' List files in a folder
list_folder_contents <- function(folder_path) {
  if (!dir.exists(folder_path)) {
    return(NA_character_)
  }
  
  files <- list.files(folder_path, full.names = FALSE)
  
  if (length(files) == 0) {
    return("EMPTY_FOLDER")
  }
  
  # Get file extensions
  file_info <- sapply(files, function(f) {
    ext <- tools::file_ext(f)
    if (ext == "") ext <- "no_extension"
    paste0(f, " (", ext, ")")
  })
  
  return(paste(file_info, collapse = "; "))
}

#' Main audit function
audit_yaml_folders <- function() {
  
  cat("\n")
  cat(strrep("=", 75), "\n")
  cat("YAML TO FOLDER AUDIT\n")
  cat(strrep("=", 75), "\n\n")
  
  # Validate paths
  if (!dir.exists(CONFIG$base_dir)) {
    stop("Base directory not found: ", CONFIG$base_dir)
  }
  
  strategic_plans_path <- file.path(CONFIG$base_dir, CONFIG$strategic_plans_folder)
  if (!dir.exists(strategic_plans_path)) {
    stop("Strategic plans folder not found: ", strategic_plans_path)
  }
  
  output_path <- file.path(CONFIG$base_dir, CONFIG$output_folder)
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
    cat("[CREATE] Created output folder:", output_path, "\n")
  }
  
  # Read YAML
  hospitals <- read_yaml_hospitals()
  
  # Build audit data
  cat("[AUDIT] Checking folders for", length(hospitals), "hospitals...\n\n")
  
  audit_results <- tibble()
  
  for (i in seq_along(hospitals)) {
    hospital <- hospitals[[i]]
    
    fac <- format_fac(hospital$FAC)
    name <- hospital$name
    content_type <- hospital$strategy_search$content_type
    
    # Handle NULL content_type
    if (is.null(content_type)) {
      content_type <- NA_character_
    }
    
    # Get expected folder
    expected_folder <- get_expected_folder(hospital)
    folder_path <- file.path(strategic_plans_path, expected_folder)
    
    # Check if folder exists
    folder_exists <- dir.exists(folder_path)
    
    # List files in folder
    files_list <- list_folder_contents(folder_path)
    
    # Add to results
    audit_results <- bind_rows(
      audit_results,
      tibble(
        FAC = fac,
        Hospital_name = name,
        content_type = content_type,
        expected_folder = expected_folder,
        folder_exists = folder_exists,
        files_in_folder = files_list
      )
    )
    
    # Progress indicator
    if (i %% 20 == 0) {
      cat("  Processed", i, "of", length(hospitals), "hospitals\n")
    }
  }
  
  cat("  Processed", length(hospitals), "of", length(hospitals), "hospitals\n\n")
  
  # Summary statistics
  cat(strrep("=", 75), "\n")
  cat("AUDIT SUMMARY\n")
  cat(strrep("=", 75), "\n")
  cat("Total Hospitals:", nrow(audit_results), "\n")
  cat("Folders Found:", sum(audit_results$folder_exists), "\n")
  cat("Folders Missing:", sum(!audit_results$folder_exists), "\n")
  
  # Count folders with no files
  empty_or_missing <- sum(is.na(audit_results$files_in_folder) | 
                          audit_results$files_in_folder == "EMPTY_FOLDER")
  cat("Empty/Missing Folders:", empty_or_missing, "\n")
  
  # Count by content type
  cat("\nBy Content Type:\n")
  content_summary <- audit_results %>%
    group_by(content_type) %>%
    summarise(
      count = n(),
      folders_exist = sum(folder_exists),
      .groups = "drop"
    )
  print(content_summary)
  
  # Save results
  output_file <- file.path(output_path, CONFIG$output_filename)
  saveRDS(audit_results, output_file)
  
  cat("\n[SAVE] Results saved to:", output_file, "\n")
  cat(strrep("=", 75), "\n\n")
  
  # Show hospitals with missing folders
  missing_folders <- audit_results %>%
    filter(!folder_exists) %>%
    select(FAC, Hospital_name, expected_folder)
  
  if (nrow(missing_folders) > 0) {
    cat("\nHOSPITALS WITH MISSING FOLDERS:\n")
    cat(strrep("-", 75), "\n")
    print(missing_folders, n = Inf)
  } else {
    cat("\n✓ All hospitals have folders in strategic_plans directory\n")
  }
  
  return(invisible(audit_results))
}

# EXECUTION ----
if (!interactive()) {
  audit_yaml_folders()
} else {
  cat("\n", strrep("=", 75), "\n")
  cat("YAML TO FOLDER AUDIT SCRIPT LOADED\n")
  cat(strrep("=", 75), "\n\n")
  cat("Script loaded. Run with: results <- audit_yaml_folders()\n\n")
}
FolderAudit<-audit_yaml_folders()
saveRDS(FolderAudit,paste0("E:/Hospital_Strategic_Plans/outputs","/FolderAudit",Sys.Date(),".Rds"))
