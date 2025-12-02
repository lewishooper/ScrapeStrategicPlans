# Copy Strategic Plan PDFs to Processed Folder
# Purpose: Consolidate all downloaded PDFs into a single master folder
# Author: Hospital Strategic Plans Project
# Date: 2025-11-30

library(dplyr)

# Configuration
BASE_DIR <- "E:/Hospital_Strategic_Plans"
SOURCE_DIR <- file.path(BASE_DIR, "strategic_plans")
DEST_DIR <- file.path(BASE_DIR, "Processed")

# Create destination directory if it doesn't exist
if (!dir.exists(DEST_DIR)) {
  dir.create(DEST_DIR, recursive = TRUE)
  cat("Created destination directory:", DEST_DIR, "\n")
}

# Function to copy PDFs
copy_strategic_pdfs <- function() {
  cat("\n", strrep("=", 70), "\n")
  cat("Copying Strategic Plan PDFs to Processed Folder\n")
  cat(strrep("=", 70), "\n\n")
  
  cat("Source:", SOURCE_DIR, "\n")
  cat("Destination:", DEST_DIR, "\n\n")
  
  # Get all subdirectories in strategic_plans
  subdirs <- list.dirs(SOURCE_DIR, full.names = TRUE, recursive = FALSE)
  
  if (length(subdirs) == 0) {
    cat("No subdirectories found in strategic_plans folder.\n")
    return(invisible(NULL))
  }
  
  cat("Found", length(subdirs), "hospital folder(s)\n\n")
  
  # Track results
  copied <- 0
  skipped <- 0
  errors <- 0
  
  # Process each subdirectory
  for (subdir in subdirs) {
    hospital_name <- basename(subdir)
    
    # Find all PDFs in this subdirectory
    pdf_files <- list.files(subdir, pattern = "\\.pdf$", 
                            full.names = TRUE, ignore.case = TRUE)
    
    if (length(pdf_files) == 0) {
      cat("[SKIP]", hospital_name, "- No PDFs found\n")
      skipped <- skipped + 1
      next
    }
    
    # Copy each PDF
    for (pdf_path in pdf_files) {
      pdf_filename <- basename(pdf_path)
      dest_path <- file.path(DEST_DIR, pdf_filename)
      
      # Check if file already exists
      if (file.exists(dest_path)) {
        # Check if files are identical
        if (tools::md5sum(pdf_path) == tools::md5sum(dest_path)) {
          cat("[EXISTS]", pdf_filename, "- Already in Processed (identical)\n")
          skipped <- skipped + 1
        } else {
          # Files differ - create versioned copy
          timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
          base_name <- tools::file_path_sans_ext(pdf_filename)
          ext <- tools::file_ext(pdf_filename)
          new_filename <- paste0(base_name, "_", timestamp, ".", ext)
          dest_path <- file.path(DEST_DIR, new_filename)
          
          success <- file.copy(pdf_path, dest_path, overwrite = FALSE)
          
          if (success) {
            cat("[COPY]", pdf_filename, "->", new_filename, 
                "(version conflict)\n")
            copied <- copied + 1
          } else {
            cat("[ERROR]", pdf_filename, "- Copy failed\n")
            errors <- errors + 1
          }
        }
      } else {
        # File doesn't exist - copy it
        success <- file.copy(pdf_path, dest_path, overwrite = FALSE)
        
        if (success) {
          cat("[COPY]", pdf_filename, "\n")
          copied <- copied + 1
        } else {
          cat("[ERROR]", pdf_filename, "- Copy failed\n")
          errors <- errors + 1
        }
      }
    }
  }
  
  # Summary
  cat("\n", strrep("=", 70), "\n")
  cat("SUMMARY\n")
  cat(strrep("=", 70), "\n")
  cat("Files copied:", copied, "\n")
  cat("Files skipped (already exist):", skipped, "\n")
  cat("Errors:", errors, "\n")
  cat("\nAll PDFs now in:", DEST_DIR, "\n")
  cat(strrep("=", 70), "\n")
  
  return(invisible(list(copied = copied, skipped = skipped, errors = errors)))
}

# Run the copy operation
if (!interactive()) {
  copy_strategic_pdfs()
} else {
  cat("\nScript loaded. Run with: copy_strategic_pdfs()\n")
}