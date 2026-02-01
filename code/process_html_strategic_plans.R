# process_html_strategic_plans.R
# Purpose: Convert HTML strategic plans to PDF for hospitals without downloadable PDFs
# Author: Hospital Strategic Plans Project
# Date: 2025-12-07

library(yaml)
library(pagedown)
library(stringr)
library(dplyr)

# Configuration ----
CONFIG <- list(
  base_dir = "E:/Hospital_Strategic_Plans",
  working_yaml = "code/Hospital_strategy.yaml",
  pdf_folder = "strategic_plans",
  
  # Conversion settings
  wait_time = 3,  # seconds to wait for page load
  timeout = 30,   # max seconds for conversion
  
  # Testing
  test_mode = FALSE,
  test_sample_size = 3  # Start with just 3 hospitals
)

# Helper Functions ----
clean_hospital_name <- function(name) {
  name %>%
    str_replace_all("[^A-Za-z0-9]", "_") %>%
    str_replace_all("_{2,}", "_") %>%
    str_remove("^_|_$")
}

# Read hospitals from YAML
read_yaml_hospitals <- function() {
  yaml_path <- file.path(CONFIG$base_dir, CONFIG$working_yaml)
  
  if (!file.exists(yaml_path)) {
    stop("Working YAML file not found: ", yaml_path)
  }
  
  data <- read_yaml(yaml_path)
  
  if (is.null(data$hospitals)) {
    hospitals <- data
  } else {
    hospitals <- data$hospitals
  }
  
  cat("[LOAD] Total hospitals in working file:", length(hospitals), "\n")
  return(hospitals)
}

# Write updated YAML
write_yaml_hospitals <- function(hospitals) {
  yaml_path <- file.path(CONFIG$base_dir, CONFIG$working_yaml)
  
  # Create backup
  if (file.exists(yaml_path)) {
    backup_dir <- file.path(CONFIG$base_dir, "Backups")
    if (!dir.exists(backup_dir)) {
      dir.create(backup_dir, recursive = TRUE)
    }
    
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    backup_path <- file.path(backup_dir, paste0("Hospital_strategy_", timestamp, ".yaml"))
    file.copy(yaml_path, backup_path)
    cat("[BACKUP] Created:", basename(backup_path), "\n")
  }
  
  write_yaml(hospitals, yaml_path)
  cat("[SAVE] Updated working file\n")
}

# Convert HTML to PDF
convert_html_to_pdf <- function(strategy_url, fac, hospital_name) {
  cat("  [HTML→PDF] Converting HTML page to PDF...\n")
  cat("    URL:", strategy_url, "\n")
  
  safe_name <- clean_hospital_name(hospital_name)
  folder_name <- paste0(fac, "_", safe_name)
  folder_path <- file.path(CONFIG$base_dir, CONFIG$pdf_folder, folder_name)
  
  dir.create(folder_path, showWarnings = FALSE, recursive = TRUE)
  
  year_month <- format(Sys.time(), "%Y%m")
  filename <- paste0("Strategy_", year_month, "_", fac, "_HTML.pdf")
  filepath <- file.path(folder_path, filename)
  
  cat("    Destination:", file.path(folder_name, filename), "\n")
  
  conversion_success <- tryCatch({
    pagedown::chrome_print(
      input = strategy_url,
      output = filepath,
      wait = CONFIG$wait_time,
      timeout = CONFIG$timeout,
      verbose = 0
    )
    
    if (file.exists(filepath)) {
      file_size <- file.info(filepath)$size
      cat("    ✓ HTML converted to PDF (", round(file_size/1024, 1), "KB)\n")
      TRUE
    } else {
      FALSE
    }
    
  }, error = function(e) {
    cat("    ERROR: Conversion failed -", e$message, "\n")
    FALSE
  })
  
  if (conversion_success) {
    return(list(
      success = TRUE,
      folder = folder_name,
      filename = filename,
      error = NULL
    ))
  } else {
    return(list(
      success = FALSE,
      folder = folder_name,
      filename = filename,
      error = "HTML to PDF conversion failed"
    ))
  }
}

# Process HTML hospitals
process_html_hospitals <- function() {
  cat("\n", strrep("=", 70), "\n")
  cat("Processing HTML Strategic Plans\n")
  cat(strrep("=", 70), "\n\n")
  
  # Read hospitals
  hospitals <- read_yaml_hospitals()
  
  # Filter to hospitals that:
  # 1. Have strategy_url_found = TRUE
  # 2. Have pdf_found = FALSE OR pdf_downloaded = FALSE
  # 3. Have a strategy_url
# Filter to hospitals with HTML strategic plans
  # Filter to hospitals with HTML strategic plans
  # Look for: strategy_url_found = TRUE AND content_type = "html"
  html_candidates <- Filter(function(h) {
    search <- h$strategy_search
    !is.null(search) &&
      !is.null(search$strategy_url_found) && search$strategy_url_found == TRUE &&
      !is.null(search$strategy_url) && search$strategy_url != "" &&
      !is.null(search$content_type) && search$content_type == "html"
  }, hospitals)
  
  cat("Found", length(html_candidates), "hospitals with strategy URLs but no PDFs\n\n")
  
  if (length(html_candidates) == 0) {
    cat("No HTML hospitals to process\n")
    return(invisible(NULL))
  }
  
  # Test mode
  if (CONFIG$test_mode) {
    html_candidates <- html_candidates[1:min(CONFIG$test_sample_size, length(html_candidates))]
    cat("[TEST MODE] Processing", length(html_candidates), "hospitals\n\n")
  }
  
  # Track results
  converted <- 0
  failed <- 0
  
  # Process each hospital
  for (i in seq_along(html_candidates)) {
    hospital <- html_candidates[[i]]
    
    cat("\n", strrep("=", 70), "\n")
    cat("Processing:", hospital$name, "(FAC:", hospital$FAC, ")\n")
    cat("Strategy URL:", hospital$strategy_search$strategy_url, "\n")
    cat(strrep("=", 70), "\n")
    
    result <- convert_html_to_pdf(
      strategy_url = hospital$strategy_search$strategy_url,
      fac = hospital$FAC,
      hospital_name = hospital$name
    )
    
    # Find this hospital in original list and update
    hospital_idx <- which(sapply(hospitals, function(h) h$FAC == hospital$FAC))
    
    if (result$success) {
      hospitals[[hospital_idx]]$strategy_search$pdf_found <- TRUE
      hospitals[[hospital_idx]]$strategy_search$pdf_downloaded <- TRUE
      hospitals[[hospital_idx]]$strategy_search$content_type <- "html_converted"
      hospitals[[hospital_idx]]$strategy_search$local_folder <- result$folder
      hospitals[[hospital_idx]]$strategy_search$local_filename <- result$filename
      hospitals[[hospital_idx]]$strategy_search$download_confidence <- "html_conversion"
      hospitals[[hospital_idx]]$strategy_search$strategy_notes <- paste0(
        hospitals[[hospital_idx]]$strategy_search$strategy_notes,
        " | HTML page converted to PDF on ", Sys.Date()
      )
      hospitals[[hospital_idx]]$strategy_search$requires_manual_review <- FALSE
      
      converted <- converted + 1
    } else {
      hospitals[[hospital_idx]]$strategy_search$strategy_notes <- paste0(
        hospitals[[hospital_idx]]$strategy_search$strategy_notes,
        " | HTML to PDF conversion failed on ", Sys.Date(), ": ", result$error
      )
      
      failed <- failed + 1
    }
    
    cat("\nProgress:", i, "/", length(html_candidates), "processed\n")
  }
  
  # Write updated YAML
  cat("\n", strrep("=", 70), "\n")
  cat("Writing updated YAML file...\n")
  write_yaml_hospitals(hospitals)
  
  # Summary
  cat("\n", strrep("=", 70), "\n")
  cat("SUMMARY\n")
  cat(strrep("=", 70), "\n")
  cat("Total HTML hospitals processed:", length(html_candidates), "\n")
  cat("Successfully converted:", converted, "\n")
  cat("Failed conversions:", failed, "\n")
  cat(strrep("=", 70), "\n")
  
  return(invisible(list(
    processed = length(html_candidates),
    converted = converted,
    failed = failed
  )))
}

# Run the script ----
if (!interactive()) {
  results <- process_html_hospitals()
} else {
  cat("\nScript loaded. Run with: results <- process_html_hospitals()\n")
  cat("To change test mode: CONFIG$test_mode <- FALSE\n\n")
  cat("To adjust sample size: CONFIG$test_sample_size <- 3\n\n")
}
