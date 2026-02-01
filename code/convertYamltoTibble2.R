# Convert Hospital_strategy.yaml to Tibble
# Purpose: Load YAML results into R tibble for easy debugging and analysis
# Author: Hospital Strategic Plans Project
# Date: 2025-12-05

library(yaml)
library(dplyr)
library(tibble)

# Configuration
BASE_DIR <- "E:/Hospital_Strategic_Plans"
YAML_FILE <- file.path(BASE_DIR, "code/Hospital_strategy.yaml")

# Function to convert YAML to tibble
yaml_to_tibble <- function(yaml_path = YAML_FILE) {
  
  cat("\n", strrep("=", 70), "\n")
  cat("Converting YAML to Tibble\n")
  cat(strrep("=", 70), "\n\n")
  
  # Check file exists
  if (!file.exists(yaml_path)) {
    stop("YAML file not found: ", yaml_path)
  }
  
  cat("Reading:", yaml_path, "\n\n")
  
  # Read YAML
  hospitals <- read_yaml(yaml_path)
  
  # Handle both list and hospitals$hospitals structures
  if (!is.null(hospitals$hospitals)) {
    hospitals <- hospitals$hospitals
  }
  
  cat("Total hospitals in YAML:", length(hospitals), "\n\n")
  
  # Convert to flat tibble
  results_df <- tibble(
    FAC = character(),
    hospital_name = character(),
    hospital_type = character(),
    base_url = character(),
    search_attempted = logical(),
    search_date = character(),
    strategy_url_found = logical(),
    strategy_url = character(),
    pdf_found = logical(),
    pdf_url = character(),
    pdf_downloaded = logical(),
    download_confidence = character(),
    content_type = character(),
    local_folder = character(),
    local_filename = character(),
    manual_pdf_url = character(),
    requires_manual_review = logical(),
    strategy_notes = character()
  )
  
  # Process each hospital
  for (hospital in hospitals) {
    search <- hospital$strategy_search
    
    # Handle missing strategy_search
    if (is.null(search)) {
      search <- list(
        search_attempted = FALSE,
        search_date = NA,
        strategy_url_found = FALSE,
        strategy_url = "",
        pdf_found = FALSE,
        pdf_url = "",
        pdf_downloaded = FALSE,
        download_confidence = "",
        content_type = "",
        local_folder = "",
        local_filename = "",
        manual_pdf_url = "",
        requires_manual_review = FALSE,
        strategy_notes = "Not processed"
      )
    }
    
    # Create row
    row <- tibble(
      FAC = as.character(hospital$FAC),
      hospital_name = hospital$name,
      hospital_type = hospital$hospital_type,
      base_url = hospital$base_url,
      search_attempted = isTRUE(search$search_attempted),
      search_date = as.character(search$search_date %||% NA),
      strategy_url_found = isTRUE(search$strategy_url_found),
      strategy_url = search$strategy_url %||% "",
      pdf_found = isTRUE(search$pdf_found),
      pdf_url = search$pdf_url %||% "",
      pdf_downloaded = isTRUE(search$pdf_downloaded),
      download_confidence = search$download_confidence %||% "",
      content_type = search$content_type %||% "",
      local_folder = search$local_folder %||% "",
      local_filename = search$local_filename %||% "",
      manual_pdf_url = search$manual_pdf_url %||% "",
      requires_manual_review = isTRUE(search$requires_manual_review),
      strategy_notes = search$strategy_notes %||% ""
    )
    
    results_df <- bind_rows(results_df, row)
  }
  
  cat("Converted to tibble with", nrow(results_df), "rows\n")
  cat(strrep("=", 70), "\n\n")
  
  # Print summary statistics
  cat("SUMMARY STATISTICS\n")
  cat(strrep("-", 70), "\n")
  cat("Total hospitals:", nrow(results_df), "\n")
  cat("Search attempted:", sum(results_df$search_attempted), "\n")
  cat("Strategy URL found:", sum(results_df$strategy_url_found), "\n")
  cat("PDF found:", sum(results_df$pdf_found), "\n")
  cat("PDF downloaded:", sum(results_df$pdf_downloaded), "\n")
  cat("  - High confidence:", sum(results_df$download_confidence == "high"), "\n")
  cat("  - Medium confidence:", sum(results_df$download_confidence == "medium"), "\n")
  cat("  - Manual:", sum(results_df$download_confidence == "manual"), "\n")
  cat("Requires manual review:", sum(results_df$requires_manual_review), "\n")
  cat("Has manual PDF URL:", sum(results_df$manual_pdf_url != ""), "\n")
  cat(strrep("-", 70), "\n\n")
  
  return(results_df)
}

# Helper function: Quick filter views
create_filter_views <- function(df) {
  cat("\nCreating filter views...\n\n")
  
  list(
    all = df,
    downloaded = filter(df, pdf_downloaded == TRUE),
    needs_review = filter(df, requires_manual_review == TRUE),
    not_found = filter(df, search_attempted == TRUE, pdf_found == FALSE),
    has_manual_url = filter(df, manual_pdf_url != ""),
    medium_confidence = filter(df, download_confidence == "medium")
  )
}

# Run if not in interactive mode
if (!interactive()) {
  results <- yaml_to_tibble()
  print(results)
} else {
  cat("\nScript loaded. Usage:\n")
  cat("  results <- yaml_to_tibble()\n")
  cat("  views <- create_filter_views(results)\n\n")
  cat("Available views:\n")
  cat("  views$all              - All hospitals\n")
  cat("  views$downloaded       - Successfully downloaded PDFs\n")
  cat("  views$needs_review     - Requires manual review\n")
  cat("  views$not_found        - No PDF found\n")
  cat("  views$has_manual_url   - Has manual URL entered\n")
  cat("  views$medium_confidence - Medium confidence downloads\n\n")
}