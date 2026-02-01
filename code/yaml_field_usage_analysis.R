# ============================================================================
# YAML Field Usage Analysis
# ============================================================================
# Purpose: Document which fields from Hospital_strategy.yaml are used by
#          download_strategic_pdfs.R script
# Author: Skip
# Created: 2026-01-24
# Output: Creates a baseline documentation table for yaml field usage
# ============================================================================

# This script creates a baseline table showing:
# 1. Which yaml fields exist
# 2. Which fields are used by download_strategic_pdfs.R
# 3. Where in the code they are used
# 4. Whether they are essential or optional

library(dplyr)
library(tibble)

# YAML Field Usage Documentation ----
yaml_field_usage <- tribble(
  ~field_path, ~field_name, ~used_by_download_script, ~usage_location, ~usage_purpose, ~essential,
  
  # Top-level fields
  "FAC", "FAC", TRUE, "Lines 148, 210, 218-221", "Hospital identification, folder/file naming", TRUE,
  "name", "name", TRUE, "Line 209", "Folder name construction when local_folder missing", TRUE,
  "hospital_type", "hospital_type", FALSE, NA, "Classification only, not used in download", FALSE,
  "base_url", "base_url", FALSE, NA, "Not used in download script (used in search script)", FALSE,
  
  # strategy_search nested fields
  "strategy_search", "strategy_search (parent)", TRUE, "Multiple locations", "Container for all download-related metadata", TRUE,
  "strategy_search$search_attempted", "search_attempted", FALSE, NA, "Historical tracking only", FALSE,
  "strategy_search$search_date", "search_date", FALSE, NA, "Historical tracking only", FALSE,
  "strategy_search$strategy_url_found", "strategy_url_found", FALSE, NA, "Historical tracking only", FALSE,
  "strategy_search$strategy_url", "strategy_url", FALSE, NA, "Reference only, not used in download", FALSE,
  "strategy_search$pdf_found", "pdf_found", FALSE, NA, "Historical tracking only", FALSE,
  
  # CRITICAL FIELDS - Actually used
  "strategy_search$pdf_url", "pdf_url", TRUE, "download_pdf() function", "Source URL for downloading PDF", TRUE,
  "strategy_search$pdf_downloaded", "pdf_downloaded", TRUE, "Line ~1244 in main()", "Filter to determine which hospitals to process", TRUE,
  "strategy_search$download_confidence", "download_confidence", FALSE, NA, "Historical tracking only", FALSE,
  "strategy_search$content_type", "content_type", TRUE, "HTML extraction logic", "Determines whether HTML extraction needed", TRUE,
  "strategy_search$local_folder", "local_folder", TRUE, "Lines 205-211", "Primary method for determining folder location", TRUE,
  "strategy_search$local_filename", "local_filename", FALSE, NA, "Not used (filename generated from FAC + date)", FALSE,
  "strategy_search$manual_pdf_url", "manual_pdf_url", FALSE, NA, "Not used in current version", FALSE,
  "strategy_search$requires_manual_review", "requires_manual_review", FALSE, NA, "Historical tracking only", FALSE,
  "strategy_search$strategy_notes", "strategy_notes", FALSE, NA, "Historical tracking only", FALSE
)

# Add summary flags
yaml_field_usage <- yaml_field_usage %>%
  mutate(
    status = case_when(
      essential & used_by_download_script ~ "ESSENTIAL - Used for downloads",
      used_by_download_script ~ "Used but not essential",
      !used_by_download_script ~ "Not used - Historical/reference only"
    )
  )

# Function to generate analysis report
generate_usage_report <- function(save_to_file = TRUE, output_path = "E:/Hospital_Strategic_Plans/outputs") {
  
  cat("\n")
  cat(strrep("=", 80), "\n")
  cat("HOSPITAL_STRATEGY.YAML FIELD USAGE ANALYSIS\n")
  cat("Analysis of download_strategic_pdfs.R\n")
  cat(strrep("=", 80), "\n\n")
  
  # Summary statistics
  cat("SUMMARY\n")
  cat(strrep("-", 80), "\n")
  cat("Total Fields Documented:", nrow(yaml_field_usage), "\n")
  cat("Fields Used by Script:", sum(yaml_field_usage$used_by_download_script), "\n")
  cat("Essential Fields:", sum(yaml_field_usage$essential), "\n")
  cat("Historical/Unused Fields:", sum(!yaml_field_usage$used_by_download_script), "\n\n")
  
  # Essential fields
  cat("\n")
  cat(strrep("=", 80), "\n")
  cat("ESSENTIAL FIELDS (Required for download_strategic_pdfs.R)\n")
  cat(strrep("=", 80), "\n\n")
  essential_fields <- yaml_field_usage %>%
    filter(essential) %>%
    select(field_path, usage_purpose, usage_location)
  print(essential_fields, n = Inf)
  
  # Used but not essential
  cat("\n\n")
  cat(strrep("=", 80), "\n")
  cat("USED BUT NOT ESSENTIAL FIELDS\n")
  cat(strrep("=", 80), "\n\n")
  optional_used <- yaml_field_usage %>%
    filter(used_by_download_script, !essential) %>%
    select(field_path, usage_purpose, usage_location)
  
  if (nrow(optional_used) > 0) {
    print(optional_used, n = Inf)
  } else {
    cat("None\n")
  }
  
  # Historical/Unused fields
  cat("\n\n")
  cat(strrep("=", 80), "\n")
  cat("HISTORICAL/UNUSED FIELDS (Candidates for cleanup)\n")
  cat(strrep("=", 80), "\n\n")
  unused_fields <- yaml_field_usage %>%
    filter(!used_by_download_script) %>%
    select(field_path, field_name)
  print(unused_fields, n = Inf)
  
  # Field categorization
  cat("\n\n")
  cat(strrep("=", 80), "\n")
  cat("FIELD CATEGORIZATION\n")
  cat(strrep("=", 80), "\n\n")
  category_summary <- yaml_field_usage %>%
    group_by(status) %>%
    summarise(count = n(), .groups = "drop") %>%
    arrange(desc(count))
  print(category_summary, n = Inf)
  
  # Save to RDS if requested
  if (save_to_file) {
    if (!dir.exists(output_path)) {
      dir.create(output_path, recursive = TRUE)
    }
    
    output_file <- file.path(output_path, "yaml_field_usage_baseline.rds")
    saveRDS(yaml_field_usage, output_file)
    cat("\n\n[SAVE] Baseline saved to:", output_file, "\n")
    
    # Also save as CSV for easy viewing
    csv_file <- file.path(output_path, "yaml_field_usage_baseline.csv")
    write.csv(yaml_field_usage, csv_file, row.names = FALSE)
    cat("[SAVE] CSV version saved to:", csv_file, "\n")
  }
  
  cat("\n")
  cat(strrep("=", 80), "\n")
  cat("RECOMMENDATIONS FOR YAML CLEANUP\n")
  cat(strrep("=", 80), "\n\n")
  cat("After February API extraction, consider:\n")
  cat("1. Remove historical tracking fields that are no longer needed\n")
  cat("2. Keep essential fields: FAC, name, pdf_url, pdf_downloaded, content_type, local_folder\n")
  cat("3. Evaluate whether strategy_search nested structure is still optimal\n")
  cat("4. Consider consolidating metadata fields\n\n")
  
  return(invisible(yaml_field_usage))
}

# EXECUTION ----
if (!interactive()) {
  generate_usage_report(save_to_file = TRUE)
} else {
  cat("\n", strrep("=", 75), "\n")
  cat("YAML FIELD USAGE ANALYSIS LOADED\n")
  cat(strrep("=", 75), "\n\n")
  cat("To generate report: usage_data <- generate_usage_report()\n")
  cat("View data: View(yaml_field_usage)\n\n")
}
saveRDS(yaml_field_usage,paste0("E:/Hospital_Strategic_Plans/outputs/FieldUsage",Sys.Date(),".rds"))
        