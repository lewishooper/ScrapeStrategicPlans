# ============================================================================
# Manual Review Script: Strategy URLs with No/Low-Confidence PDFs
# Purpose: Create CSV for manual review of hospitals needing attention
# ============================================================================

library(yaml)
library(dplyr)
library(purrr)

# Set working directory
setwd("E:/Hospital_Strategic_Plans")

cat("\n")
cat(strrep("=", 70), "\n")
cat("Manual Review Report Generator\n")
cat("Strategy URLs Found but PDFs Missing or Low Confidence\n")
cat(strrep("=", 70), "\n\n")

# Read hospitals
hospitals <- read_yaml("code/Hospital_strategy.yaml")

cat("Total hospitals in file:", length(hospitals), "\n\n")

# Extract hospitals needing review
review_needed <- map_dfr(hospitals, function(h) {
  search <- h$strategy_search
  
  # Skip if no search attempted or strategy URL not found
  if (is.null(search) || 
      is.null(search$strategy_url_found) ||
      search$strategy_url_found == FALSE) {
    return(NULL)
  }
  
  # Determine review category
  review_category <- NA
  review_reason <- NA
  pdf_status <- NA
  
  # Case 1: Strategy URL found but NO PDF found
  if (search$strategy_url_found == TRUE && 
      (is.null(search$pdf_found) || search$pdf_found == FALSE)) {
    review_category <- "No PDF Found"
    review_reason <- "Strategy page identified but no PDF detected"
    pdf_status <- "Not Found"
  }
  
  # Case 2: PDF found but download failed
  else if (!is.null(search$pdf_found) && search$pdf_found == TRUE &&
           !is.null(search$pdf_downloaded) && search$pdf_downloaded == FALSE) {
    review_category <- "Download Failed"
    review_reason <- "PDF identified but download failed"
    pdf_status <- "Download Failed"
  }
  
  # Case 3: PDF downloaded with MEDIUM confidence
  else if (!is.null(search$pdf_downloaded) && search$pdf_downloaded == TRUE &&
           !is.null(search$download_confidence) && 
           search$download_confidence == "medium") {
    review_category <- "Medium Confidence"
    review_reason <- "PDF downloaded but verify it's correct strategic plan"
    pdf_status <- "Downloaded - Verify"
  }
  
  # If none of the above, skip
  else {
    return(NULL)
  }
  
  # Build output row
  data.frame(
    FAC = h$FAC,
    Hospital_Name = h$name,
    Hospital_Type = h$hospital_type,
    Base_URL = h$base_url,
    Strategy_URL = ifelse(is.null(search$strategy_url), "", search$strategy_url),
    PDF_URL = ifelse(is.null(search$pdf_url), "", search$pdf_url),
    Review_Category = review_category,
    Review_Reason = review_reason,
    PDF_Status = pdf_status,
    Download_Confidence = ifelse(is.null(search$download_confidence), 
                                 "", search$download_confidence),
    Local_Folder = ifelse(is.null(search$local_folder), "", search$local_folder),
    Local_Filename = ifelse(is.null(search$local_filename), "", search$local_filename),
    Strategy_Notes = ifelse(is.null(search$strategy_notes), "", search$strategy_notes),
    Manual_PDF_URL = ifelse(is.null(search$manual_pdf_url), "", search$manual_pdf_url),
    stringsAsFactors = FALSE
  )
})

# Check if we have any results
if (is.null(review_needed) || nrow(review_needed) == 0) {
  cat("No hospitals found needing manual review!\n")
  cat("All hospitals either:\n")
  cat("  - Have no strategy URL found, OR\n")
  cat("  - Have high-confidence PDFs successfully downloaded\n\n")
  quit()
}

# Sort by review category for easier manual review
review_needed <- review_needed %>%
  arrange(Review_Category, Hospital_Type, Hospital_Name)

# Generate summary statistics
cat("SUMMARY STATISTICS\n")
cat(strrep("-", 70), "\n")

summary_stats <- review_needed %>%
  group_by(Review_Category) %>%
  summarise(Count = n(), .groups = 'drop')

print(summary_stats)

cat("\nTotal hospitals needing review:", nrow(review_needed), "\n")

# Breakdown by hospital type
cat("\nBreakdown by Hospital Type:\n")
type_breakdown <- review_needed %>%
  group_by(Hospital_Type, Review_Category) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  arrange(Hospital_Type, Review_Category)

print(type_breakdown)

# Save full report
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
output_file <- file.path("outputs", paste0("manual_review_needed_", timestamp, ".csv"))

write.csv(review_needed, output_file)

cat("\n")
cat(strrep("=", 70), "\n")
cat("REPORT SAVED\n")
cat(strrep("=", 70), "\n")
cat("File:", output_file, "\n")
cat("Total records:", nrow(review_needed), "\n")

# Create separate files by category for easier workflow
cat("\nCreating category-specific files...\n")

for (category in unique(review_needed$Review_Category)) {
  category_data <- review_needed %>%
    filter(Review_Category == category)
  
  safe_category_name <- gsub(" ", "_", tolower(category))
  category_file <- file.path("outputs", 
                             paste0("review_", safe_category_name, "_", timestamp, ".csv"))
  
  write.csv(category_data, category_file, row.names = FALSE)
  cat("  -", category, ":", nrow(category_data), "hospitals ->", basename(category_file), "\n")
}

cat("\n")
cat(strrep("=", 70), "\n")
cat("NEXT STEPS\n")
cat(strrep("=", 70), "\n")
cat("1. Open the CSV file(s) in Excel\n")
cat("2. For each hospital:\n")
cat("   - Visit the Strategy_URL\n")
cat("   - If PDF exists but wasn't found: copy URL to Manual_PDF_URL column\n")
cat("   - If HTML-only: note in spreadsheet for later handling\n")
cat("   - If medium confidence: verify downloaded PDF is correct\n")
cat("3. Save your updated spreadsheet\n")
cat("4. Return to update Hospital_strategy.yaml with findings\n")
cat(strrep("=", 70), "\n\n")

# Optional: Create a quick reference summary for console
cat("QUICK REFERENCE - Hospitals by Category:\n")
cat(strrep("-", 70), "\n")

for (category in unique(review_needed$Review_Category)) {
  cat("\n", category, ":\n")
  category_hospitals <- review_needed %>%
    filter(Review_Category == category) %>%
    select(FAC, Hospital_Name)
  
  for (i in 1:min(10, nrow(category_hospitals))) {
    cat("  ", category_hospitals$FAC[i], "-", category_hospitals$Hospital_Name[i], "\n")
  }
  
  if (nrow(category_hospitals) > 10) {
    cat("  ... and", nrow(category_hospitals) - 10, "more\n")
  }
}

cat("\n")