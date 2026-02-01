# ===================================================================
# HOSPITAL STRATEGIC PLANS - TEXT TO DATAFRAME CONVERTER
# TWO-TABLE NORMALIZED SCHEMA
# ===================================================================
# Purpose: Convert Phase 3 Level 1 extraction text files into two linked tables
# Source Folder: E:\Hospital_Strategic_Plans\processed\StrategyFebruary2026
# Output: 
#   - l1_objectives (one row per strategic objective)
#   - hospital_metadata (one row per hospital)
# ===================================================================
#rm(list=ls())
library(tidyverse)
library(lubridate)

# Set working directory and define paths
source_folder <- "E:/Hospital_Strategic_Plans/processed/StrategyFebruary2026"

# ===================================================================
# FUNCTION: Extract FAC from filename
# ===================================================================
extract_fac_from_filename <- function(filename) {
  # Pattern: Phase3_L1_XXX_YYYYMMDD.txt (note: capital L)
  # Extract 3 digits after "Phase3_L1_"
  fac_match <- str_extract(filename, "(?<=Phase3_L1_)\\d{3}")
  return(fac_match)
}

# ===================================================================
# FUNCTION: Parse a single field from text - FIXED VERSION
# ===================================================================
parse_field <- function(text, field_name) {
  # More precise pattern - stops at next field or section break
  pattern <- paste0(field_name, ":\\s*(.+?)(?=\\n[A-Z_]+:|\\n={5,}|\\n-{3,}|$)")
  match <- str_match(text, regex(pattern, dotall = FALSE))
  if (!is.na(match[1, 2])) {
    return(str_trim(match[1, 2]))
  }
  return(NA_character_)
}

# ===================================================================
# FUNCTION: Extract all L1 entries from text - FIXED VERSION
# ===================================================================
extract_l1_entries <- function(text) {
  # Find all L1.X_TITLE and L1.X_TEXT pairs
  # Pattern handles newline after L1.X_TEXT: before content begins
  l1_pattern <- "(L1\\.\\d+)_TITLE:\\s*(.+?)\\s+\\1_TEXT:\\s*\\n+(.+?)(?=\\n+---|\\n+={5,}|\\n+L1\\.\\d+_TITLE:|$)"
  matches <- str_match_all(text, regex(l1_pattern, dotall = TRUE))[[1]]
  
  if (nrow(matches) == 0) {
    return(NULL)
  }
  
  # Create dataframe of L1 entries
  l1_df <- tibble(
    l1_sequence = matches[, 2],  # e.g., "L1.1", "L1.2"
    l1_title = str_trim(matches[, 3]),
    l1_text = str_trim(matches[, 4])
  ) %>%
    # Extract just the number from L1.X
    mutate(
      objective_number = as.integer(str_extract(l1_sequence, "(?<=L1\\.)\\d+"))
    ) %>%
    select(objective_number, l1_title, l1_text)
  
  return(l1_df)
}

# ===================================================================
# FUNCTION: Process a single text file
# ===================================================================
process_file <- function(filepath) {
  
  # Read file
  text <- read_file(filepath)
  filename <- basename(filepath)
  
  # Extract FAC from filename (authoritative source)
  fac <- extract_fac_from_filename(filename)
  
  # Extract document-level fields
  fac_from_text <- parse_field(text, "FAC")
  hospital_name_text <- parse_field(text, "HOSPITAL_NAME_TEXT")
  hospital_name_file <- parse_field(text, "HOSPITAL_NAME_FILE")
  plan_start <- parse_field(text, "PLAN_START")
  plan_end <- parse_field(text, "PLAN_END")
  plan_classification <- parse_field(text, "PLAN_CLASSIFICATION")
  document_structure <- parse_field(text, "DOCUMENT_STRUCTURE")
  document_issues <- parse_field(text, "DOCUMENT_ISSUES")
  l2_present <- parse_field(text, "L2_PRESENT")
  specificity <- parse_field(text, "SPECIFICITY")
  scope <- parse_field(text, "SCOPE")
  overall_quality_notes <- parse_field(text, "OVERALL_QUALITY_NOTES")
  extraction_date_raw <- parse_field(text, "EXTRACTION_DATE")
  
  # Extract L1 entries
  l1_entries <- extract_l1_entries(text)
  
  if (is.null(l1_entries) || nrow(l1_entries) == 0) {
    warning(paste("No L1 entries found in file:", filename))
    return(list(objectives = NULL, metadata = NULL))
  }
  
  # Count L1 objectives for validation
  num_l1_objectives <- nrow(l1_entries)
  
  # Create L1 objectives table
  objectives_df <- l1_entries %>%
    mutate(
      fac = fac,
      extraction_date = extraction_date_raw
    ) %>%
    select(fac, extraction_date, objective_number, l1_title, l1_text)
  
  # Create hospital metadata table
  metadata_df <- tibble(
    fac = fac,
    extraction_date = extraction_date_raw,
    fac_from_text = fac_from_text,
    hospital_name_text = hospital_name_text,
    hospital_name_file = hospital_name_file,
    plan_start = plan_start,
    plan_end = plan_end,
    plan_classification = plan_classification,
    document_structure = document_structure,
    document_issues = document_issues,
    l2_present = l2_present,
    specificity = specificity,
    scope = scope,
    overall_quality_notes = overall_quality_notes,
    num_l1_objectives = num_l1_objectives,
    processed_date = "2026-01"
  )
  
  return(list(objectives = objectives_df, metadata = metadata_df))
}

# ===================================================================
# MAIN PROCESSING
# ===================================================================

# Get all .txt files in the folder
txt_files <- list.files(source_folder, pattern = "\\.txt$", full.names = TRUE)

cat("Found", length(txt_files), "text files to process\n\n")

# Initialize lists to collect results
all_objectives <- list()
all_metadata <- list()

# Process all files
for (i in seq_along(txt_files)) {
  file <- txt_files[i]
  cat("Processing", i, "of", length(txt_files), ":", basename(file), "\n")
  
  result <- tryCatch(
    process_file(file),
    error = function(e) {
      warning(paste("Error processing", basename(file), ":", e$message))
      return(list(objectives = NULL, metadata = NULL))
    }
  )
  
  if (!is.null(result$objectives)) {
    all_objectives[[i]] <- result$objectives
  }
  if (!is.null(result$metadata)) {
    all_metadata[[i]] <- result$metadata
  }
}

# Combine all results
l1_objectives <- bind_rows(all_objectives)
hospital_metadata <- bind_rows(all_metadata)

# ===================================================================
# DATA TYPE CONVERSION - Convert dates to Date objects
# ===================================================================

cat("\n=== Converting dates to Date objects ===\n")

# Convert l1_objectives dates
l1_objectives <- l1_objectives %>%
  mutate(
    extraction_date = ym(extraction_date)
  )

# Convert hospital_metadata dates
hospital_metadata <- hospital_metadata %>%
  mutate(
    extraction_date = ym(extraction_date),
    plan_start = if_else(
      !is.na(plan_start) & plan_start != "nf",
      ymd(paste0(plan_start, "-01-01")),
      NA_Date_
    ),
    plan_end = if_else(
      !is.na(plan_end) & plan_end != "nf",
      ymd(paste0(plan_end, "-01-01")),
      NA_Date_
    ),
    processed_date = ym(processed_date)
  )

cat("Date conversion complete\n")

# ===================================================================
# VALIDATION & SUMMARY
# ===================================================================

cat("\n========================================\n")
cat("=== PROCESSING COMPLETE ===\n")
cat("========================================\n\n")

cat("=== TABLE 1: L1_OBJECTIVES ===\n")
cat("Total L1 objective rows:", nrow(l1_objectives), "\n")
cat("Unique hospitals (FAC):", n_distinct(l1_objectives$fac), "\n")
cat("Date range:", min(l1_objectives$extraction_date, na.rm = TRUE), "to", 
    max(l1_objectives$extraction_date, na.rm = TRUE), "\n")
cat("\nObjective number distribution:\n")
print(table(l1_objectives$objective_number))
cat("\nColumns:\n")
print(glimpse(l1_objectives))

cat("\n=== TABLE 2: HOSPITAL_METADATA ===\n")
cat("Total hospital rows:", nrow(hospital_metadata), "\n")
cat("Unique hospitals (FAC):", n_distinct(hospital_metadata$fac), "\n")
cat("Expected: 133 unique hospitals\n")

cat("\n=== FAC VALIDATION ===\n")
cat("FACs marked as 'nf' in text:", 
    sum(hospital_metadata$fac_from_text == "nf", na.rm = TRUE), "\n")
cat("Missing FAC from filename:", 
    sum(is.na(hospital_metadata$fac)), "\n")

# Check for FAC mismatches
mismatches <- hospital_metadata %>%
  filter(fac != fac_from_text & fac_from_text != "nf" & !is.na(fac_from_text))

if (nrow(mismatches) > 0) {
  cat("\nWARNING: FAC mismatches found:\n")
  print(mismatches %>% select(fac, fac_from_text, hospital_name_text))
} else {
  cat("\nNo FAC mismatches detected (all 'nf' or matching)\n")
}

cat("\n=== DUPLICATE CHECK ===\n")
duplicates <- hospital_metadata %>%
  group_by(fac, extraction_date) %>%
  filter(n() > 1) %>%
  ungroup()

if (nrow(duplicates) > 0) {
  cat("WARNING: Duplicate hospital extractions found:\n")
  print(duplicates %>% select(fac, hospital_name_text, extraction_date))
} else {
  cat("No duplicates found (all FAC + extraction_date combinations unique)\n")
}

cat("\n=== L1 OBJECTIVES COUNT VALIDATION ===\n")
cat("Min objectives per hospital:", min(hospital_metadata$num_l1_objectives), "\n")
cat("Max objectives per hospital:", max(hospital_metadata$num_l1_objectives), "\n")
cat("Mean objectives per hospital:", round(mean(hospital_metadata$num_l1_objectives), 1), "\n")
cat("\nDistribution:\n")
print(table(hospital_metadata$num_l1_objectives))

cat("\n=== PLAN DATE VALIDATION ===\n")
cat("Plan start range:", min(hospital_metadata$plan_start, na.rm = TRUE), "to",
    max(hospital_metadata$plan_start, na.rm = TRUE), "\n")
cat("Plan end range:", min(hospital_metadata$plan_end, na.rm = TRUE), "to",
    max(hospital_metadata$plan_end, na.rm = TRUE), "\n")

cat("\nColumns:\n")
print(glimpse(hospital_metadata))

cat("\n=== SAMPLE DATA ===\n")
cat("\nFirst 5 rows of l1_objectives:\n")
print(head(l1_objectives, 5))

cat("\nFirst 3 rows of hospital_metadata:\n")
print(head(hospital_metadata, 3))

# ===================================================================
# SAVE OUTPUT
# ===================================================================

cat("\n=== SAVING FILES ===\n")

# Save L1 objectives
output_file_obj_rds <- file.path(source_folder, "l1_objectives.rds")
saveRDS(l1_objectives, output_file_obj_rds)
cat("L1 objectives saved as RDS:", output_file_obj_rds, "\n")

output_file_obj_csv <- file.path(source_folder, "l1_objectives.csv")
write_csv(l1_objectives, output_file_obj_csv)
cat("L1 objectives saved as CSV:", output_file_obj_csv, "\n")

# Save hospital metadata
output_file_meta_rds <- file.path(source_folder, "hospital_metadata.rds")
saveRDS(hospital_metadata, output_file_meta_rds)
cat("Hospital metadata saved as RDS:", output_file_meta_rds, "\n")

output_file_meta_csv <- file.path(source_folder, "hospital_metadata.csv")
write_csv(hospital_metadata, output_file_meta_csv)
cat("Hospital metadata saved as CSV:", output_file_meta_csv, "\n")

cat("\n========================================\n")
cat("=== PROCESSING COMPLETE - READY FOR ANALYSIS ===\n")
cat("========================================\n")

# Return both dataframes
list(
  l1_objectives = l1_objectives,
  hospital_metadata = hospital_metadata
)