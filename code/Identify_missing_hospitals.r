# Identify Missing Hospitals
# Purpose: Find validated hospitals in base_hospitals_validated.yaml that are not yet
#          in Hospital_strategy.yaml and export them for manual review
# Author: Hospital Strategic Plans Project
# Date: 2025-12-04

library(yaml)
library(dplyr)

cat("\n", strrep("=", 70), "\n")
cat("IDENTIFY MISSING HOSPITALS\n")
cat("Find validated hospitals not yet in working file\n")
cat(strrep("=", 70), "\n\n")

# Configuration ----
BASE_DIR <- "E:/Hospital_Strategic_Plans"
master_yaml <- file.path(BASE_DIR, "code/base_hospitals_validated.yaml")
working_yaml <- file.path(BASE_DIR, "code/Hospital_strategy.yaml")
output_yaml <- file.path(BASE_DIR, "Outputs/MissingHospitals.yaml")

# Ensure Outputs directory exists
output_dir <- file.path(BASE_DIR, "Outputs")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("[SETUP] Created Outputs directory\n\n")
}

# Step 1: Read Master File ----
cat("[STEP 1] Reading base_hospitals_validated.yaml...\n")

if (!file.exists(master_yaml)) {
  stop("Master file not found: ", master_yaml)
}

master_data <- read_yaml(master_yaml)

# Handle structure (has 'hospitals:' wrapper)
if (is.null(master_data$hospitals)) {
  master_hospitals <- master_data
  cat("  Structure: Direct list\n")
} else {
  master_hospitals <- master_data$hospitals
  cat("  Structure: Has 'hospitals:' wrapper\n")
}

total_master <- length(master_hospitals)
cat("  Total hospitals in master:", total_master, "\n\n")

# Step 2: Read Working File ----
cat("[STEP 2] Reading Hospital_strategy.yaml...\n")

if (!file.exists(working_yaml)) {
  stop("Working file not found: ", working_yaml)
}

working_data <- read_yaml(working_yaml)

# Handle structure (direct list, no wrapper)
if (is.null(working_data$hospitals)) {
  working_hospitals <- working_data
  cat("  Structure: Direct list (no wrapper)\n")
} else {
  working_hospitals <- working_data$hospitals
  cat("  Structure: Has 'hospitals:' wrapper\n")
}

total_working <- length(working_hospitals)
cat("  Total hospitals in working file:", total_working, "\n\n")

# Step 3: Filter Master to Validated Only ----
cat("[STEP 3] Filtering master to validated hospitals only...\n")
cat("  Criteria: base_url_validated = 'yes'\n")
cat("  Note: Including ALL validated (even robots_allowed='no')\n\n")

validated_hospitals <- list()
validated_count <- 0
skipped_not_validated <- 0

for (hospital in master_hospitals) {
  # Check if validated
  is_validated <- !is.null(hospital$base_url_validated) && 
    (hospital$base_url_validated == "yes" || hospital$base_url_validated == TRUE)
  
  if (is_validated) {
    validated_hospitals[[length(validated_hospitals) + 1]] <- hospital
    validated_count <- validated_count + 1
  } else {
    skipped_not_validated <- skipped_not_validated + 1
  }
}

cat("  Validated hospitals:", validated_count, "\n")
cat("  Not validated (skipped):", skipped_not_validated, "\n\n")

# Step 4: Extract FAC Numbers ----
cat("[STEP 4] Extracting FAC numbers for comparison...\n")

# Get FACs from validated master hospitals
validated_facs <- sapply(validated_hospitals, function(h) as.character(h$FAC))
cat("  FACs in validated master:", length(validated_facs), "\n")

# Get FACs from working file
working_facs <- sapply(working_hospitals, function(h) as.character(h$FAC))
cat("  FACs in working file:", length(working_facs), "\n\n")

# Step 5: Find Missing Hospitals ----
cat("[STEP 5] Identifying missing hospitals...\n")

missing_facs <- setdiff(validated_facs, working_facs)
missing_count <- length(missing_facs)

cat("  Missing hospitals (validated but not in working file):", missing_count, "\n")

if (missing_count == 0) {
  cat("\n✓ No missing hospitals found!\n")
  cat("  All validated hospitals from master are already in working file.\n\n")
  cat(strrep("=", 70), "\n")
  stop("No action needed - working file is complete")
}

cat("  Missing FAC numbers:\n")
cat("   ", paste(sort(missing_facs), collapse = ", "), "\n\n")

# Get the full hospital records for missing FACs
missing_hospitals <- list()
for (hospital in validated_hospitals) {
  if (as.character(hospital$FAC) %in% missing_facs) {
    missing_hospitals[[length(missing_hospitals) + 1]] <- hospital
  }
}

# Step 6: Check for robots_allowed status ----
cat("[STEP 6] Checking robots_allowed status of missing hospitals...\n")

robots_yes <- 0
robots_no <- 0
robots_unknown <- 0

for (hospital in missing_hospitals) {
  if (!is.null(hospital$robots_allowed)) {
    if (hospital$robots_allowed == "yes" || hospital$robots_allowed == TRUE) {
      robots_yes <- robots_yes + 1
    } else {
      robots_no <- robots_no + 1
    }
  } else {
    robots_unknown <- robots_unknown + 1
  }
}

cat("  robots_allowed = 'yes':", robots_yes, "\n")
cat("  robots_allowed = 'no':", robots_no, "(will need manual handling)\n")
cat("  robots_allowed = unknown:", robots_unknown, "\n\n")

# Step 7: Transform Structure ----
cat("[STEP 7] Transforming to Hospital_strategy.yaml format...\n")

# Function to create empty strategy_search structure
create_empty_strategy_search <- function() {
  list(
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
    strategy_notes = ""
  )
}

# Transform each missing hospital
transformed_hospitals <- list()

for (hospital in missing_hospitals) {
  transformed <- list(
    FAC = hospital$FAC,
    name = hospital$name,
    hospital_type = hospital$hospital_type,
    base_url = hospital$base_url,
    strategy_search = create_empty_strategy_search()
  )
  
  transformed_hospitals[[length(transformed_hospitals) + 1]] <- transformed
}

cat("  Transformed", length(transformed_hospitals), "hospital records\n")
cat("  Structure: Direct list (matches Hospital_strategy.yaml)\n")
cat("  Removed: base_url_validated, robots_allowed, last_validated,\n")
cat("           leadership_url, notes, status\n")
cat("  Added: strategy_search with empty/default values\n\n")

# Step 8: Write Output File ----
cat("[STEP 8] Writing to MissingHospitals.yaml...\n")

# Write as direct list (no wrapper)
write_yaml(transformed_hospitals, output_yaml)

cat("  ✓ File created:", output_yaml, "\n")
cat("  Format: Direct list (no 'hospitals:' wrapper)\n")
cat("  Ready for manual review in VS Code\n\n")

# Step 9: Summary Report ----
cat(strrep("=", 70), "\n")
cat("SUMMARY REPORT\n")
cat(strrep("=", 70), "\n\n")

cat("Input Files:\n")
cat("  Master file:", basename(master_yaml), "\n")
cat("  Working file:", basename(working_yaml), "\n\n")

cat("Hospital Counts:\n")
cat("  Total in master:", total_master, "\n")
cat("  Validated in master:", validated_count, "\n")
cat("  Currently in working file:", total_working, "\n")
cat("  Missing (exported):", missing_count, "\n\n")

cat("Missing Hospitals Breakdown:\n")
cat("  robots_allowed = 'yes':", robots_yes, "(ready to process)\n")
cat("  robots_allowed = 'no':", robots_no, "(needs manual handling)\n")
if (robots_unknown > 0) {
  cat("  robots_allowed = unknown:", robots_unknown, "(check manually)\n")
}
cat("\n")

cat("Output File:\n")
cat("  Location:", output_yaml, "\n")
cat("  Hospitals exported:", missing_count, "\n")
cat("  Format: Matches Hospital_strategy.yaml structure\n\n")

cat("Missing FAC Numbers (sorted):\n")
missing_facs_sorted <- sort(missing_facs)
# Print in rows of 10
for (i in seq(1, length(missing_facs_sorted), by = 10)) {
  end_idx <- min(i + 9, length(missing_facs_sorted))
  cat("  ", paste(missing_facs_sorted[i:end_idx], collapse = ", "), "\n")
}
cat("\n")

# Step 10: Next Steps Guidance ----
cat(strrep("=", 70), "\n")
cat("NEXT STEPS\n")
cat(strrep("=", 70), "\n\n")

cat("1. REVIEW the output file:\n")
cat("   Open in VS Code: ", output_yaml, "\n")
cat("   Check each hospital card for accuracy\n\n")

cat("2. MANUAL EDITS (if needed):\n")
cat("   - Verify hospital names\n")
cat("   - Check base_url format\n")
cat("   - Note which have robots_allowed='no' (", robots_no, " hospitals)\n")
cat("   - Add any manual notes\n\n")

cat("3. MERGE with Hospital_strategy.yaml:\n")
cat("   You'll handle this manually by:\n")
cat("   a) Opening both files in VS Code\n")
cat("   b) Copy/paste approved hospitals from MissingHospitals.yaml\n")
cat("   c) Append to end of Hospital_strategy.yaml\n")
cat("   d) Optional: Sort by FAC number for organization\n\n")

cat("4. AFTER MERGE:\n")
cat("   Verify total count:\n")
cat("     data <- yaml::read_yaml('code/Hospital_strategy.yaml')\n")
cat("     length(data)  # Should be:", total_working + missing_count, "\n\n")

cat("5. RUN PHASE 2 with expanded list:\n")
cat("     CONFIG$test_sample_size <- 80  # or whatever you want\n")
cat("     results <- run_phase2()\n\n")

# Warning about robots_allowed='no'
if (robots_no > 0) {
  cat(strrep("=", 70), "\n")
  cat("⚠ IMPORTANT NOTE\n")
  cat(strrep("=", 70), "\n\n")
  cat("You have", robots_no, "hospital(s) with robots_allowed='no'\n")
  cat("These hospitals will need special handling:\n")
  cat("  - May block automated scraping\n")
  cat("  - Will likely require manual PDF download\n")
  cat("  - Consider adding manual_pdf_url before processing\n\n")
  cat("FACs with robots_allowed='no':\n")
  
  robots_no_facs <- c()
  for (hospital in missing_hospitals) {
    if (!is.null(hospital$robots_allowed) && 
        (hospital$robots_allowed == "no" || hospital$robots_allowed == FALSE)) {
      robots_no_facs <- c(robots_no_facs, as.character(hospital$FAC))
    }
  }
  
  cat("  ", paste(sort(robots_no_facs), collapse = ", "), "\n\n")
}

cat(strrep("=", 70), "\n")
cat("✓ SCRIPT COMPLETE\n")
cat(strrep("=", 70), "\n\n")

cat("Review the output file and proceed with manual steps when ready.\n\n")