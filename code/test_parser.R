# ==============================================================================
# TEST SCRIPT FOR EXTRACTION PARSER
# ==============================================================================
# Purpose: Test parse_extraction_output.R on Phase3_L1_Extraction_20260131.txt
# Test Hospitals: 592, 947, 979, 952
# ==============================================================================

library(dplyr)
library(tibble)
getwd()
# Source the parser
source("parse_extraction_output.R")

cat("\n")
cat(strrep("=", 80), "\n")
cat("PARSER TEST SCRIPT\n")
cat(strrep("=", 80), "\n\n")

# ==============================================================================
# TEST 1: PARSE THE FILE
# ==============================================================================

cat("TEST 1: Parsing extraction file...\n")
cat(strrep("-", 80), "\n\n")

# Parse the file from project knowledge
input_file <- "/mnt/project/Phase3_L1_Extraction_20260131.txt"

parsed <- parse_extraction_file(input_file, verbose = TRUE)

hospitals <- parsed$hospitals
priorities <- parsed$priorities

# ==============================================================================
# TEST 2: CHECK TEST HOSPITALS
# ==============================================================================

cat("\n")
cat("TEST 2: Checking test hospitals (592, 947, 979, 952)...\n")
cat(strrep("-", 80), "\n\n")

test_facs <- c("592", "947", "979", "952")

for (fac in test_facs) {
  cat(sprintf("FAC %s:\n", fac))
  
  # Check hospital record
  hospital_rec <- hospitals %>% filter(fac == !!fac)
  
  if (nrow(hospital_rec) == 0) {
    cat("  ✗ NOT FOUND in hospitals table\n\n")
    next
  }
  
  if (nrow(hospital_rec) > 1) {
    cat(sprintf("  ✗ DUPLICATE - appears %d times\n\n", nrow(hospital_rec)))
    next
  }
  
  cat("  ✓ Found in hospitals table\n")
  cat(sprintf("    Name: %s\n", hospital_rec$hospital_name_text))
  cat(sprintf("    Plan: %s - %s (%s)\n", 
              ifelse(is.na(hospital_rec$plan_start), "?", hospital_rec$plan_start),
              ifelse(is.na(hospital_rec$plan_end), "?", hospital_rec$plan_end),
              ifelse(is.na(hospital_rec$plan_classification), "?", hospital_rec$plan_classification)))
  
  # Check priorities
  priority_recs <- priorities %>% filter(fac == !!fac)
  
  cat(sprintf("    Priorities: %d\n", nrow(priority_recs)))
  
  if (nrow(priority_recs) > 0) {
    for (i in 1:min(3, nrow(priority_recs))) {
      p <- priority_recs[i, ]
      cat(sprintf("      L1.%d: %s\n", p$priority_number, 
                  substr(p$priority_title, 1, 60)))
    }
    if (nrow(priority_recs) > 3) {
      cat(sprintf("      ... and %d more\n", nrow(priority_recs) - 3))
    }
  }
  
  cat("\n")
}

# ==============================================================================
# TEST 3: RUN VALIDATION
# ==============================================================================

cat("\n")
cat("TEST 3: Running validation checks...\n")
cat(strrep("-", 80), "\n")

validation <- validate_extraction_data(hospitals, priorities)

# ==============================================================================
# TEST 4: SUMMARY STATISTICS
# ==============================================================================

cat("\n")
cat("TEST 4: Summary statistics...\n")
cat(strrep("-", 80), "\n")

print_data_summary(hospitals, priorities)

# ==============================================================================
# TEST 5: DETAILED INSPECTION OF ONE TEST HOSPITAL
# ==============================================================================

cat("\n")
cat("TEST 5: Detailed inspection of FAC 947 (UHN)...\n")
cat(strrep("-", 80), "\n\n")

fac_947_hospital <- hospitals %>% filter(fac == "947")
fac_947_priorities <- priorities %>% filter(fac == "947")

cat("HOSPITAL RECORD:\n")
print(glimpse(fac_947_hospital))

cat("\n\nPRIORITIES:\n")
for (i in 1:nrow(fac_947_priorities)) {
  p <- fac_947_priorities[i, ]
  cat(sprintf("\nL1.%d: %s\n", p$priority_number, p$priority_title))
  cat("TEXT: ")
  if (is.na(p$priority_text)) {
    cat("(no text - NA)\n")
  } else {
    cat(substr(p$priority_text, 1, 200), "...\n")
  }
}

# ==============================================================================
# TEST 6: CHECK FOR EXPECTED ISSUES
# ==============================================================================

cat("\n\n")
cat("TEST 6: Checking for known patterns...\n")
cat(strrep("-", 80), "\n\n")

# Check hospitals with "nf" plan classification
nf_plans <- hospitals %>%
  filter(is.na(plan_classification) | plan_classification == "nf")

cat(sprintf("Hospitals with NA/nf plan_classification: %d\n", nrow(nf_plans)))
if (nrow(nf_plans) > 0 && nrow(nf_plans) <= 10) {
  cat("  FACs:", paste(nf_plans$fac, collapse = ", "), "\n")
}

# Check hospitals with no priorities
no_priorities <- hospitals %>%
  anti_join(priorities, by = "fac")

cat(sprintf("\nHospitals with no priorities extracted: %d\n", nrow(no_priorities)))
if (nrow(no_priorities) > 0 && nrow(no_priorities) <= 10) {
  cat("  FACs:", paste(no_priorities$fac, collapse = ", "), "\n")
  cat("  Names:", paste(substr(no_priorities$hospital_name_text, 1, 30), collapse = ", "), "\n")
}

# ==============================================================================
# TEST 7: SAMPLE DATA FOR VERIFICATION
# ==============================================================================

cat("\n\n")
cat("TEST 7: Sample hospital records (first 5)...\n")
cat(strrep("-", 80), "\n\n")

print(hospitals %>% 
        select(fac, hospital_name_text, plan_start, plan_end, plan_classification) %>%
        head(5))

cat("\n\nSample priorities (first 10)...\n")
print(priorities %>%
        select(fac, priority_number, priority_title) %>%
        head(10))

# ==============================================================================
# FINAL SUMMARY
# ==============================================================================

cat("\n\n")
cat(strrep("=", 80), "\n")
cat("TEST COMPLETED\n")
cat(strrep("=", 80), "\n\n")

cat("RESULTS:\n")
cat(sprintf("  Hospitals parsed: %d\n", nrow(hospitals)))
cat(sprintf("  Priorities parsed: %d\n", nrow(priorities)))
cat(sprintf("  Test hospitals found: %d/4\n", sum(test_facs %in% hospitals$fac)))
cat(sprintf("  Validation: %s\n", ifelse(validation$passed, "PASSED ✓", "ISSUES FOUND ⚠")))

if (!validation$passed) {
  cat("\nISSUES DETECTED:\n")
  for (issue_name in names(validation$issues)) {
    cat(sprintf("  - %s\n", issue_name))
  }
}

cat("\n")
cat(strrep("=", 80), "\n\n")

# Return data for further inspection
cat("Data stored in variables:\n")
cat("  hospitals  - Hospital metadata table\n")
cat("  priorities - Strategic priorities table\n")
cat("  validation - Validation results\n\n")
