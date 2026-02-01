
source("E:/Hospital_Strategic_Plans/code/quick_yaml_check.R")


# Count Hospitals in base_hospitals_validated.yaml
# Purpose: See how many validated hospitals are available
# Date: 2025-12-04

library(yaml)

cat("\n", strrep("=", 70), "\n")
cat("COUNTING HOSPITALS IN BASE FILES\n")
cat(strrep("=", 70), "\n\n")

# File paths
master_yaml <- "E:/Hospital_Strategic_Plans/code/base_hospitals_validated.yaml"
working_yaml <- "E:/Hospital_Strategic_Plans/code/Hospital_strategy.yaml"

# Check master file
cat("[FILE 1] base_hospitals_validated.yaml (Master/Source)\n")
cat(strrep("-", 70), "\n")

if (!file.exists(master_yaml)) {
  cat("✗ File not found:", master_yaml, "\n\n")
} else {
  master_data <- read_yaml(master_yaml)
  
  # Handle both structures
  if (is.null(master_data$hospitals)) {
    master_hospitals <- master_data
  } else {
    master_hospitals <- master_data$hospitals
  }
  
  total_master <- length(master_hospitals)
  cat("Total hospitals in master file:", total_master, "\n")
  
  # Count validated
  validated_count <- sum(sapply(master_hospitals, function(h) {
    !is.null(h$base_url_validated) && 
      (h$base_url_validated == "yes" || h$base_url_validated == TRUE)
  }))
  cat("Hospitals with base_url_validated = 'yes':", validated_count, "\n")
  
  # Count robots allowed
  robots_allowed_count <- sum(sapply(master_hospitals, function(h) {
    !is.null(h$robots_allowed) && 
      (h$robots_allowed == "yes" || h$robots_allowed == TRUE)
  }))
  cat("Hospitals with robots_allowed = 'yes':", robots_allowed_count, "\n")
  
  # Count BOTH validated AND robots allowed
  both_count <- sum(sapply(master_hospitals, function(h) {
    !is.null(h$base_url_validated) && 
      (h$base_url_validated == "yes" || h$base_url_validated == TRUE) &&
      !is.null(h$robots_allowed) &&
      (h$robots_allowed == "yes" || h$robots_allowed == TRUE)
  }))
  cat("Hospitals with BOTH validated AND robots_allowed:", both_count, "\n\n")
  
  # Show first few FAC numbers
  cat("First 10 hospital FAC numbers:\n")
  for (i in 1:min(10, total_master)) {
    h <- master_hospitals[[i]]
    validated <- if (!is.null(h$base_url_validated) && 
                     (h$base_url_validated == "yes" || h$base_url_validated == TRUE)) "✓" else "✗"
    robots <- if (!is.null(h$robots_allowed) && 
                  (h$robots_allowed == "yes" || h$robots_allowed == TRUE)) "✓" else "✗"
    cat("  ", i, ". FAC:", h$FAC, "- Validated:", validated, "- Robots:", robots, "\n")
  }
  cat("  ...\n\n")
}

# Check working file
cat("[FILE 2] Hospital_strategy.yaml (Working File)\n")
cat(strrep("-", 70), "\n")

if (!file.exists(working_yaml)) {
  cat("✗ File not found:", working_yaml, "\n\n")
} else {
  working_data <- read_yaml(working_yaml)
  
  # Handle both structures
  if (is.null(working_data$hospitals)) {
    working_hospitals <- working_data
  } else {
    working_hospitals <- working_data$hospitals
  }
  
  total_working <- length(working_hospitals)
  cat("Total hospitals in working file:", total_working, "\n\n")
  
  # Show first few FAC numbers
  cat("First 10 hospital FAC numbers:\n")
  for (i in 1:min(10, total_working)) {
    h <- working_hospitals[[i]]
    cat("  ", i, ". FAC:", h$FAC, "- Name:", substr(h$name, 1, 40), "\n")
  }
  if (total_working > 10) {
    cat("  ...\n")
  }
  cat("\nLast hospital FAC:", working_hospitals[[total_working]]$FAC, "\n\n")
}

# Summary
cat(strrep("=", 70), "\n")
cat("SUMMARY\n")
cat(strrep("=", 70), "\n\n")

if (exists("both_count") && exists("total_working")) {
  cat("Hospitals available to process (validated & robots OK):", both_count, "\n")
  cat("Hospitals currently in working file:", total_working, "\n")
  
  if (total_working < both_count) {
    difference <- both_count - total_working
    cat("Additional hospitals that could be added:", difference, "\n\n")
    cat("ACTION: You have", difference, "more validated hospitals available\n")
    cat("        These can be added to Hospital_strategy.yaml\n")
  } else if (total_working == both_count) {
    cat("\n✓ Working file has all validated hospitals from master\n")
  } else {
    cat("\n⚠ Working file has MORE hospitals than master?\n")
    cat("  This might mean master file is out of date\n")
  }
} else {
  cat("Could not complete comparison - check file paths\n")
}

cat("\n", strrep("=", 70), "\n")