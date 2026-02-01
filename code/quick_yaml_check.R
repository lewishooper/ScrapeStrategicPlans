# QUICK CHECK - How many hospitals in your YAML?
# Run this immediately to assess damage
# Date: 2025-12-04

library(yaml)

BASE_DIR <- "E:/Hospital_Strategic_Plans"
YAML_FILE <- file.path(BASE_DIR, "code/Hospital_strategy.yaml")

cat("\n", strrep("=", 70), "\n")
cat("YAML FILE STATUS CHECK\n")
cat(strrep("=", 70), "\n\n")

# Check 1: File exists?
if (!file.exists(YAML_FILE)) {
  cat("✗ CRITICAL: YAML file not found!\n")
  cat("  Expected:", YAML_FILE, "\n")
  stop("Cannot proceed")
}

cat("✓ YAML file found\n\n")

# Check 2: How many hospitals?
data <- read_yaml(YAML_FILE)
if (is.null(data$hospitals)) {
  hospitals <- data
} else {
  hospitals <- data$hospitals
}

total_hospitals <- length(hospitals)

cat("CURRENT STATE:\n")
cat("  Total hospitals in YAML:", total_hospitals, "\n\n")

# Check 3: Look for manually entered data
manual_pdf_count <- 0
content_type_count <- 0
html_count <- 0
comparison_result_count<-0

for (h in hospitals) {
  if (!is.null(h$strategy_search)) {
    # Check manual_pdf_url
    if (!is.null(h$strategy_search$manual_pdf_url) && 
        h$strategy_search$manual_pdf_url != "") {
      manual_pdf_count <- manual_pdf_count + 1
    }
    
    # Check content_type
    if (!is.null(h$strategy_search$content_type) && 
        h$strategy_search$content_type != "") {
      content_type_count <- content_type_count + 1
      
      if (h$strategy_search$content_type == "html") {
        html_count <- html_count + 1
      }
      if (h$strategy_search$comparison_result == "changed") {
        comparison_result_count <- comparison_result_count + 1
      }
    }
  }
}

cat("MANUAL DATA FOUND:\n")
cat("  Hospitals with manual_pdf_url:", manual_pdf_count, "\n")
cat("  Hospitals with content_type set:", content_type_count, "\n")
cat("  Hospitals with content_type='html':", html_count, "\n\n")
#cat("  Hospitals with comparision_result='changed':", comparison_result_count, "\n\n")
# Check 4: Look at backups
backup_dir <- file.path(BASE_DIR, "Backups")
if (dir.exists(backup_dir)) {
  backups <- list.files(backup_dir, pattern = "Hospital_strategy.*\\.yaml", 
                        full.names = TRUE)
  
  if (length(backups) > 0) {
    cat("BACKUPS AVAILABLE:\n")
    backup_info <- file.info(backups)
    backup_info <- backup_info[order(backup_info$mtime, decreasing = TRUE), ]
    
    for (i in 1:min(5, nrow(backup_info))) {
      backup_file <- rownames(backup_info)[i]
      backup_time <- format(backup_info$mtime[i], "%Y-%m-%d %H:%M:%S")
      cat("  ", i, ". ", basename(backup_file), "\n", sep = "")
      cat("     Modified:", backup_time, "\n")
      
      # Check how many hospitals in this backup
      tryCatch({
        backup_data <- read_yaml(backup_file)
        if (is.null(backup_data$hospitals)) {
          backup_hospitals <- backup_data
        } else {
          backup_hospitals <- backup_data$hospitals
        }
        cat("     Hospitals:", length(backup_hospitals), "\n")
      }, error = function(e) {
        cat("     [Cannot read backup]\n")
      })
    }
  } else {
    cat("⚠ No backups found in Backups folder\n")
  }
} else {
  cat("⚠ Backups folder does not exist\n")
}

# Assessment
cat("\n", strrep("=", 70), "\n")
cat("ASSESSMENT\n")
cat(strrep("=", 70), "\n")

if (total_hospitals >= 70) {
  cat("✓ GOOD: You have", total_hospitals, "hospitals (full dataset)\n")
} else if (total_hospitals >= 5 && total_hospitals < 70) {
  cat("✗ WARNING: You only have", total_hospitals, "hospitals\n")
  cat("  Expected: 70+ hospitals\n")
  cat("  --> Likely lost data in test mode run\n")
  cat("  --> RESTORE FROM BACKUP IMMEDIATELY\n")
} else {
  cat("✗ CRITICAL: Only", total_hospitals, "hospitals\n")
  cat("  --> File appears corrupted or empty\n")
  cat("  --> RESTORE FROM BACKUP IMMEDIATELY\n")
}

if (manual_pdf_count > 0 || content_type_count > 0) {
  cat("\n⚠ You have manually entered data that needs preservation:\n")
  cat("  Manual PDFs:", manual_pdf_count, "\n")
  cat("  Content types:", content_type_count, "\n")
  cat("  --> These will be lost on next run unless we fix the script\n")
}

cat(strrep("=", 70), "\n\n")

cat("NEXT STEPS:\n")
if (total_hospitals < 70) {
  cat("1. RESTORE from backup (see list above)\n")
  cat("2. Report back: Which backup should we use?\n")
  cat("3. Get fixed script before running again\n")
} else {
  cat("1. Report back the numbers above\n")
  cat("2. Get fixed script to preserve manual data\n")
  cat("3. DO NOT run in test mode until fixed\n")
}