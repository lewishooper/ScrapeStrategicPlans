# ==============================================================================
# EXTRACTION OUTPUT PARSER - Version 2 (Enhanced Compatibility)
# ==============================================================================
# Purpose: Parse Phase3_L1_Extraction text files into structured R dataframes
# Author: Skip & Claude  
# Created: 2026-01-31
# Version: 2.0 - Enhanced error handling and compatibility
# ==============================================================================

# ==============================================================================
# PACKAGE LOADING WITH ERROR HANDLING
# ==============================================================================

cat("Loading required packages...\n")

required_packages <- c("dplyr", "tibble", "stringr", "readr")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    stop(sprintf("Package '%s' is required but not installed. Please run: install.packages('%s')", pkg, pkg))
  }
}

cat("All packages loaded successfully.\n\n")

# ==============================================================================
# UTILITY FUNCTIONS
# ==============================================================================

# Safe string repeat function (compatible with older R versions)
str_repeat <- function(str, times) {
  paste(rep(str, times), collapse = "")
}

# ==============================================================================
# MAIN PARSING FUNCTION
# ==============================================================================

#' Parse Phase 3 L1 Extraction file into two dataframes
#' 
#' @param file_path Path to Phase3_L1_Extraction_YYYYMMDD.txt file
#' @param verbose Print progress messages (default: TRUE)
#' 
#' @return List with two tibbles:
#'   - hospitals: One row per hospital (metadata)
#'   - priorities: Multiple rows per hospital (strategic priorities)
#'   
parse_extraction_file <- function(file_path, verbose = TRUE) {
  
  if (verbose) {
    cat("\n")
    cat(str_repeat("=", 80), "\n")
    cat("PARSING EXTRACTION FILE\n")
    cat(str_repeat("=", 80), "\n")
    cat(sprintf("File: %s\n", basename(file_path)))
  }
  
  # Check file exists
  if (!file.exists(file_path)) {
    stop(sprintf("File not found: %s", file_path))
  }
  
  # Read entire file
  lines <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
  
  if (verbose) {
    cat(sprintf("Total lines: %s\n", format(length(lines), big.mark = ",")))
  }
  
  # Find hospital boundaries
  hospital_starts <- grep("^HOSPITAL [0-9]+ OF [0-9]+", lines)
  
  if (verbose) {
    cat(sprintf("Hospital sections found: %d\n", length(hospital_starts)))
  }
  
  # Initialize storage
  all_hospitals <- list()
  all_priorities <- list()
  
  # Parse each hospital
  for (i in seq_along(hospital_starts)) {
    
    start_line <- hospital_starts[i]
    
    # Determine end line
    if (i < length(hospital_starts)) {
      end_line <- hospital_starts[i + 1] - 1
    } else {
      end_line <- length(lines)
    }
    
    # Extract this hospital's section
    hospital_lines <- lines[start_line:end_line]
    
    # Parse this hospital
    parsed <- parse_hospital_section(hospital_lines, hospital_num = i, verbose = verbose)
    
    if (!is.null(parsed)) {
      all_hospitals[[i]] <- parsed$hospital
      all_priorities <- c(all_priorities, parsed$priorities)
    }
  }
  
  # Convert to dataframes
  hospitals_df <- bind_rows(all_hospitals)
  priorities_df <- bind_rows(all_priorities)
  
  # Validation
  if (verbose) {
    cat("\n")
    cat(str_repeat("=", 80), "\n")
    cat("PARSING COMPLETE\n")
    cat(str_repeat("=", 80), "\n")
    cat(sprintf("Hospitals parsed: %d\n", nrow(hospitals_df)))
    cat(sprintf("Priorities parsed: %d\n", nrow(priorities_df)))
    cat(sprintf("Unique FAC codes: %d\n", length(unique(hospitals_df$fac))))
    
    # Check for duplicates
    dup_facs <- hospitals_df %>%
      group_by(fac) %>%
      filter(n() > 1) %>%
      pull(fac) %>%
      unique()
    
    if (length(dup_facs) > 0) {
      cat(sprintf("\nWARNING: %d duplicate FAC codes found:\n", length(dup_facs)))
      cat(paste(dup_facs, collapse = ", "), "\n")
    } else {
      cat("No duplicate FAC codes\n")
    }
    
    cat(str_repeat("=", 80), "\n\n")
  }
  
  return(list(
    hospitals = hospitals_df,
    priorities = priorities_df
  ))
}

# ==============================================================================
# PARSE SINGLE HOSPITAL SECTION
# ==============================================================================

parse_hospital_section <- function(hospital_lines, hospital_num, verbose = FALSE) {
  
  # Find FAC line
  fac_line <- grep("^FAC: ", hospital_lines)
  
  if (length(fac_line) == 0) {
    if (verbose) cat(sprintf("  Hospital %d: No FAC found, skipping\n", hospital_num))
    return(NULL)
  }
  
  # Use last FAC occurrence (actual data, not wrapper)
  fac_line <- fac_line[length(fac_line)]
  
  # Extract FAC
  fac <- str_trim(str_remove(hospital_lines[fac_line], "^FAC:\\s*"))
  
  # Initialize hospital record
  hospital <- tibble(
    fac = fac,
    hospital_name_file = NA_character_,
    hospital_name_text = NA_character_,
    extraction_date = NA_character_,
    plan_start = NA_character_,
    plan_end = NA_character_,
    plan_classification = NA_character_,
    document_structure = NA_character_,
    document_issues = NA_character_,
    l2_present = NA_character_,
    specificity = NA_character_,
    scope = NA_character_,
    overall_quality_notes = NA_character_
  )
  
  # Extract header fields
  search_lines <- hospital_lines[fac_line:min(fac_line + 50, length(hospital_lines))]
  
  hospital$hospital_name_file <- extract_field(search_lines, "HOSPITAL_NAME_FILE")
  hospital$hospital_name_text <- extract_field(search_lines, "HOSPITAL_NAME_TEXT")
  hospital$extraction_date <- extract_field(search_lines, "EXTRACTION_DATE")
  hospital$plan_start <- extract_field(search_lines, "PLAN_START")
  hospital$plan_end <- extract_field(search_lines, "PLAN_END")
  hospital$plan_classification <- extract_field(search_lines, "PLAN_CLASSIFICATION")
  hospital$document_structure <- extract_field(search_lines, "DOCUMENT_STRUCTURE")
  hospital$document_issues <- extract_field(search_lines, "DOCUMENT_ISSUES")
  hospital$l2_present <- extract_field(search_lines, "L2_PRESENT")
  
  # Extract quality indicators
  quality_start <- grep("QUALITY INDICATORS", hospital_lines)
  if (length(quality_start) > 0) {
    quality_lines <- hospital_lines[quality_start[1]:min(quality_start[1] + 20, length(hospital_lines))]
    hospital$specificity <- extract_field(quality_lines, "SPECIFICITY")
    hospital$scope <- extract_field(quality_lines, "SCOPE")
    hospital$overall_quality_notes <- extract_field(quality_lines, "OVERALL_QUALITY_NOTES")
  }
  
  # Extract priorities
  priorities <- extract_priorities(hospital_lines, fac)
  
  return(list(
    hospital = hospital,
    priorities = priorities
  ))
}

# ==============================================================================
# HELPER: EXTRACT SINGLE FIELD VALUE
# ==============================================================================

extract_field <- function(lines, field_name) {
  pattern <- paste0("^", field_name, ":\\s*")
  matching_lines <- grep(pattern, lines, value = TRUE)
  
  if (length(matching_lines) == 0) {
    return(NA_character_)
  }
  
  value <- str_trim(str_remove(matching_lines[length(matching_lines)], pattern))
  
  if (value == "" || tolower(value) == "nf") {
    return(NA_character_)
  }
  
  return(value)
}

# ==============================================================================
# HELPER: EXTRACT ALL PRIORITIES
# ==============================================================================

extract_priorities <- function(hospital_lines, fac) {
  
  title_lines <- grep("^L1\\.[0-9]+_TITLE:", hospital_lines)
  
  if (length(title_lines) == 0) {
    return(list())
  }
  
  priorities_list <- list()
  
  for (i in seq_along(title_lines)) {
    title_line_num <- title_lines[i]
    
    # Extract priority number
    priority_num <- as.integer(str_extract(hospital_lines[title_line_num], "(?<=L1\\.)[0-9]+"))
    
    # Extract title
    title <- str_trim(str_remove(hospital_lines[title_line_num], "^L1\\.[0-9]+_TITLE:\\s*"))
    
    # Extract text
    text_start <- title_line_num + 1
    text_end <- text_start
    
    for (j in (text_start + 1):min(text_start + 100, length(hospital_lines))) {
      line <- hospital_lines[j]
      
      if (grepl("^---+$", line) || 
          grepl("^=+$", line) ||
          grepl("^L1\\.[0-9]+_TITLE:", line) ||
          grepl("^QUALITY INDICATORS", line)) {
        text_end <- j - 1
        break
      }
    }
    
    # Combine text lines
    text_lines <- hospital_lines[text_start:text_end]
    text_lines <- str_remove(text_lines, "^L1\\.[0-9]+_TEXT:\\s*")
    priority_text <- paste(text_lines, collapse = "\n")
    priority_text <- str_trim(priority_text)
    
    if (tolower(priority_text) == "nf" || priority_text == "") {
      priority_text <- NA_character_
    }
    
    priority <- tibble(
      fac = fac,
      priority_number = priority_num,
      priority_title = title,
      priority_text = priority_text
    )
    
    priorities_list[[i]] <- priority
  }
  
  return(priorities_list)
}

# ==============================================================================
# VALIDATION FUNCTIONS
# ==============================================================================

validate_extraction_data <- function(hospitals, priorities) {
  
  cat("\n")
  cat(str_repeat("=", 80), "\n")
  cat("DATA VALIDATION REPORT\n")
  cat(str_repeat("=", 80), "\n\n")
  
  issues <- list()
  
  # 1. FAC format
  cat("1. FAC FORMAT CHECK\n")
  invalid_facs <- hospitals %>%
    filter(!grepl("^[0-9]{3}$", fac))
  
  if (nrow(invalid_facs) > 0) {
    cat(sprintf("   ISSUE: %d hospitals have invalid FAC format\n", nrow(invalid_facs)))
    issues$invalid_fac_format <- invalid_facs$fac
  } else {
    cat("   OK: All FAC codes are 3-digit format\n")
  }
  
  # 2. Duplicates
  cat("\n2. DUPLICATE CHECK\n")
  dup_facs <- hospitals %>%
    group_by(fac) %>%
    filter(n() > 1) %>%
    pull(fac) %>%
    unique()
  
  if (length(dup_facs) > 0) {
    cat(sprintf("   ISSUE: %d duplicate FAC codes\n", length(dup_facs)))
    issues$duplicate_facs <- dup_facs
  } else {
    cat("   OK: No duplicates\n")
  }
  
  # 3. Missing fields
  cat("\n3. MISSING CRITICAL FIELDS\n")
  missing_name <- sum(is.na(hospitals$hospital_name_text))
  
  if (missing_name > 0) {
    cat(sprintf("   WARNING: %d hospitals missing hospital_name_text\n", missing_name))
    issues$missing_hospital_name <- missing_name
  } else {
    cat("   OK: All hospitals have names\n")
  }
  
  # 4. Referential integrity
  cat("\n4. REFERENTIAL INTEGRITY\n")
  orphaned <- priorities %>%
    anti_join(hospitals, by = "fac")
  
  if (nrow(orphaned) > 0) {
    cat(sprintf("   ISSUE: %d orphaned priorities\n", nrow(orphaned)))
    issues$orphaned_priorities <- unique(orphaned$fac)
  } else {
    cat("   OK: All priorities link to hospitals\n")
  }
  
  # 5. Completeness
  cat("\n5. COMPLETENESS CHECK\n")
  no_priorities <- hospitals %>%
    anti_join(priorities, by = "fac")
  
  if (nrow(no_priorities) > 0) {
    cat(sprintf("   WARNING: %d hospitals have no priorities\n", nrow(no_priorities)))
    issues$hospitals_no_priorities <- no_priorities$fac
  } else {
    cat("   OK: All hospitals have priorities\n")
  }
  
  cat("\n")
  cat(str_repeat("=", 80), "\n")
  if (length(issues) == 0) {
    cat("VALIDATION PASSED\n")
  } else {
    cat(sprintf("VALIDATION COMPLETED - %d issue types found\n", length(issues)))
  }
  cat(str_repeat("=", 80), "\n\n")
  
  return(list(
    passed = length(issues) == 0,
    issues = issues
  ))
}

# ==============================================================================
# SUMMARY FUNCTION
# ==============================================================================

print_data_summary <- function(hospitals, priorities) {
  
  cat("\n")
  cat(str_repeat("=", 80), "\n")
  cat("DATA SUMMARY\n")
  cat(str_repeat("=", 80), "\n\n")
  
  cat("HOSPITALS TABLE:\n")
  cat(sprintf("  Total hospitals: %d\n", nrow(hospitals)))
  cat(sprintf("  Unique FACs: %d\n", length(unique(hospitals$fac))))
  
  plan_class_summary <- hospitals %>%
    count(plan_classification, name = "count") %>%
    arrange(desc(count))
  
  cat("\n  Plan Classification:\n")
  for (i in 1:nrow(plan_class_summary)) {
    cat(sprintf("    %s: %d\n", 
                ifelse(is.na(plan_class_summary$plan_classification[i]), "NA", plan_class_summary$plan_classification[i]),
                plan_class_summary$count[i]))
  }
  
  cat("\n\nPRIORITIES TABLE:\n")
  cat(sprintf("  Total priorities: %d\n", nrow(priorities)))
  
  priorities_per_hospital <- priorities %>%
    count(fac, name = "num_priorities")
  
  cat(sprintf("  Average per hospital: %.1f\n", mean(priorities_per_hospital$num_priorities)))
  cat(sprintf("  Min: %d, Max: %d\n", 
              min(priorities_per_hospital$num_priorities),
              max(priorities_per_hospital$num_priorities)))
  
  cat("\n")
  cat(str_repeat("=", 80), "\n\n")
}

# ==============================================================================
# EXPORT FUNCTION
# ==============================================================================

parse_and_export <- function(
    input_file,
    output_dir = dirname(input_file),
    export_csv = TRUE,
    export_rds = TRUE,
    run_validation = TRUE
) {
  
  # Parse file
  parsed <- parse_extraction_file(input_file, verbose = TRUE)
  
  hospitals <- parsed$hospitals
  priorities <- parsed$priorities
  
  # Print summary
  print_data_summary(hospitals, priorities)
  
  # Validate
  if (run_validation) {
    validation <- validate_extraction_data(hospitals, priorities)
  }
  
  # Export
  if (export_csv || export_rds) {
    
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    input_basename <- tools::file_path_sans_ext(basename(input_file))
    
    cat("\n")
    cat(str_repeat("=", 80), "\n")
    cat("EXPORTING DATA\n")
    cat(str_repeat("=", 80), "\n")
    
    if (export_csv) {
      hospitals_csv <- file.path(output_dir, paste0(input_basename, "_hospitals.csv"))
      priorities_csv <- file.path(output_dir, paste0(input_basename, "_priorities.csv"))
      
      write_csv(hospitals, hospitals_csv)
      write_csv(priorities, priorities_csv)
      
      cat(sprintf("Saved: %s\n", basename(hospitals_csv)))
      cat(sprintf("Saved: %s\n", basename(priorities_csv)))
    }
    
    if (export_rds) {
      rds_file <- file.path(output_dir, paste0(input_basename, "_parsed.rds"))
      saveRDS(list(hospitals = hospitals, priorities = priorities), rds_file)
      cat(sprintf("Saved: %s\n", basename(rds_file)))
    }
    
    cat(str_repeat("=", 80), "\n\n")
  }
  
  return(list(
    hospitals = hospitals,
    priorities = priorities
  ))
}

# ==============================================================================
# STARTUP MESSAGE
# ==============================================================================

cat("\n")
cat(str_repeat("=", 80), "\n")
cat("EXTRACTION OUTPUT PARSER - READY (Version 2)\n")
cat(str_repeat("=", 80), "\n")
cat("\nR Version: ", as.character(getRversion()), "\n")
cat("Working Directory: ", getwd(), "\n\n")
cat("USAGE:\n\n")
cat('  data <- parse_and_export("Phase3_L1_Extraction_20260131.txt")\n\n')
cat("Type ?parse_extraction_file for help\n")
cat(str_repeat("=", 80), "\n\n")