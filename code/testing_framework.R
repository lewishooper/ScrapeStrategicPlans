# ============================================================================
# TESTING FRAMEWORK FOR REFACTORED API EXTRACTION SYSTEM
# ============================================================================
# Purpose: Comprehensive testing suite to validate refactored system
# Author: Skip & Claude
# Created: 2026-01-24
# Version: 1.0
# ============================================================================

library(dplyr)
library(yaml)

# ============================================================================
# TEST CONFIGURATION
# ============================================================================

TEST_CONFIG <- list(
  # Test hospitals - select 3-5 representative hospitals
  test_facs = c("592", "593", "597"),  # Small sample for quick tests
  
  # Paths
  base_dir = "E:/Hospital_Strategic_Plans",
  yaml_file = "E:/Hospital_Strategic_Plans/code/Hospital_strategy.yaml",
  strategic_plans_dir = "E:/Hospital_Strategic_Plans/strategic_plans",
  
  # Test output directory
  test_output_dir = "E:/Hospital_Strategic_Plans/outputs/testing",
  
  # Expected results
  expected_pdfs = 3,  # How many test hospitals should have PDFs
  expected_pages_min = 5,  # Minimum pages expected in valid PDF
  
  # API test settings
  api_test_enabled = TRUE,  # Set FALSE to skip API calls in testing
  api_delay_seconds = 10  # Shorter delay for testing
)

# ============================================================================
# TEST 1: FILE STRUCTURE VALIDATION
# ============================================================================

test_file_structure <- function() {
  
  cat("\n")
  cat(strrep("=", 80), "\n")
  cat("TEST 1: FILE STRUCTURE VALIDATION\n")
  cat(strrep("=", 80), "\n\n")
  
  tests_passed <- 0
  tests_failed <- 0
  
  # Test 1.1: Check base directory
  cat("1.1 Checking base directory...\n")
  if (dir.exists(TEST_CONFIG$base_dir)) {
    cat("  ✓ Base directory exists\n")
    tests_passed <- tests_passed + 1
  } else {
    cat("  ✗ Base directory not found\n")
    tests_failed <- tests_failed + 1
  }
  
  # Test 1.2: Check YAML file
  cat("1.2 Checking YAML file...\n")
  if (file.exists(TEST_CONFIG$yaml_file)) {
    cat("  ✓ YAML file exists\n")
    tests_passed <- tests_passed + 1
    
    # Try to read it
    tryCatch({
      yaml_data <- read_yaml(TEST_CONFIG$yaml_file)
      cat(sprintf("  ✓ YAML file readable (%d hospitals)\n", length(yaml_data)))
      tests_passed <- tests_passed + 1
    }, error = function(e) {
      cat(sprintf("  ✗ YAML file not readable: %s\n", e$message))
      tests_failed <- tests_failed + 1
    })
  } else {
    cat("  ✗ YAML file not found\n")
    tests_failed <- tests_failed + 2
  }
  
  # Test 1.3: Check strategic_plans directory
  cat("1.3 Checking strategic_plans directory...\n")
  if (dir.exists(TEST_CONFIG$strategic_plans_dir)) {
    cat("  ✓ Strategic plans directory exists\n")
    tests_passed <- tests_passed + 1
    
    folders <- list.dirs(TEST_CONFIG$strategic_plans_dir, recursive = FALSE, full.names = FALSE)
    cat(sprintf("  ✓ Found %d hospital folders\n", length(folders)))
    tests_passed <- tests_passed + 1
  } else {
    cat("  ✗ Strategic plans directory not found\n")
    tests_failed <- tests_failed + 2
  }
  
  # Test 1.4: Check test hospital folders
  cat("1.4 Checking test hospital folders...\n")
  for (fac in TEST_CONFIG$test_facs) {
    yaml_data <- read_yaml(TEST_CONFIG$yaml_file)
    
    # Find hospital in YAML
    hospital <- NULL
    for (h in yaml_data) {
      if (sprintf("%03d", as.numeric(h$FAC)) == sprintf("%03d", as.numeric(fac))) {
        hospital <- h
        break
      }
    }
    
    if (!is.null(hospital)) {
      local_folder <- hospital$strategy_search$local_folder
      folder_path <- file.path(TEST_CONFIG$strategic_plans_dir, local_folder)
      
      if (dir.exists(folder_path)) {
        cat(sprintf("  ✓ FAC %s folder exists: %s\n", fac, local_folder))
        tests_passed <- tests_passed + 1
      } else {
        cat(sprintf("  ✗ FAC %s folder not found: %s\n", fac, local_folder))
        tests_failed <- tests_failed + 1
      }
    }
  }
  
  # Summary
  cat("\n")
  cat(strrep("-", 80), "\n")
  cat(sprintf("File Structure Tests: %d passed, %d failed\n", tests_passed, tests_failed))
  cat(strrep("-", 80), "\n\n")
  
  return(list(passed = tests_passed, failed = tests_failed))
}

# ============================================================================
# TEST 2: HOSPITAL DISCOVERY VALIDATION
# ============================================================================

test_hospital_discovery <- function() {
  
  cat("\n")
  cat(strrep("=", 80), "\n")
  cat("TEST 2: HOSPITAL DISCOVERY VALIDATION\n")
  cat(strrep("=", 80), "\n\n")
  
  tests_passed <- 0
  tests_failed <- 0
  
  # Load discovery module
  source("hospital_discovery.R")
  
  # Test 2.1: Discovery function runs
  cat("2.1 Testing hospital discovery function...\n")
  tryCatch({
    hospitals <- get_hospital_list_from_yaml(
      yaml_path = TEST_CONFIG$yaml_file,
      strategic_plans_dir = TEST_CONFIG$strategic_plans_dir,
      verbose = FALSE
    )
    cat(sprintf("  ✓ Discovery completed (%d hospitals found)\n", nrow(hospitals)))
    tests_passed <- tests_passed + 1
  }, error = function(e) {
    cat(sprintf("  ✗ Discovery failed: %s\n", e$message))
    tests_failed <- tests_failed + 1
    return(list(passed = tests_passed, failed = tests_failed))
  })
  
  # Test 2.2: Check test hospitals are found
  cat("2.2 Checking test hospitals are discovered...\n")
  for (fac in TEST_CONFIG$test_facs) {
    fac_formatted <- sprintf("%03d", as.numeric(fac))
    found <- fac_formatted %in% hospitals$fac_code
    
    if (found) {
      cat(sprintf("  ✓ FAC %s found in discovery\n", fac))
      tests_passed <- tests_passed + 1
    } else {
      cat(sprintf("  ✗ FAC %s NOT found in discovery\n", fac))
      tests_failed <- tests_failed + 1
    }
  }
  
  # Test 2.3: Check PDF paths are valid
  cat("2.3 Validating PDF paths...\n")
  valid_pdfs <- hospitals %>%
    filter(fac_code %in% sprintf("%03d", as.numeric(TEST_CONFIG$test_facs))) %>%
    filter(pdf_valid)
  
  cat(sprintf("  Found %d valid PDFs out of %d test hospitals\n", 
              nrow(valid_pdfs), length(TEST_CONFIG$test_facs)))
  
  for (i in 1:nrow(valid_pdfs)) {
    h <- valid_pdfs[i, ]
    cat(sprintf("  ✓ FAC %s: %s (%d pages, %.1f KB)\n", 
                h$fac_code, h$pdf_filename, h$pdf_pages, h$pdf_size_kb))
    tests_passed <- tests_passed + 1
  }
  
  if (nrow(valid_pdfs) < length(TEST_CONFIG$test_facs)) {
    missing <- length(TEST_CONFIG$test_facs) - nrow(valid_pdfs)
    cat(sprintf("  ⚠ Warning: %d test hospitals don't have valid PDFs\n", missing))
  }
  
  # Test 2.4: Check skip reasons for invalid
  cat("2.4 Checking skip reasons...\n")
  skipped <- hospitals %>%
    filter(fac_code %in% sprintf("%03d", as.numeric(TEST_CONFIG$test_facs))) %>%
    filter(!pdf_valid)
  
  if (nrow(skipped) > 0) {
    for (i in 1:nrow(skipped)) {
      h <- skipped[i, ]
      cat(sprintf("  ⚠ FAC %s skipped: %s\n", h$fac_code, h$skip_reason))
    }
  } else {
    cat("  ✓ All test hospitals have valid PDFs\n")
    tests_passed <- tests_passed + 1
  }
  
  # Summary
  cat("\n")
  cat(strrep("-", 80), "\n")
  cat(sprintf("Hospital Discovery Tests: %d passed, %d failed\n", tests_passed, tests_failed))
  cat(strrep("-", 80), "\n\n")
  
  return(list(
    passed = tests_passed, 
    failed = tests_failed,
    hospitals = hospitals,
    valid_pdfs = valid_pdfs
  ))
}

# ============================================================================
# TEST 3: PDF PROCESSING VALIDATION
# ============================================================================

test_pdf_processing <- function(test_hospitals) {
  
  cat("\n")
  cat(strrep("=", 80), "\n")
  cat("TEST 3: PDF PROCESSING VALIDATION\n")
  cat(strrep("=", 80), "\n\n")
  
  tests_passed <- 0
  tests_failed <- 0
  
  # Load PDF processor
  source("pdf_image_processor.R")
  
  if (nrow(test_hospitals) == 0) {
    cat("  ⚠ No test hospitals with valid PDFs - skipping PDF tests\n")
    return(list(passed = 0, failed = 0))
  }
  
  # Test first hospital only (to save time)
  test_hospital <- test_hospitals[1, ]
  
  cat(sprintf("Testing PDF: %s\n", test_hospital$pdf_filename))
  cat(sprintf("Path: %s\n", test_hospital$pdf_path))
  cat(sprintf("Pages: %d\n", test_hospital$pdf_pages))
  cat(sprintf("Size: %.1f KB\n\n", test_hospital$pdf_size_kb))
  
  # Test 3.1: PDF to base64 conversion
  cat("3.1 Testing PDF to base64 conversion...\n")
  tryCatch({
    # Use lower DPI for testing (faster)
    base64_images <- pdf_to_base64_images(test_hospital$pdf_path, dpi = 100)
    
    cat(sprintf("  ✓ Converted %d pages to base64\n", length(base64_images)))
    tests_passed <- tests_passed + 1
    
    # Check first image
    if (length(base64_images) > 0) {
      img_size <- nchar(base64_images[[1]])
      cat(sprintf("  ✓ First image: %d characters\n", img_size))
      tests_passed <- tests_passed + 1
    }
    
  }, error = function(e) {
    cat(sprintf("  ✗ Conversion failed: %s\n", e$message))
    tests_failed <- tests_failed + 2
  })
  
  # Test 3.2: API content creation
  cat("3.2 Testing API content block creation...\n")
  tryCatch({
    test_prompt <- "Extract strategic priorities from this hospital strategic plan."
    
    content <- create_api_content_with_images(
      pdf_path = test_hospital$pdf_path,
      text_prompt = test_prompt,
      dpi = 100
    )
    
    cat(sprintf("  ✓ Created %d content blocks\n", length(content)))
    tests_passed <- tests_passed + 1
    
    # Count image blocks
    image_blocks <- sum(sapply(content, function(x) x$type == "image"))
    text_blocks <- sum(sapply(content, function(x) x$type == "text"))
    
    cat(sprintf("  ✓ Images: %d, Text: %d\n", image_blocks, text_blocks))
    tests_passed <- tests_passed + 1
    
  }, error = function(e) {
    cat(sprintf("  ✗ Content creation failed: %s\n", e$message))
    tests_failed <- tests_failed + 2
  })
  
  # Summary
  cat("\n")
  cat(strrep("-", 80), "\n")
  cat(sprintf("PDF Processing Tests: %d passed, %d failed\n", tests_passed, tests_failed))
  cat(strrep("-", 80), "\n\n")
  
  return(list(passed = tests_passed, failed = tests_failed))
}

# ============================================================================
# TEST 4: API INTEGRATION TEST (OPTIONAL - COSTS MONEY)
# ============================================================================

test_api_integration <- function(test_hospitals) {
  
  cat("\n")
  cat(strrep("=", 80), "\n")
  cat("TEST 4: API INTEGRATION TEST (COSTS ~$0.10)\n")
  cat(strrep("=", 80), "\n\n")
  
  if (!TEST_CONFIG$api_test_enabled) {
    cat("⚠ API testing disabled in TEST_CONFIG\n")
    cat("  Set TEST_CONFIG$api_test_enabled <- TRUE to enable\n\n")
    return(list(passed = 0, failed = 0, skipped = TRUE))
  }
  
  # Check API key
  api_key <- Sys.getenv("ANTHROPIC_API_KEY")
  if (is.null(api_key) || api_key == "") {
    cat("✗ API key not set - skipping API test\n\n")
    return(list(passed = 0, failed = 1, skipped = TRUE))
  }
  
  cat("⚠ WARNING: This test will make an actual API call and cost ~$0.10\n")
  
  if (interactive()) {
    response <- readline("Continue with API test? (yes/no): ")
    if (tolower(trimws(response)) != "yes") {
      cat("API test skipped\n\n")
      return(list(passed = 0, failed = 0, skipped = TRUE))
    }
  }
  
  tests_passed <- 0
  tests_failed <- 0
  
  # Load required modules
  source("api_functions_with_images.R")
  source("pdf_image_processor.R")
  
  # Test with first hospital
  if (nrow(test_hospitals) == 0) {
    cat("No test hospitals available\n\n")
    return(list(passed = 0, failed = 1))
  }
  
  test_hospital <- test_hospitals[1, ]
  
  cat(sprintf("Testing API extraction with: FAC %s\n", test_hospital$fac_code))
  cat(sprintf("PDF: %s (%d pages)\n\n", test_hospital$pdf_filename, test_hospital$pdf_pages))
  
  # Create simple test protocol
  test_protocol <- "Please extract the following from this strategic plan:
  
FAC: [Hospital FAC code]
HOSPITAL_NAME: [Full hospital name]
  
STRATEGIC_PRIORITIES:
1. [First priority]
2. [Second priority]

Keep response brief for testing purposes."
  
  # Test 4.1: Create content and call API
  cat("4.1 Calling Claude API...\n")
  
  tryCatch({
    # Create content
    content <- create_api_content_with_images(
      pdf_path = test_hospital$pdf_path,
      text_prompt = test_protocol,
      dpi = 100  # Lower DPI for faster/cheaper test
    )
    
    # Call API
    result <- call_claude_api_with_images(
      content = content,
      api_key = api_key,
      max_tokens = 2000,  # Lower token limit for test
      temperature = 0
    )
    
    if (result$success) {
      cat("  ✓ API call successful\n")
      cat(sprintf("  Input tokens: %d\n", result$input_tokens))
      cat(sprintf("  Output tokens: %d\n", result$output_tokens))
      
      # Calculate cost
      cost <- (result$input_tokens / 1000000 * 3.00) + 
              (result$output_tokens / 1000000 * 15.00)
      cat(sprintf("  Cost: $%.4f\n", cost))
      
      cat(sprintf("  Response length: %d characters\n", nchar(result$response_text)))
      
      # Check if response contains expected fields
      has_fac <- grepl("FAC:", result$response_text, ignore.case = TRUE)
      has_hospital <- grepl("HOSPITAL_NAME:", result$response_text, ignore.case = TRUE)
      has_priorities <- grepl("STRATEGIC_PRIORITIES:", result$response_text, ignore.case = TRUE)
      
      if (has_fac && has_hospital && has_priorities) {
        cat("  ✓ Response contains expected fields\n")
        tests_passed <- tests_passed + 2
      } else {
        cat("  ⚠ Response may be missing some expected fields\n")
        tests_passed <- tests_passed + 1
      }
      
      # Print first 200 characters of response
      cat("\n  Response preview:\n")
      cat(strrep("-", 78), "\n")
      cat(substr(result$response_text, 1, 200), "...\n")
      cat(strrep("-", 78), "\n")
      
    } else {
      cat(sprintf("  ✗ API call failed: %s\n", result$error))
      tests_failed <- tests_failed + 2
    }
    
  }, error = function(e) {
    cat(sprintf("  ✗ Test failed: %s\n", e$message))
    tests_failed <- tests_failed + 2
  })
  
  # Summary
  cat("\n")
  cat(strrep("-", 80), "\n")
  cat(sprintf("API Integration Tests: %d passed, %d failed\n", tests_passed, tests_failed))
  cat(strrep("-", 80), "\n\n")
  
  return(list(passed = tests_passed, failed = tests_failed))
}

# ============================================================================
# TEST 5: YAML UPDATE VALIDATION
# ============================================================================

test_yaml_updates <- function() {
  
  cat("\n")
  cat(strrep("=", 80), "\n")
  cat("TEST 5: YAML UPDATE VALIDATION\n")
  cat(strrep("=", 80), "\n\n")
  
  tests_passed <- 0
  tests_failed <- 0
  
  # Load yaml updater
  source("yaml_updater.R")
  
  # Test 5.1: Read current YAML
  cat("5.1 Reading YAML file...\n")
  tryCatch({
    status <- get_extraction_status_summary(TEST_CONFIG$yaml_file)
    cat("  ✓ YAML read successfully\n")
    tests_passed <- tests_passed + 1
  }, error = function(e) {
    cat(sprintf("  ✗ YAML read failed: %s\n", e$message))
    tests_failed <- tests_failed + 1
  })
  
  # Test 5.2: Check if test hospitals have extraction flags
  cat("5.2 Checking extraction flags for test hospitals...\n")
  
  yaml_data <- read_yaml(TEST_CONFIG$yaml_file)
  
  for (fac in TEST_CONFIG$test_facs) {
    fac_formatted <- sprintf("%03d", as.numeric(fac))
    
    # Find hospital
    hospital <- NULL
    for (h in yaml_data) {
      if (sprintf("%03d", as.numeric(h$FAC)) == fac_formatted) {
        hospital <- h
        break
      }
    }
    
    if (!is.null(hospital) && !is.null(hospital$strategy_search)) {
      baseline_extracted <- hospital$strategy_search$baseline_extracted
      needs_extraction <- hospital$strategy_search$needs_api_extraction
      
      cat(sprintf("  FAC %s: baseline_extracted=%s, needs_extraction=%s\n",
                  fac, 
                  ifelse(is.null(baseline_extracted), "NULL", baseline_extracted),
                  ifelse(is.null(needs_extraction), "NULL", needs_extraction)))
      tests_passed <- tests_passed + 1
    } else {
      cat(sprintf("  ⚠ FAC %s: No extraction flags found\n", fac))
    }
  }
  
  # Summary
  cat("\n")
  cat(strrep("-", 80), "\n")
  cat(sprintf("YAML Update Tests: %d passed, %d failed\n", tests_passed, tests_failed))
  cat(strrep("-", 80), "\n\n")
  
  return(list(passed = tests_passed, failed = tests_failed))
}

# ============================================================================
# MASTER TEST RUNNER
# ============================================================================

run_all_tests <- function(include_api_test = FALSE) {
  
  cat("\n")
  cat(strrep("=", 90), "\n")
  cat("COMPREHENSIVE TESTING SUITE FOR REFACTORED API EXTRACTION SYSTEM\n")
  cat(strrep("=", 90), "\n")
  cat(sprintf("Test Date: %s\n", Sys.time()))
  cat(sprintf("Test Hospitals: %s\n", paste(TEST_CONFIG$test_facs, collapse = ", ")))
  cat(strrep("=", 90), "\n")
  
  # Store all results
  all_results <- list()
  
  # Run tests
  all_results$file_structure <- test_file_structure()
  
  discovery_result <- test_hospital_discovery()
  all_results$hospital_discovery <- discovery_result
  
  if (!is.null(discovery_result$valid_pdfs) && nrow(discovery_result$valid_pdfs) > 0) {
    all_results$pdf_processing <- test_pdf_processing(discovery_result$valid_pdfs)
    
    if (include_api_test) {
      all_results$api_integration <- test_api_integration(discovery_result$valid_pdfs)
    } else {
      cat("\n⚠ API integration test skipped (set include_api_test = TRUE to run)\n")
    }
  }
  
  all_results$yaml_updates <- test_yaml_updates()
  
  # Overall summary
  cat("\n")
  cat(strrep("=", 90), "\n")
  cat("OVERALL TEST SUMMARY\n")
  cat(strrep("=", 90), "\n")
  
  total_passed <- sum(sapply(all_results, function(x) x$passed))
  total_failed <- sum(sapply(all_results, function(x) x$failed))
  
  cat(sprintf("Total tests passed: %d\n", total_passed))
  cat(sprintf("Total tests failed: %d\n", total_failed))
  
  if (total_failed == 0) {
    cat("\n✓✓✓ ALL TESTS PASSED ✓✓✓\n")
    cat("System is ready for extraction!\n")
  } else {
    cat(sprintf("\n⚠ %d test(s) failed - review output above\n", total_failed))
    cat("Fix issues before running full extraction.\n")
  }
  
  cat(strrep("=", 90), "\n\n")
  
  return(invisible(all_results))
}

# ============================================================================
# INSTRUCTIONS
# ============================================================================

if (interactive()) {
  cat("\n")
  cat(strrep("=", 80), "\n")
  cat("TESTING FRAMEWORK - INSTRUCTIONS\n")
  cat(strrep("=", 80), "\n")
  cat("\nQUICK START:\n\n")
  cat("1. CONFIGURE TEST HOSPITALS:\n")
  cat("   TEST_CONFIG$test_facs <- c('592', '593', '597')\n\n")
  cat("2. RUN ALL TESTS (WITHOUT API):\n")
  cat("   results <- run_all_tests(include_api_test = FALSE)\n\n")
  cat("3. RUN ALL TESTS (WITH API - COSTS ~$0.10):\n")
  cat("   results <- run_all_tests(include_api_test = TRUE)\n\n")
  cat("4. RUN INDIVIDUAL TESTS:\n")
  cat("   test_file_structure()\n")
  cat("   test_hospital_discovery()\n")
  cat("   test_pdf_processing(hospitals)\n")
  cat("   test_yaml_updates()\n\n")
  cat(strrep("=", 80), "\n\n")
}
