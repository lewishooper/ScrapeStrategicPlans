# ============================================================================
# CONFIGURATION FILE FOR HOSPITAL EXTRACTION WORKFLOW
# ============================================================================
# Updated for new folder-based structure with YAML metadata
# Author: Skip & Claude
# Version: 2.0
# Last Updated: 2026-01-24
# ============================================================================

# ----------------------------------------------------------------------------
# API SETTINGS
# ----------------------------------------------------------------------------

# Your Claude API key from console.anthropic.com
# SECURITY: Do NOT share this file or commit it to version control
API_KEY <- Sys.getenv("ANTHROPIC_API_KEY")

# If not set in environment, uncomment and add here:
# API_KEY <- "sk-ant-api03-YOUR_API_KEY_HERE"

# API endpoint and model settings
API_ENDPOINT <- "https://api.anthropic.com/v1/messages"
API_VERSION <- "2023-06-01"
MODEL_NAME <- "claude-sonnet-4-20250514"  # Sonnet 4.5
MAX_TOKENS <- 16000  # Maximum tokens for output

# ----------------------------------------------------------------------------
# FILE PATHS - UPDATED FOR NEW STRUCTURE
# ----------------------------------------------------------------------------

# Base directory
BASE_DIR <- "E:/Hospital_Strategic_Plans"

# Hospital metadata (YAML file)
YAML_FILE <- file.path(BASE_DIR, "code", "Hospital_strategy.yaml")

# Directory containing hospital folders with PDFs
STRATEGIC_PLANS_DIR <- file.path(BASE_DIR, "strategic_plans")

# Directory where extraction output files will be saved
OUTPUT_DIRECTORY <- file.path(BASE_DIR, "processed")

# Location of the protocol file
PROTOCOL_FILE <- file.path(BASE_DIR, "Phase3ExtractionProtocolV4_1.rmd")

# Progress tracking directory
PROGRESS_DIR <- file.path(BASE_DIR, "outputs", "progress")

# Progress tracking file (will be created automatically)
PROGRESS_FILE <- file.path(PROGRESS_DIR, "extraction_progress.csv")

# Log directory for skipped hospitals, errors, etc.
LOG_DIR <- file.path(BASE_DIR, "outputs", "logs")

# ----------------------------------------------------------------------------
# BATCH PROCESSING SETTINGS
# ----------------------------------------------------------------------------

# How many hospitals per batch file (for organization/review)
# Note: Hospitals are processed serially regardless, this is just for output grouping
HOSPITALS_PER_BATCH <- 5

# Starting batch number (for continuing from where you left off)
# Set this to the next batch number if resuming
STARTING_BATCH_NUMBER <- 1  # Will auto-increment from progress if available

# Delay between API calls (seconds) to avoid rate limiting
# Recommended: 60-90 seconds for stability
API_DELAY_SECONDS <- 90

# Maximum retries for failed API calls
MAX_RETRIES <- 3

# ----------------------------------------------------------------------------
# PROCESSING MODE
# ----------------------------------------------------------------------------
# EXTRACTION MODE
# - "baseline": Extract all hospitals that haven't been baseline-extracted
# - "changes_only": Extract only hospitals with comparison_result = "changed"
# - "force_all": Force extraction of all hospitals regardless of status
EXTRACTION_MODE <- "force_all"  # Use force_all for February 2026 baseline run
# Skip hospitals with these FAC codes (e.g., too large, manual processing needed)
# Example: SKIP_FACS <- c("969", "980")
SKIP_FACS <- c("632", "969", "980")

# ----------------------------------------------------------------------------
# OUTPUT SETTINGS
# ----------------------------------------------------------------------------

# Create individual hospital files in addition to batch files
CREATE_INDIVIDUAL_FILES <- TRUE

# Individual file naming pattern
# Available placeholders: {FAC}, {NAME}, {DATE}, {BATCH}
INDIVIDUAL_FILE_PATTERN <- "Phase3_L1_{FAC}_{DATE}.txt"

# Batch file naming pattern
BATCH_FILE_PATTERN <- "Phase3_Batch{BATCH}_L1_Extraction.txt"

# ----------------------------------------------------------------------------
# COST TRACKING
# ----------------------------------------------------------------------------

# Pricing per million tokens (as of January 2025)
PRICE_INPUT_PER_MILLION <- 3.00   # Sonnet 4.5 input cost
PRICE_OUTPUT_PER_MILLION <- 15.00 # Sonnet 4.5 output cost

# Budget alert threshold (USD)
BUDGET_ALERT_THRESHOLD <- 20.00

# ----------------------------------------------------------------------------
# PDF PROCESSING SETTINGS
# ----------------------------------------------------------------------------

# DPI for PDF to image conversion
# Higher = better quality but more tokens/cost
# - 150 dpi: Good balance (recommended)
# - 200 dpi: Higher quality, ~30% more cost
# - 100 dpi: Lower quality, ~30% less cost
PDF_DPI <- 150

# Maximum PDF size (KB)
MAX_PDF_SIZE_KB <- 100000  # 100 MB

# Minimum PDF size (KB)
MIN_PDF_SIZE_KB <- 10

# ----------------------------------------------------------------------------
# EXTRACTION SETTINGS
# ----------------------------------------------------------------------------

# Current date for EXTRACTION_DATE field (YYYY-MM format)
EXTRACTION_DATE <- format(Sys.Date(), "%Y-%m")

# System date for PLAN_CLASSIFICATION logic
CURRENT_YEAR <- as.integer(format(Sys.Date(), "%Y"))

# ----------------------------------------------------------------------------
# VALIDATION SETTINGS
# ----------------------------------------------------------------------------

# Validate configuration on load
validate_config <- function() {
  errors <- c()
  warnings <- c()
  
  # Check API key is set
  if (is.null(API_KEY) || API_KEY == "" || API_KEY == "sk-ant-api03-YOUR_API_KEY_HERE") {
    errors <- c(errors, "API_KEY not set - set in environment or uncomment in config.R")
  }
  
  # Check base directory exists
  if (!dir.exists(BASE_DIR)) {
    errors <- c(errors, paste("BASE_DIR does not exist:", BASE_DIR))
  }
  
  # Check YAML file exists
  if (!file.exists(YAML_FILE)) {
    errors <- c(errors, paste("YAML_FILE not found:", YAML_FILE))
  }
  
  # Check strategic plans directory exists
  if (!dir.exists(STRATEGIC_PLANS_DIR)) {
    errors <- c(errors, paste("STRATEGIC_PLANS_DIR does not exist:", STRATEGIC_PLANS_DIR))
  }
  
  # Check protocol file exists
  if (!file.exists(PROTOCOL_FILE)) {
    warnings <- c(warnings, paste("PROTOCOL_FILE not found:", PROTOCOL_FILE))
    warnings <- c(warnings, "  Will need protocol file before extraction can begin")
  }
  
  # Create output directories if they don't exist
  dirs_to_create <- c(OUTPUT_DIRECTORY, PROGRESS_DIR, LOG_DIR)
  
  for (dir in dirs_to_create) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
      message("Created directory: ", dir)
    }
  }
  
  # Report errors
  if (length(errors) > 0) {
    stop("Configuration errors:\n", paste("  ✗", errors, collapse = "\n"))
  }
  
  # Report warnings
  if (length(warnings) > 0) {
    warning("Configuration warnings:\n", paste("  ⚠", warnings, collapse = "\n"))
  }
  
  message("✓ Configuration validated successfully!")
  return(TRUE)
}

# ----------------------------------------------------------------------------
# HELPER FUNCTIONS
# ----------------------------------------------------------------------------

#' Get current extraction mode description
get_mode_description <- function() {
  switch(EXTRACTION_MODE,
         "baseline" = "Baseline Mode - Extract all hospitals without baseline extraction",
         "changes_only" = "Changes Only - Extract only hospitals flagged for extraction",
         "force_all" = "Force All - Extract all hospitals regardless of status",
         "Unknown mode")
}

# ----------------------------------------------------------------------------
# LOAD CONFIGURATION
# ----------------------------------------------------------------------------

# Print configuration summary
print_config <- function() {
  cat("\n")
  cat(strrep("=", 80), "\n")
  cat("HOSPITAL EXTRACTION WORKFLOW CONFIGURATION\n")
  cat(strrep("=", 80), "\n")
  cat("Model:                ", MODEL_NAME, "\n")
  cat("Extraction Mode:      ", get_mode_description(), "\n")
  cat("\nPaths:\n")
  cat("  Base Directory:     ", BASE_DIR, "\n")
  cat("  YAML File:          ", YAML_FILE, "\n")
  cat("  Strategic Plans:    ", STRATEGIC_PLANS_DIR, "\n")
  cat("  Output Directory:   ", OUTPUT_DIRECTORY, "\n")
  cat("  Protocol File:      ", PROTOCOL_FILE, "\n")
  cat("\nProcessing:\n")
  cat("  Hospitals per batch:", HOSPITALS_PER_BATCH, "\n")
  cat("  Starting batch:     ", STARTING_BATCH_NUMBER, "\n")
  cat("  API delay:          ", API_DELAY_SECONDS, "seconds\n")
  cat("  PDF DPI:            ", PDF_DPI, "\n")
  cat("\nCost Settings:\n")
  cat("  Budget alert:       ", sprintf("$%.2f", BUDGET_ALERT_THRESHOLD), "\n")
  cat("  Input pricing:      ", sprintf("$%.2f/M tokens", PRICE_INPUT_PER_MILLION), "\n")
  cat("  Output pricing:     ", sprintf("$%.2f/M tokens", PRICE_OUTPUT_PER_MILLION), "\n")
  cat(strrep("=", 80), "\n")
  cat("\n")
}

# Auto-run validation when sourced (can disable for testing)
if (interactive()) {
  tryCatch({
    validate_config()
    print_config()
  }, error = function(e) {
    warning("Configuration validation failed: ", e$message)
    cat("\nPlease fix configuration errors before running extraction.\n")
  })
}

# Export key paths as global variables for backward compatibility
PDF_DIR <- STRATEGIC_PLANS_DIR  # Legacy name
