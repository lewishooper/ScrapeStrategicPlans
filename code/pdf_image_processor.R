# ========================================================================
# PDF to Image Processing Functions for Claude API
# ========================================================================
# Purpose: Convert PDF pages to base64-encoded PNG images for API submission
# This replicates the chat window's ability to "see" graphics and visual content
# ========================================================================

# Required Libraries
# install.packages(c("pdftools", "magick", "base64enc", "jsonlite"))
library(pdftools)
library(magick)
library(base64enc)
library(jsonlite)

# ========================================================================
# FUNCTION: pdf_to_base64_images
# ========================================================================
# Converts all pages of a PDF to base64-encoded PNG images
# 
# Parameters:
#   pdf_path: Full path to PDF file
#   dpi: Resolution for conversion (default 150 is good balance)
#        - 150 dpi: Good quality, reasonable file size
#        - 200 dpi: Higher quality, larger files, more tokens
#        - 100 dpi: Lower quality, smaller files, fewer tokens
# 
# Returns: List of base64-encoded strings, one per page
# ========================================================================

pdf_to_base64_images <- function(pdf_path, dpi = 150) {
  
  # Validate file exists
  if (!file.exists(pdf_path)) {
    stop(sprintf("PDF file not found: %s", pdf_path))
  }
  
  cat(sprintf("\nConverting PDF to images: %s\n", basename(pdf_path)))
  cat(sprintf("Resolution: %d dpi\n", dpi))
  
  # Get page count
  pdf_info <- pdftools::pdf_info(pdf_path)
  num_pages <- pdf_info$pages
  cat(sprintf("Total pages: %d\n", num_pages))
  
  # Convert PDF pages to temporary PNG files
  tryCatch({
    
    # Create temporary directory for images
    temp_dir <- tempdir()
    
    # Convert PDF to PNG images (one per page)
    cat("Converting pages to PNG...\n")
    image_files <- pdftools::pdf_convert(
      pdf_path,
      format = "png",
      dpi = dpi,
      pages = NULL,  # All pages
      filenames = file.path(temp_dir, sprintf("page_%03d.png", 1:num_pages)),
      verbose = FALSE
    )
    
    cat(sprintf("Created %d image files\n", length(image_files)))
    
    # Convert each image to base64
    cat("Encoding images to base64...\n")
    base64_images <- list()
    
    for (i in seq_along(image_files)) {
      # Read image and convert to base64
      base64_images[[i]] <- base64enc::base64encode(image_files[i])
      
      # Get file size for reporting
      file_size_kb <- file.info(image_files[i])$size / 1024
      cat(sprintf("  Page %d: %.1f KB\n", i, file_size_kb))
    }
    
    # Clean up temporary image files
    file.remove(image_files)
    
    cat(sprintf("Successfully encoded %d pages\n", length(base64_images)))
    
    return(base64_images)
    
  }, error = function(e) {
    stop(sprintf("Error converting PDF to images: %s", e$message))
  })
}


# ========================================================================
# FUNCTION: create_api_content_with_images
# ========================================================================
# Creates the content array for Claude API with PDF images + text prompt
# 
# Parameters:
#   pdf_path: Full path to PDF file
#   text_prompt: The extraction prompt (protocol + instructions)
#   dpi: Resolution for PDF conversion (default 150)
# 
# Returns: List formatted for Claude API content parameter
# ========================================================================

create_api_content_with_images <- function(pdf_path, text_prompt, dpi = 150) {
  
  # Convert PDF to base64 images
  base64_images <- pdf_to_base64_images(pdf_path, dpi)
  
  # Build content array
  content <- list()
  
  cat("\nBuilding API content structure...\n")
  
  # Add all PDF pages as image content blocks
  for (i in seq_along(base64_images)) {
    content[[length(content) + 1]] <- list(
      type = "image",
      source = list(
        type = "base64",
        media_type = "image/png",
        data = base64_images[[i]]
      )
    )
  }
  
  cat(sprintf("Added %d image content blocks\n", length(base64_images)))
  
  # Add the text prompt as final content block
  content[[length(content) + 1]] <- list(
    type = "text",
    text = text_prompt
  )
  
  cat("Added text prompt content block\n")
  cat(sprintf("Total content blocks: %d\n", length(content)))
  
  return(content)
}


# ========================================================================
# FUNCTION: estimate_image_tokens
# ========================================================================
# Estimates token count for image content (approximate)
# Claude uses ~2000 tokens per image for analysis
# 
# Parameters:
#   num_pages: Number of PDF pages/images
# 
# Returns: Estimated token count
# ========================================================================

estimate_image_tokens <- function(num_pages) {
  # Rough estimate: ~2000 tokens per image for Claude analysis
  # This is approximate - actual usage varies by image complexity
  tokens_per_image <- 2000
  estimated_tokens <- num_pages * tokens_per_image
  
  return(estimated_tokens)
}


# ========================================================================
# FUNCTION: get_pdf_info_summary
# ========================================================================
# Gets summary information about a PDF for logging/reporting
# 
# Parameters:
#   pdf_path: Full path to PDF file
# 
# Returns: List with PDF metadata
# ========================================================================

get_pdf_info_summary <- function(pdf_path) {
  
  info <- pdftools::pdf_info(pdf_path)
  
  summary <- list(
    filename = basename(pdf_path),
    pages = info$pages,
    file_size_mb = file.info(pdf_path)$size / (1024 * 1024),
    estimated_tokens = estimate_image_tokens(info$pages),
    pdf_version = info$version,
    created = info$created,
    modified = info$modified
  )
  
  return(summary)
}


# ========================================================================
# TESTING FUNCTION
# ========================================================================
# Test the PDF to image conversion on a single file
# ========================================================================

test_pdf_conversion <- function(pdf_path, dpi = 150) {
  
  cat("\n========================================\n")
  cat("TESTING PDF TO IMAGE CONVERSION\n")
  cat("========================================\n")
  
  # Get PDF info
  info <- get_pdf_info_summary(pdf_path)
  
  cat("\nPDF Information:\n")
  cat(sprintf("  Filename: %s\n", info$filename))
  cat(sprintf("  Pages: %d\n", info$pages))
  cat(sprintf("  File Size: %.2f MB\n", info$file_size_mb))
  cat(sprintf("  Estimated Tokens: ~%d\n", info$estimated_tokens))
  
  # Test conversion
  cat("\nStarting conversion test...\n")
  
  start_time <- Sys.time()
  
  base64_images <- pdf_to_base64_images(pdf_path, dpi)
  
  end_time <- Sys.time()
  elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  cat("\n========================================\n")
  cat("CONVERSION TEST RESULTS\n")
  cat("========================================\n")
  cat(sprintf("Pages converted: %d\n", length(base64_images)))
  cat(sprintf("Time elapsed: %.2f seconds\n", elapsed))
  cat(sprintf("Avg time per page: %.2f seconds\n", elapsed / length(base64_images)))
  
  # Get base64 string lengths (for token estimation)
  string_lengths <- sapply(base64_images, nchar)
  cat(sprintf("\nBase64 string lengths:\n"))
  cat(sprintf("  Min: %d characters\n", min(string_lengths)))
  cat(sprintf("  Max: %d characters\n", max(string_lengths)))
  cat(sprintf("  Mean: %.0f characters\n", mean(string_lengths)))
  cat(sprintf("  Total: %d characters\n", sum(string_lengths)))
  
  cat("\n✓ Conversion test successful!\n")
  cat("========================================\n\n")
  
  return(invisible(list(
    success = TRUE,
    num_pages = length(base64_images),
    elapsed_seconds = elapsed,
    base64_images = base64_images
  )))
}


# ========================================================================
# EXAMPLE USAGE
# ========================================================================
# 
# # Test conversion on a single PDF
# test_pdf_conversion("E:/ExtractStrategies/Extract_Strategies/PDFs/941_Humber_River_Hospital.pdf")
# 
# # Convert PDF and create API content structure
# content <- create_api_content_with_images(
#   pdf_path = "E:/ExtractStrategies/Extract_Strategies/PDFs/941_Humber_River_Hospital.pdf",
#   text_prompt = "Your extraction protocol here...",
#   dpi = 150
# )
# 
# # Use in API call
# response <- call_claude_api(
#   messages = list(
#     list(
#       role = "user",
#       content = content  # This is the content with images
#     )
#   ),
#   max_tokens = 16000,
#   temperature = 0
# )
# 
# ========================================================================
