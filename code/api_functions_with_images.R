# ========================================================================
# Modified Claude API Functions for Image-Based PDF Processing
# ========================================================================
# Purpose: Updated API call functions to handle image content blocks
# Replaces or supplements your existing API functions
# ========================================================================

library(httr)
library(jsonlite)

# ========================================================================
# FUNCTION: call_claude_api_with_images
# ========================================================================
# Makes API call to Claude with image content (PDF pages as images)
# Includes retry logic with exponential backoff
# 
# Parameters:
#   content: List of content blocks (images + text) from create_api_content_with_images()
#   api_key: Your Anthropic API key
#   model: Model to use (default: claude-sonnet-4-5-20241022)
#   max_tokens: Maximum tokens for response (default: 16000)
#   temperature: Temperature setting (default: 0 for deterministic)
#   max_retries: Number of retry attempts on failure (default: 3)
#   system_prompt: Optional system prompt (default: NULL)
# 
# Returns: List with response content and metadata
# ========================================================================

call_claude_api_with_images <- function(
    content,
    api_key,
    model = "claude-sonnet-4-20250514",  # Updated to correct Sonnet 4.5 model
    max_tokens = 16000,
    temperature = 0,
    max_retries = 3,
    system_prompt = NULL
) {
  
  # Validate API key
  if (is.null(api_key) || api_key == "") {
    stop("API key is required")
  }
  
  # Build request body
  request_body <- list(
    model = model,
    max_tokens = max_tokens,
    temperature = temperature,
    messages = list(
      list(
        role = "user",
        content = content  # This contains images + text
      )
    )
  )
  
  # Add system prompt if provided
  if (!is.null(system_prompt)) {
    request_body$system <- system_prompt
  }
  
  cat("\n========================================\n")
  cat("CALLING CLAUDE API\n")
  cat("========================================\n")
  cat(sprintf("Model: %s\n", model))
  cat(sprintf("Max tokens: %d\n", max_tokens))
  cat(sprintf("Temperature: %.1f\n", temperature))
  cat(sprintf("Content blocks: %d\n", length(content)))
  
  # Count images in content
  num_images <- sum(sapply(content, function(x) x$type == "image"))
  cat(sprintf("Image blocks: %d\n", num_images))
  
  # Retry logic with exponential backoff
  attempt <- 1
  last_error <- NULL
  
  while (attempt <= max_retries) {
    
    if (attempt > 1) {
      wait_time <- 2^(attempt - 1)  # Exponential backoff: 2, 4, 8 seconds
      cat(sprintf("\nRetry attempt %d of %d (waiting %d seconds)...\n", 
                  attempt, max_retries, wait_time))
      Sys.sleep(wait_time)
    } else {
      cat("\nSending request to API...\n")
    }
    
    tryCatch({
      
      # Make API call
      response <- httr::POST(
        url = "https://api.anthropic.com/v1/messages",
        httr::add_headers(
          "x-api-key" = api_key,
          "anthropic-version" = "2023-06-01",
          "content-type" = "application/json"
        ),
        body = jsonlite::toJSON(request_body, auto_unbox = TRUE),
        encode = "raw"
      )
      
      # Check HTTP status
      if (httr::status_code(response) != 200) {
        error_content <- httr::content(response, "text", encoding = "UTF-8")
        stop(sprintf("API error (HTTP %d): %s", 
                     httr::status_code(response), error_content))
      }
      
      # Parse response
      response_content <- httr::content(response, "text", encoding = "UTF-8")
      response_json <- jsonlite::fromJSON(response_content, simplifyVector = FALSE)
      
      # Extract text from response
      if (!is.null(response_json$content) && length(response_json$content) > 0) {
        response_text <- paste(
          sapply(response_json$content, function(x) {
            if (!is.null(x$text)) x$text else ""
          }),
          collapse = "\n"
        )
      } else {
        response_text <- ""
      }
      
      # Extract usage statistics
      usage <- response_json$usage
      
      cat("\n========================================\n")
      cat("API CALL SUCCESSFUL\n")
      cat("========================================\n")
      cat(sprintf("Input tokens: %d\n", usage$input_tokens))
      cat(sprintf("Output tokens: %d\n", usage$output_tokens))
      cat(sprintf("Total tokens: %d\n", usage$input_tokens + usage$output_tokens))
      cat(sprintf("Response length: %d characters\n", nchar(response_text)))
      cat("========================================\n\n")
      
      # Return successful result
      return(list(
        success = TRUE,
        response_text = response_text,
        input_tokens = usage$input_tokens,
        output_tokens = usage$output_tokens,
        total_tokens = usage$input_tokens + usage$output_tokens,
        model = response_json$model,
        stop_reason = response_json$stop_reason,
        attempt = attempt,
        full_response = response_json
      ))
      
    }, error = function(e) {
      last_error <- e$message
      cat(sprintf("\n✗ Attempt %d failed: %s\n", attempt, e$message))
      attempt <<- attempt + 1
      return(NULL)
    })
    
    # If we got here without returning, increment attempt
    attempt <- attempt + 1
  }
  
  # All retries failed
  cat("\n========================================\n")
  cat("API CALL FAILED\n")
  cat("========================================\n")
  cat(sprintf("All %d attempts failed\n", max_retries))
  cat(sprintf("Last error: %s\n", last_error))
  cat("========================================\n\n")
  
  return(list(
    success = FALSE,
    error = last_error,
    attempts = max_retries
  ))
}


# ========================================================================
# FUNCTION: extract_hospital_with_images
# ========================================================================
# Complete workflow: PDF → Images → API → Extraction
# Combines PDF conversion and API call into single function
# 
# Parameters:
#   pdf_path: Full path to PDF file
#   protocol_text: The extraction protocol text
#   api_key: Your Anthropic API key
#   batch_num: Batch number for prompt
#   hospital_num: Hospital number within batch
#   dpi: Resolution for PDF conversion (default: 150)
#   max_tokens: Maximum tokens for response
# 
# Returns: List with extraction results and metadata
# ========================================================================

extract_hospital_with_images <- function(
    pdf_path,
    protocol_text,
    api_key,
    batch_num,
    hospital_num,
    fac_code = NULL,           # NEW PARAMETER
    hospital_name = NULL,      # NEW PARAMETER
    dpi = 150,
    max_tokens = 16000
) {
  
  cat("\n########################################\n")
  cat(sprintf("EXTRACTING HOSPITAL %d - BATCH %d\n", hospital_num, batch_num))
  cat("########################################\n")
  cat(sprintf("PDF: %s\n", basename(pdf_path)))
  
  # Validate inputs
  if (!file.exists(pdf_path)) {
    return(list(
      success = FALSE,
      error = sprintf("PDF file not found: %s", pdf_path)
    ))
  }
  
  # Check PDF size and adjust DPI if needed
  pdf_info <- pdftools::pdf_info(pdf_path)
  file_size_mb <- file.info(pdf_path)$size / (1024 * 1024)
  
  # Automatic DPI adjustment for large PDFs
  adjusted_dpi <- dpi
  if (file_size_mb > 20 || pdf_info$pages > 20) {
    adjusted_dpi <- 100  # Reduce to 100 DPI for large files
    cat(sprintf("⚠ Large PDF detected (%.1f MB, %d pages)\n", file_size_mb, pdf_info$pages))
    cat(sprintf("⚠ Automatically reducing DPI from %d to %d\n", dpi, adjusted_dpi))
  } else if (file_size_mb > 10 || pdf_info$pages > 10) {
    adjusted_dpi <- min(dpi, 125)  # Cap at 125 DPI for medium files
    if (adjusted_dpi < dpi) {
      cat(sprintf("⚠ Medium-sized PDF (%.1f MB, %d pages)\n", file_size_mb, pdf_info$pages))
      cat(sprintf("⚠ Reducing DPI from %d to %d\n", dpi, adjusted_dpi))
    }
  }
  
  cat(sprintf("Using DPI: %d\n", adjusted_dpi))
  
  # Build complete prompt
  # Build complete prompt
  prompt_text <- sprintf(
    "%s\n\n========================================\n\n",
    protocol_text
  )
  
  # INJECT FAC AND HOSPITAL NAME (NEW CODE)
  # INJECT FAC CODE (hospital name extracted from document)
  if (!is.null(fac_code)) {
    prompt_text <- paste0(
      prompt_text,
      "========================================\n",
      "CRITICAL HEADER INFORMATION\n",
      "========================================\n\n",
      sprintf("You MUST use this exact value for the FAC field:\n\n"),
      sprintf("FAC: %s\n\n", fac_code),
      sprintf("Do NOT attempt to extract the FAC number from the PDF document.\n"),
      sprintf("Use the value provided above exactly as written.\n\n"),
      sprintf("You SHOULD extract HOSPITAL_NAME_FILE and HOSPITAL_NAME_TEXT from the document itself.\n\n"),
      "========================================\n\n"
    )
  }
  
  # CONTINUE WITH ORIGINAL PROMPT
  prompt_text <- paste0(
    prompt_text,
    sprintf("Please extract Level 1 strategic priorities from this hospital strategic plan PDF.\n")
  )
  prompt_text <- paste0(
    prompt_text,
    sprintf("This is Hospital %d in Batch %d.\n\n", hospital_num, batch_num)
  )
  prompt_text <- paste0(
    prompt_text,
    "Follow the protocol exactly as specified above."
  )
  
  # Step 1: Convert PDF to images and create content structure
  cat("\n--- Step 1: Converting PDF to images ---\n")
  
  content <- tryCatch({
    create_api_content_with_images(pdf_path, prompt_text, adjusted_dpi)
  }, error = function(e) {
    return(list(error = e$message))
  })
  
  if (!is.null(content$error)) {
    return(list(
      success = FALSE,
      error = sprintf("PDF conversion failed: %s", content$error)
    ))
  }
  
  
  # Step 2: Call API with image content
  cat("\n--- Step 2: Calling Claude API ---\n")
  
  api_result <- call_claude_api_with_images(
    content = content,
    api_key = api_key,
    max_tokens = max_tokens,
    temperature = 0
  )
  
  if (!api_result$success) {
    return(list(
      success = FALSE,
      error = sprintf("API call failed: %s", api_result$error)
    ))
  }
  
  # Success
  cat("\n✓ Extraction completed successfully\n")
  
  return(list(
    success = TRUE,
    extraction_text = api_result$response_text,
    input_tokens = api_result$input_tokens,
    output_tokens = api_result$output_tokens,
    total_tokens = api_result$total_tokens,
    model = api_result$model,
    pdf_filename = basename(pdf_path),
    batch_num = batch_num,
    hospital_num = hospital_num
  ))
}


# ========================================================================
# FUNCTION: calculate_cost
# ========================================================================
# Calculates cost based on token usage
# Current rates (as of Dec 2024):
#   Sonnet 4: $3.00 per MTok input, $15.00 per MTok output
# 
# Parameters:
#   input_tokens: Number of input tokens
#   output_tokens: Number of output tokens
#   model: Model name (for rate lookup)
# 
# Returns: Cost in USD
# ========================================================================

calculate_cost <- function(input_tokens, output_tokens, model = "claude-sonnet-4-5-20241022") {
  
  # Rate per million tokens (MTok)
  # Update these if rates change
  if (grepl("sonnet-4|sonnet.*4", model, ignore.case = TRUE)) {
    input_rate <- 3.00   # $3.00 per MTok
    output_rate <- 15.00  # $15.00 per MTok
  } else if (grepl("opus-4|opus.*4", model, ignore.case = TRUE)) {
    input_rate <- 15.00
    output_rate <- 75.00
  } else if (grepl("haiku-4|haiku.*4", model, ignore.case = TRUE)) {
    input_rate <- 0.80
    output_rate <- 4.00
  } else {
    # Default to Sonnet rates
    input_rate <- 3.00
    output_rate <- 15.00
  }
  
  # Calculate cost
  input_cost <- (input_tokens / 1000000) * input_rate
  output_cost <- (output_tokens / 1000000) * output_rate
  total_cost <- input_cost + output_cost
  
  return(list(
    input_cost = input_cost,
    output_cost = output_cost,
    total_cost = total_cost,
    input_rate = input_rate,
    output_rate = output_rate
  ))
}


# ========================================================================
# EXAMPLE USAGE
# ========================================================================
# 
# # Load protocol
# protocol_text <- readLines("path/to/protocol.txt", warn = FALSE) %>% paste(collapse = "\n")
# 
# # Extract single hospital
# result <- extract_hospital_with_images(
#   pdf_path = "E:/ExtractStrategies/Extract_Strategies/PDFs/941_Humber_River_Hospital.pdf",
#   protocol_text = protocol_text,
#   api_key = Sys.getenv("ANTHROPIC_API_KEY"),
#   batch_num = 22,
#   hospital_num = 1,
#   dpi = 150,
#   max_tokens = 16000
# )
# 
# if (result$success) {
#   # Save extraction
#   writeLines(result$extraction_text, "output.txt")
#   
#   # Calculate cost
#   cost <- calculate_cost(result$input_tokens, result$output_tokens, result$model)
#   cat(sprintf("\nCost: $%.4f\n", cost$total_cost))
# }
# 
# ========================================================================
