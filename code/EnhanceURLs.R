###############################################################################
#  Integrated YAML Enhancer for Hospital Cards
#  - Renames url → executive_url
#  - Adds base_url
#  - Validates base_url HTTP status
#  - Checks robots.txt scraping permissions
#  - Writes updated YAML with pretty indentation
###############################################################################

library(yaml)
library(httr)
library(robotstxt)
library(urltools)

###############################################################################
#  Pretty YAML indentation helper
###############################################################################
pretty_indent_yaml <- function(input_path) {
  lines <- readLines(input_path, warn = FALSE)
  out   <- c()
  
  for (line in lines) {
    
    # List item under hospitals:
    if (grepl("^- ", line)) {
      out <- c(out, paste0("  ", line))       # indent 2
      next
    }
    
    # Nested items (FAC:, name:, etc.)
    if (grepl("^\\s+[A-Za-z_]", line)) {
      out <- c(out, paste0("    ", trimws(line)))  # indent 4
      next
    }
    
    out <- c(out, line)
  }
  
  writeLines(out, input_path)
}
#### Safe value
safe_value <- function(x) {
  
  # NULL → ""
  if (is.null(x)) return("")
  
  # If x is a vector (length > 1), apply safe_value to each element
  if (is.atomic(x) && length(x) > 1) {
    return(lapply(as.list(x), safe_value))
  }
  
  # If x is logical/character/numeric scalar NA → ""
  if (length(x) == 1 && is.na(x)) return("")
  
  # Lists: recursively sanitize children
  if (is.list(x)) return(lapply(x, safe_value))
  
  # Everything else is safe
  return(x)
}

###############################################################################
#  URL Helper Functions
###############################################################################



# Extract scheme://domain from a URL
extract_base_url <- function(full_url) {
  parsed <- urltools::url_parse(full_url)
  if (is.na(parsed$domain) || parsed$domain == "") return(NA_character_)
  base <- paste0(parsed$scheme, "://", parsed$domain)
  return(base)
}

# Validate base URL (returns list(valid = TRUE/FALSE, status = "..."))
validate_base_url <- function(base_url) {
  if (is.na(base_url) || base_url == "") {
    return(list(valid = FALSE, status = "Missing base_url"))
  }
  
  try({
    resp <- httr::GET(base_url, timeout(10))
    code <- httr::status_code(resp)
    
    if (code >= 200 && code < 400) {
      return(list(valid = TRUE,  status = paste("OK - HTTP", code)))
    } else {
      return(list(valid = FALSE, status = paste("Invalid - HTTP", code)))
    }
  }, silent = TRUE)
  
  return(list(valid = FALSE, status = "Error connecting"))
}

# Check robots.txt permissions
check_robots_txt <- function(base_url, executive_url) {
  
  if (is.na(base_url) || base_url == "") {
    return(list(found = FALSE,
                allows = NA,
                comment = "No base_url"))
  }
  
  robots_url <- paste0(base_url, "/robots.txt")
  
  # Attempt to fetch robots.txt
  txt <- tryCatch(httr::GET(robots_url, timeout(10)),
                  error = function(e) NULL)
  
  if (is.null(txt) || httr::status_code(txt) >= 400) {
    return(list(found = FALSE,
                allows = NA,
                comment = "robots.txt not found"))
  }
  
  # Parse robots.txt using robotstxt
  parser <- tryCatch(
    robotstxt::robotstxt(domain = base_url),
    error = function(e) NULL
  )
  
  if (is.null(parser)) {
    return(list(found = TRUE,
                allows = NA,
                comment = "robots.txt retrieved but could not be parsed"))
  }
  
  allowed <- tryCatch(
    parser$check(executive_url),
    error = function(e) NA
  )
  
  comment <- if (is.na(allowed)) {
    "Parsing failed"
  } else if (allowed) {
    "Allowed"
  } else {
    "Disallowed"
  }
  
  return(list(found = TRUE,
              allows = allowed,
              comment = comment))
}

###############################################################################
#  Main Enhancer Function
###############################################################################
enhance_yaml_urls <- function(input_yaml = "base_hospitals.yaml",
                              output_yaml = "hospitals_with_baseurl.yaml") {
  
  # Load YAML
  raw <- yaml::read_yaml(input_yaml)
  
  if (!("hospitals" %in% names(raw))) {
    stop("ERROR: top-level YAML must contain 'hospitals:'")
  }
  
  cards <- raw$hospitals
  
  # Process each hospital card
  updated_cards <- lapply(cards, function(card) {
    
    # Remove unwanted fields
    card$html_structure <- NULL
    card$pattern <- NULL
    card$expected_executives <- NULL
    
    # Rename url → executive_url
    if (!is.null(card$url)) {
      card$executive_url <- card$url
      card$url <- NULL
    }
    
    # URL fields
    exec_url <- safe_value(card$executive_url)
    base_url <- extract_base_url(exec_url)
    card$base_url <- safe_value(base_url)
    
    # Validate
    v <- validate_base_url(base_url)
    card$base_url_valid  <- safe_value(v$valid)
    card$base_url_status <- safe_value(v$status)
    
    # robots.txt
    r <- check_robots_txt(base_url, exec_url)
    card$robots_txt_found           <- safe_value(r$found)
    card$robots_txt_allows_scraping <- safe_value(r$allows)
    card$robots_txt_comment         <- safe_value(r$comment)
    
    # Force notes and status to exist as blank strings
    card$notes  <- ""
    card$status <- ""
    
    # Normalize all fields for YAML safety
    card <- lapply(card, safe_value)
    
    return(card)
  })
  
  # Preserve sorting by FAC if possible
  FAC_values <- suppressWarnings(as.numeric(sapply(updated_cards, function(x) x$FAC)))
  num_idx <- !is.na(FAC_values)
  
  updated_cards <- updated_cards[
    order(
      ifelse(num_idx, 0, 1),
      FAC_values,
      sapply(updated_cards, function(x) as.character(x$FAC))
    )
  ]
  
  # Write raw YAML
  yaml::write_yaml(list(hospitals = updated_cards), output_yaml)
  
  # Pretty-format YAML
  pretty_indent_yaml(output_yaml)
  
  cat("✓ Updated YAML written to:", output_yaml, "\n")
}

###############################################################################
#  Running Example
###############################################################################

setwd("E:/Hospital_Strategic_Plans/code")
 enhance_yaml_urls("base_hospitals.yaml", "hospitals_with_baseurl.yaml")

 
 

 raw <- yaml::read_yaml("base_hospitals.yaml")
 
 # Find any fields that are NULL, NA, or vectors
 inspect_bad_values <- function(x, path = "") {
   if (is.null(x)) cat("NULL at:", path, "\n")
   else if (is.atomic(x) && any(is.na(x))) cat("NA at:", path, "Value:", x, "\n")
   else if (is.atomic(x) && length(x) > 1) cat("Vector at:", path, "Values:", x, "\n")
   else if (is.list(x)) {
     for (n in names(x)) {
       inspect_bad_values(x[[n]], paste0(path, "/", n))
     }
   }
 }
 
 inspect_bad_values(raw)
 
