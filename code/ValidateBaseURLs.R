###############################################################################
# ValidateBaseURLs.R
# ------------------
# Reads base_hospitals.yaml and, for each hospital, validates base_url and
# adds three new fields:
#   base_url_validated: true/false
#   robots_allowed: true/false
#   last_validated: "2025-11-28"   # fixed as requested
#
# Output: base_hospitals_validated.yaml
###############################################################################

library(yaml)
library(httr)
library(robotstxt)

###############################################################################
# Helper functions
###############################################################################

is_valid_url <- function(url) {
  is.character(url) &&
    !is.null(url) &&
    nzchar(url) &&
    grepl("^https?://", url)
}

# Simple HTTP check: treat 200–399 as valid
check_base_url <- function(url) {
  
  if (!is_valid_url(url)) {
    return(FALSE)
  }
  
  resp <- tryCatch(
    httr::GET(url, timeout(10)),
    error = function(e) NULL
  )
  
  if (is.null(resp)) return(FALSE)
  
  code <- httr::status_code(resp)
  return(code >= 200 && code < 400)
}

# Simple robots.txt check on base_url
# TRUE  = allowed or unknown
# FALSE = explicitly disallowed
check_robots_allowed <- function(base_url) {
  
  if (!is_valid_url(base_url)) {
    return(FALSE)
  }
  
  # Extract domain for robotstxt::paths_allowed
  parsed <- httr::parse_url(base_url)
  domain <- parsed$hostname
  
  if (is.null(domain) || domain == "") {
    return(FALSE)
  }
  
  allowed <- tryCatch(
    suppressWarnings(
      robotstxt::paths_allowed(
        paths  = "/",
        domain = domain
      )
    ),
    error = function(e) NA
  )
  
  # If robots.txt is missing or unknown, treat as TRUE (not explicitly blocked)
  if (is.na(allowed)) return(TRUE)
  
  return(isTRUE(allowed))
}

# Pretty-print YAML: indent and blank line after status:
pretty_indent_yaml <- function(input_path) {
  
  lines <- readLines(input_path, warn = FALSE)
  out <- c()
  
  for (line in lines) {
    
    # list item line
    if (grepl("^- ", line)) {
      out <- c(out, paste0("  ", line))
      next
    }
    
    # key: value lines
    if (grepl("^\\s*[A-Za-z_]", trimws(line))) {
      out <- c(out, paste0("    ", trimws(line)))
      next
    }
    
    out <- c(out, line)
  }
  
  # Ensure a blank line after each status: line
  out <- gsub("status:.*$", "status:\n", out)
  
  writeLines(out, input_path)
}

###############################################################################
# Main validator
###############################################################################

validate_base_urls <- function(input_yaml  = "base_hospitals.yaml",
                               output_yaml = "base_hospitals_validated.yaml") {
  
  yaml_obj <- yaml::read_yaml(input_yaml)
  
  # Support both formats:
  #  - top-level list
  #  - top-level with 'hospitals:' wrapper
  has_wrapper <- FALSE
  if (is.list(yaml_obj) && "hospitals" %in% names(yaml_obj)) {
    hospitals <- yaml_obj$hospitals
    has_wrapper <- TRUE
  } else {
    hospitals <- yaml_obj
  }
  
  # Fixed date as requested
  today_str <- "2025-11-28"
  
  validated <- lapply(hospitals, function(card) {
    
    base_url <- if (!is.null(card$base_url)) card$base_url else ""
    
    base_ok   <- check_base_url(base_url)
    robots_ok <- if (base_ok) check_robots_allowed(base_url) else FALSE
    
    # Rebuild card in explicit field order.
    # We DO NOT carry forward url_status or any other stray fields.
    new_card <- list(
      FAC               = card$FAC,
      name              = card$name,
      hospital_type     = card$hospital_type,
      base_url          = base_url,
      base_url_validated = base_ok,
      robots_allowed     = robots_ok,
      last_validated     = today_str,
      leadership_url    = card$leadership_url,
      notes             = card$notes,
      status            = card$status
    )
    
    return(new_card)
  })
  
  # Re-wrap if original had a 'hospitals:' wrapper
  if (has_wrapper) {
    out_obj <- list(hospitals = validated)
  } else {
    out_obj <- validated
  }
  
  yaml::write_yaml(out_obj, output_yaml)
  pretty_indent_yaml(output_yaml)
  
  cat("✓ base_url validation complete\n")
  cat("✓ Output written to:", output_yaml, "\n")
}

###############################################################################
# Run
###############################################################################
setwd("E:/Hospital_Strategic_Plans/code")
validate_base_urls()
