setwd("E:/Hospital_Strategic_Plans/code")
###############################################################################
# CreateBaseYamlCode.R   (wrapper-aware, ordered fields, pretty-print)
###############################################################################

library(yaml)
library(urltools)

###############################################################################
# Helpers
###############################################################################

validate_fac <- function(fac) {
  
  if (is.null(fac) || length(fac) == 0) {
    stop("ERROR: FAC field is missing for at least one hospital.")
  }
  
  fac_chr <- as.character(fac)
  
  if (!grepl("^[0-9]{3}$", fac_chr)) {
    stop(paste("Invalid FAC detected:", fac_chr,
               "- FAC must be exactly 3 digits."))
  }
  
  return(fac_chr)
}

extract_base_url <- function(full_url) {
  if (is.null(full_url) || full_url == "") return("")
  parsed <- urltools::url_parse(full_url)
  if (is.na(parsed$domain) || parsed$domain == "") return("")
  paste0(parsed$scheme, "://", parsed$domain)
}

safe_value <- function(x) {
  if (is.null(x)) return("")
  if (length(x) > 1) return(paste(unlist(x), collapse = ""))
  if (length(x) == 1 && is.na(x)) return("")
  if (is.list(x)) return(paste(unlist(x), collapse = ""))
  return(as.character(x))
}

###############################################################################
# Pretty YAML formatting + blank lines
###############################################################################

pretty_indent_yaml <- function(input_path) {
  
  lines <- readLines(input_path, warn = FALSE)
  out <- c()
  
  for (line in lines) {
    
    if (grepl("^- ", line)) {
      out <- c(out, paste0("  ", line))
      next
    }
    
    if (grepl("^\\s*[A-Za-z_]", trimws(line))) {
      out <- c(out, paste0("    ", trimws(line)))
      next
    }
    
    out <- c(out, line)
  }
  
  # add blank line after status:
  out <- gsub("status:.*$", "status:\n", out)
  
  writeLines(out, input_path)
}

###############################################################################
# Main routine
###############################################################################

create_base_yaml <- function(input_yaml = "Base_hospitalsOLD.yaml",
                             output_yaml = "base_hospitals.yaml") {
  
  raw <- yaml::read_yaml(input_yaml)
  
  # handle both possible YAML formats
  if ("hospitals" %in% names(raw)) {
    raw <- raw$hospitals
  }
  
  if (!is.list(raw)) {
    stop("ERROR: YAML input is not a list of hospitals.")
  }
  
  processed <- lapply(raw, function(card) {
    
    # remove unwanted fields
    card$html_structure      <- NULL
    card$pattern             <- NULL
    card$expected_executives <- NULL
    
    # validate FAC
    card$FAC <- validate_fac(card$FAC)
    
    # rename url → leadership_url
    if (!is.null(card$url)) {
      card$leadership_url <- safe_value(card$url)
      card$url <- NULL
    } else {
      card$leadership_url <- ""
    }
    
    # compute base_url
    base_url <- extract_base_url(card$leadership_url)
    
    # reconstruct card in desired order:
    new_card <- list(
      FAC            = card$FAC,
      name           = safe_value(card$name),
      hospital_type  = safe_value(card$hospital_type),
      base_url       = base_url,
      leadership_url = card$leadership_url,
      notes          = "",
      status         = ""
    )
    
    return(new_card)
  })
  
  # Sort by FAC
  processed <- processed[
    order(sapply(processed, function(x) x$FAC))
  ]
  
  # write YAML
  yaml::write_yaml(processed, output_yaml)
  
  # pretty formatting
  pretty_indent_yaml(output_yaml)
  
  cat("✓ base_hospitals.yaml created successfully\n")
}

###############################################################################
# Execute
###############################################################################

create_base_yaml("Base_hospitalsOLD.yaml", "base_hospitals.yaml")


create_base_yaml("Base_hospitalsOLD.yaml", "base_hospitals.yaml")
