# Manual Download Recording Script
# Run this AFTER you manually download the PDFs

library(yaml)

setwd("E:/Hospital_Strategic_Plans")

# Read current YAML
hospitals <- read_yaml("code/Hospital_strategy.yaml")

# FACs you manually downloaded
manual_downloads <- data.frame(
  FAC = c("695", "640", "632"),
  actual_pdf_url = c(
    "https://providencecare.ca/wp-content/uploads/2025/06/StrategicPlan-New-Mission-and-Vision-Statements-Final-2025-28.pdf",
    "https://cgmh.on.ca/uploads/files/CGMH%20Strategic%20Plan%202025-2030.pdf",  # URL encoded
    "https://www.nygh.on.ca/wp-content/uploads/2025/05/NYGH_StrategicPlan_2025_05_28_v24.pdf"
  ),
  manual_notes = c(
    "Manual download - 403 error with automated script",
    "Manual download - URL encoding issue with spaces",
    "Manual download - 404 error, may need to check actual URL on site"
  ),
  stringsAsFactors = FALSE
)

# Update each hospital record
for (i in seq_along(hospitals)) {
  fac <- hospitals[[i]]$FAC
  
  match_idx <- which(manual_downloads$FAC == fac)
  
  if (length(match_idx) > 0) {
    cat("Updating FAC", fac, "\n")
    
    # Update the strategy_search section
    hospitals[[i]]$strategy_search$pdf_downloaded <- TRUE
    hospitals[[i]]$strategy_search$download_confidence <- "manual"
    
    # Set folder and filename
    safe_name <- gsub("[^A-Za-z0-9]", "_", hospitals[[i]]$name)
    safe_name <- gsub("_{2,}", "_", safe_name)
    safe_name <- gsub("^_|_$", "", safe_name)
    
    folder_name <- paste0(fac, "_", safe_name)
    year_month <- format(Sys.time(), "%Y%m")
    filename <- paste0("Strategy_", year_month, "_", fac, ".pdf")
    
    hospitals[[i]]$strategy_search$local_folder <- folder_name
    hospitals[[i]]$strategy_search$local_filename <- filename
    hospitals[[i]]$strategy_search$requires_manual_review <- FALSE
    hospitals[[i]]$strategy_search$strategy_notes <- manual_downloads$manual_notes[match_idx]
  }
}

# Save updated YAML
write_yaml(hospitals, "code/Hospital_strategy.yaml")

cat("\nYAML updated for", nrow(manual_downloads), "manually downloaded PDFs\n")