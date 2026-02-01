# SIMPLIFIED DIAGNOSTIC: HTML Extraction for FAC 739
# Avoids any html_remove issues

library(rvest)
library(stringr)

url <- "https://www.ndmh.ca/care-2030-strategic-plan"

cat("========================================\n")
cat("TESTING HTML EXTRACTION FOR FAC 739\n")
cat("URL:", url, "\n")
cat("========================================\n\n")

# Test 1: Can we load the page?
cat("TEST 1: Loading page...\n")
page <- tryCatch({
  read_html(url)
}, error = function(e) {
  cat("ERROR:", e$message, "\n")
  return(NULL)
})

if (is.null(page)) {
  stop("Cannot load page - stopping here")
}
cat("  ✓ Page loaded successfully\n\n")

# Test 2: Get ALL text from body (no removal)
cat("TEST 2: Extracting all body text...\n")
all_text <- page %>% 
  html_nodes("body") %>% 
  html_text() %>% 
  str_squish()

cat("  Total characters:", nchar(all_text), "\n")
cat("  First 300 chars:\n")
cat("  ", substr(all_text, 1, 300), "\n\n")

# Test 3: Try specific content selectors
cat("TEST 3: Testing specific content areas...\n")

selectors <- c("main", "article", ".content", "#content", "#main-content", ".main")

for (sel in selectors) {
  nodes <- page %>% html_nodes(sel)
  if (length(nodes) > 0) {
    text <- nodes %>% html_text() %>% str_squish()
    cat("  ✓ Found '", sel, "': ", nchar(text), " chars\n", sep = "")
  }
}
cat("\n")

# Test 4: Look for specific strategic plan keywords
cat("TEST 4: Searching for strategic plan keywords...\n")
keywords <- c("strategic", "vision", "mission", "priority", "goal", "objective")
found_keywords <- sapply(keywords, function(kw) {
  grepl(kw, all_text, ignore.case = TRUE)
})

cat("  Keywords found:\n")
for (i in seq_along(keywords)) {
  cat("    ", keywords[i], ": ", if(found_keywords[i]) "YES" else "NO", "\n", sep = "")
}
cat("\n")

# Test 5: Check page structure
cat("TEST 5: Analyzing page structure...\n")
scripts <- page %>% html_nodes("script")
iframes <- page %>% html_nodes("iframe")
cat("  Script tags:", length(scripts), "\n")
cat("  Iframes:", length(iframes), "\n\n")

# Test 6: Save full text to file
cat("TEST 6: Saving extracted text...\n")
output_file <- "E:/Hospital_Strategic_Plans/Outputs/test_739_extracted.txt"
writeLines(all_text, output_file)
cat("  Saved to:", output_file, "\n")
cat("  File size:", file.size(output_file), "bytes\n\n")

# FINAL VERDICT
cat("========================================\n")
cat("VERDICT:\n")
cat("========================================\n")

if (nchar(all_text) < 100) {
  cat("❌ FAILED: Very little text extracted (", nchar(all_text), " chars)\n")
  cat("   This page likely requires JavaScript to display content.\n")
  cat("   rvest cannot handle JavaScript-rendered pages.\n\n")
  cat("RECOMMENDATION: Mark as 'unavailable' or create PDF manually\n")
} else if (nchar(all_text) < 1000) {
  cat("⚠ WARNING: Limited text (", nchar(all_text), " chars)\n")
  cat("   Mostly navigation/headers extracted.\n\n")
  cat("RECOMMENDATION: Check the output file to see what was extracted\n")
} else {
  cat("✓ SUCCESS: Good extraction (", nchar(all_text), " chars)\n")
  cat("   This should work!\n\n")
  cat("NEXT STEP: Lower min_text_chars in CONFIG if needed\n")
}

cat("\nPlease review the saved file:\n")
cat("  ", output_file, "\n")
cat("\nDoes it contain the strategic plan content?\n")