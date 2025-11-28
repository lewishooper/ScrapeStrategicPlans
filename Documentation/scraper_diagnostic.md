# Hospital Strategic Plan Scraper - Diagnostic Analysis

## Problem Overview
The scraper is not finding strategic plans even though manual review found them in 4 out of 5 hospitals.

## Potential Issues Identified

### 🔴 CRITICAL ISSUE #1: URL Parsing Problem
**Location:** Line 154 in `get_page_links()`

```r
url = if_else(
  str_starts(url, "http"),
  url,
  url_compose(url_parse(paste0(base_url, "/", str_remove(url, "^/"))))
)
```

**Problem:** This approach to converting relative URLs to absolute URLs is overly complex and may fail. The `url_parse()` and `url_compose()` from `urltools` package can be fragile.

**Better approach:**
```r
# Use xml2::url_absolute() which is more reliable
url = if_else(
  str_starts(url, "http"),
  url,
  xml2::url_absolute(url, base_url)
)
```

Or even simpler with base R:
```r
url = if_else(
  str_starts(url, "http"),
  url,
  ifelse(str_starts(url, "/"),
    paste0(base_url, url),
    paste0(base_url, "/", url)
  )
)
```

### 🔴 CRITICAL ISSUE #2: get_base_url() Using Wrong Field
**Location:** Lines 70-74

```r
get_base_url <- function(url) {
  parsed <- url_parse(url)
  base_url <- paste0(parsed$scheme, "://", parsed$domain)
  return(base_url)
}
```

**Problem:** `url_parse()` from `urltools` returns `$domain` field, but this might not work as expected. Let's check what the actual fields are.

**Testing needed:**
```r
library(urltools)
test_url <- "https://unityhealth.to/about-us/leadership/"
parsed <- url_parse(test_url)
names(parsed)  # What fields does it actually have?
```

**Likely fix:** Use `$host` or a combination of fields:
```r
get_base_url <- function(url) {
  parsed <- url_parse(url)
  # Check if it's 'domain', 'host', or 'hostname'
  base_url <- paste0(parsed$scheme, "://", 
                     ifelse(!is.na(parsed$domain), parsed$domain, parsed$host))
  return(base_url)
}
```

### 🟡 ISSUE #3: Keyword Matching May Be Too Strict
**Location:** Lines 167-173

The function uses `fixed(keyword)` which means exact matching. This is actually GOOD (not a bug), but worth noting that it won't catch variations like:
- "Strategic-Plan" (with hyphen in URL)
- "strategic_plan" (with underscore)
- "strategicplan" (no space)

**Potential enhancement:**
```r
is_strategic_plan_link <- function(url, text) {
  combined <- str_to_lower(paste(url, text))
  
  # Also check for variations without spaces/hyphens
  combined_normalized <- str_replace_all(combined, "[_-]", " ")
  
  any(sapply(CONFIG$plan_keywords, function(keyword) {
    str_detect(combined_normalized, fixed(keyword))
  }))
}
```

### 🟡 ISSUE #4: Limited Depth on Strategic Plan Pages
**Location:** Lines 310-331

```r
if (nrow(strategic_links) > 0) {
  cat("Checking strategic plan page links...\n")
  for (i in 1:min(3, nrow(strategic_links))) {  # Check first 3 links
```

**Problem:** Only checks first 3 non-PDF strategic links. If the PDF is on the 4th or 5th link, it will be missed.

**Suggestion:** Increase to 5-7 links, or check all links (with a reasonable max like 10).

### 🟢 ISSUE #5: About Section Search is Too Limited
**Location:** Lines 339-370

The about section search only checks ONE about page. Many hospitals have:
- /about-us
- /about-us/leadership
- /about-us/publications
- /about-us/strategic-planning

**Enhancement:** Check multiple about pages and go 2 levels deep.

## Testing Strategy

### Step 1: Test URL Parsing
Create a test script to verify `get_base_url()` and `get_page_links()` work correctly:

```r
library(urltools)
test_urls <- c(
  "https://unityhealth.to/about-us/leadership/",
  "https://www.uhn.ca/TorontoGeneral/AboutUs/Leadership",
  "https://sunnybrook.ca/content/?page=about-sunnybrook"
)

for (url in test_urls) {
  cat("\nTest URL:", url, "\n")
  parsed <- url_parse(url)
  cat("Parsed fields:", paste(names(parsed), collapse=", "), "\n")
  cat("Domain field:", parsed$domain, "\n")
  
  # Try the current get_base_url function
  base <- paste0(parsed$scheme, "://", parsed$domain)
  cat("Base URL:", base, "\n")
}
```

### Step 2: Test Link Extraction
Download a known hospital homepage HTML and test link extraction:

```r
# Manually save unityhealth.to homepage as test.html
page <- read_html("test.html")
links <- get_page_links(page, "https://unityhealth.to")
View(links)

# Check for strategic plan keywords
strategic <- links %>%
  filter(is_strategic_plan_link(url, text))
View(strategic)
```

### Step 3: Add Verbose Logging
Add print statements to see what's happening:

```r
# In is_strategic_plan_link function, add:
is_strategic_plan_link <- function(url, text) {
  combined <- paste(str_to_lower(url), str_to_lower(text))
  
  result <- any(sapply(CONFIG$plan_keywords, function(keyword) {
    str_detect(combined, fixed(keyword))
  }))
  
  # Debugging: Print matches
  if (result) {
    cat("  ✓ MATCH:", text, "->", url, "\n")
  }
  
  return(result)
}
```

## Recommended Fixes Priority

### HIGH PRIORITY (Fix These First)
1. **Fix `get_base_url()`** - Verify it returns correct base URLs
2. **Fix relative URL conversion** in `get_page_links()` - Use simpler, more reliable method
3. **Add verbose logging** - See what links are actually being found

### MEDIUM PRIORITY
4. **Increase depth of strategic link checking** - from 3 to 7-10
5. **Check multiple about pages** - not just the first one
6. **Add keyword variations** - handle hyphens and underscores

### LOW PRIORITY
7. Add more plan keywords like "annual report" or "vision"
8. Add option to search /publications or /reports sections

## Next Steps

1. Create a minimal test script that:
   - Loads a saved HTML file (from Unity Health)
   - Tests URL parsing
   - Tests link extraction
   - Tests keyword matching
   
2. Fix the identified issues one at a time

3. Test on 2-3 known hospitals before running full batch

Would you like me to create the test scripts?
