# DEBUGGING GUIDE - Hospital Strategic Plan Scraper

## Executive Summary

Your scraper isn't finding strategic plans even though they exist on 4 out of 5 hospitals you checked manually. Based on code review, I've identified several likely issues and created improved versions with fixes.

## Files Created for You

### 1. **scraper_diagnostic.md** 
   - Detailed analysis of potential bugs
   - Testing strategies
   - Priority ranking of fixes

### 2. **hospital_strategic_plan_scraper_IMPROVED.R**
   - Enhanced version with key fixes:
     - Better URL parsing with fallbacks
     - Improved relative-to-absolute URL conversion using xml2::url_absolute()
     - Verbose logging to see what's happening
     - Checks more strategic links (7 instead of 3)
     - Checks multiple About pages (3 instead of 1)
     - Better keyword matching (handles hyphens/underscores)
     - Goes 3 levels deep in About sections

### 3. **test_unity_health.R**
   - Minimal test script for debugging ONE hospital
   - Tests each step independently
   - Shows exactly what links are found
   - Identifies where the logic breaks down

## Top 3 Likely Issues

### 🔴 #1: URL Parsing Problem (MOST LIKELY)
**Problem:** The `get_base_url()` function uses `parsed$domain` from `urltools::url_parse()`, but this field might be NA or incorrect.

**Location:** Lines 70-74 in original script

**How to test:**
```r
library(urltools)
test_url <- "https://unityhealth.to/about-us/leadership/"
parsed <- url_parse(test_url)
print(parsed$domain)  # Is this "unityhealth.to" or NA?
```

**If it's NA:** The base_url becomes "NA://NA" and nothing works!

### 🔴 #2: Relative URL Conversion Fails
**Problem:** Lines 150-155 use complex `url_parse()` + `url_compose()` which may fail silently.

**Original code:**
```r
url = if_else(
  str_starts(url, "http"),
  url,
  url_compose(url_parse(paste0(base_url, "/", str_remove(url, "^/"))))
)
```

**Better approach (in improved version):**
```r
url = sapply(url, function(u) {
  xml2::url_absolute(u, base_url)
})
```

### 🟡 #3: Not Checking Enough Links
**Problem:** Only checks first 3 strategic plan links. The PDF might be on link #4 or #5.

**Fixed in improved version:** Now checks 7 links (configurable)

## How to Debug - Step by Step

### Step 1: Run the Unity Health Test Script

```r
# In RStudio
setwd("E:/Hospital_Strategic_Plans")
source("Code/test_unity_health.R")  # Use the one I created
```

**What to look for:**
1. Does URL parsing work? (Test 1)
2. Does it fetch the homepage? (Test 2)
3. How many links does it find? (Test 3)
4. Do any links match "strategic plan" keywords? (Test 5)
5. Are there PDF links? (Test 6)

**Common outcomes:**

**Outcome A:** "Base URL: NA://NA"
- **Problem:** URL parsing is broken
- **Fix:** Use the improved version OR manually fix `get_base_url()`

**Outcome B:** Links found but no keyword matches
- **Problem:** Keywords don't match actual text on site
- **Solution:** Look at the output, find where strategic plan actually is, add those keywords

**Outcome C:** Keywords match but no PDFs
- **Problem:** Strategic plan is on a subpage, not main page
- **Solution:** The improved version goes deeper (checks more links)

### Step 2: Manual Check (Tell Me Where Unity Health Plan Is)

Visit https://unityhealth.to and tell me:
1. Where is the strategic plan located? (What menu/section?)
2. Is it directly on the homepage or do you need to click through?
3. What exact text describes the link? (e.g., "Our Strategic Plan", "Strategic Direction 2025")
4. Is it a direct PDF link or a page with a PDF on it?

With this info, I can:
- Add the right keywords
- Adjust the search depth
- Create a targeted fix

### Step 3: Test Improved Version on 5 Hospitals

Once we fix Unity Health:

```r
# Create a 5-hospital test file
setwd("E:/Hospital_Strategic_Plans")
hospitals_full <- read.csv("source/input_csv.csv")
hospitals_test <- hospitals_full[1:5, ]
write.csv(hospitals_test, "source/test_5hospitals.csv", row.names = FALSE)

# Run improved scraper
source("Code/hospital_strategic_plan_scraper_IMPROVED.R")
results <- process_hospitals("source/test_5hospitals.csv")

# Review results
View(results)
```

**Look for:**
- Which hospitals succeeded?
- What's different about the ones that failed?
- Check the `Notes` column for clues

## Quick Fixes You Can Try Right Now

### Fix #1: Replace get_base_url() function

Replace lines 70-74 in your original script with:

```r
get_base_url <- function(url) {
  # Try urltools first
  parsed <- url_parse(url)
  
  if (!is.na(parsed$domain) && parsed$domain != "") {
    return(paste0(parsed$scheme, "://", parsed$domain))
  }
  
  # Fallback to regex
  base <- str_extract(url, "https?://[^/]+")
  if (!is.na(base)) {
    return(base)
  }
  
  # Last resort
  stop("Could not extract base URL from: ", url)
}
```

### Fix #2: Simpler URL Conversion

Replace lines 150-155 with:

```r
url = if_else(
  str_starts(url, "http"),
  url,
  # Simpler approach
  if_else(
    str_starts(url, "/"),
    paste0(base_url, url),
    paste0(base_url, "/", url)
  )
)
```

### Fix #3: Turn on Verbose Mode

Add this line after line 15 in the CONFIG list:

```r
CONFIG <- list(
  # ... existing config ...
  
  verbose = TRUE,  # ADD THIS LINE
  
  # ... rest of config ...
)
```

Then add print statements in key functions to see what's happening:

```r
# In is_strategic_plan_link (after line 170):
if (matched) {
  cat("  ✓ MATCH:", text, "\n")
}
```

## What to Do Next

### If you have 30 minutes:
1. Run `test_unity_health.R` and send me the output
2. Manually check where Unity Health's plan actually is
3. Tell me the findings and I'll create a targeted fix

### If you have 1 hour:
1. Try the improved version on 5 hospitals
2. Compare results with original version
3. For any that still fail, manually check and tell me where the plans are
4. I'll refine the improved version based on actual patterns

### If you have 2 hours:
1. Do the above
2. Once we get 5/5 working, test on 25 hospitals
3. Iterate fixes for common failure patterns
4. Run full batch overnight

## Common Questions

**Q: Why isn't robots.txt blocking it?**
A: robots.txt checks are passing, so the issue is in link extraction/matching logic.

**Q: Should I just do it manually?**
A: For 5-10 hospitals, yes. For 100+ hospitals, fixing the scraper is worth it.

**Q: Can you just write the URLs for me?**
A: I can't access the web from here, but you can run the test script and show me the output!

## Next Communication Template

When you report back, please include:

```
HOSPITAL: Unity Health Toronto
URL: https://unityhealth.to

TEST RESULTS:
- URL parsing: [WORKED / FAILED / Base URL was: ___]
- Homepage fetch: [SUCCESS / FAILED]
- Links found: [NUMBER]
- Strategic keyword matches: [NUMBER]
- PDF links: [NUMBER]

MANUAL CHECK:
- Strategic plan location: [Where you found it manually]
- Link text: [Exact text of the link]
- Is it a direct PDF? [YES / NO, it's on a page that has PDF]
```

This will help me create the perfect fix for your situation!
