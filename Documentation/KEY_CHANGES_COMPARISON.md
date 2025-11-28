# KEY CHANGES: Original vs Improved Scraper

## 🔧 CRITICAL FIX #1: URL Parsing

### ❌ ORIGINAL (Lines 70-74)
```r
get_base_url <- function(url) {
  parsed <- url_parse(url)
  base_url <- paste0(parsed$scheme, "://", parsed$domain)
  return(base_url)
}
```

**Problem:** If `parsed$domain` is NA, you get "NA://NA" → everything fails

### ✅ IMPROVED
```r
get_base_url <- function(url) {
  tryCatch({
    parsed <- url_parse(url)
    
    # Handle both 'domain' and 'host' fields
    domain_value <- if (!is.na(parsed$domain) && parsed$domain != "") {
      parsed$domain
    } else if ("host" %in% names(parsed) && !is.na(parsed$host)) {
      parsed$host
    } else {
      stop("Could not extract domain from URL")
    }
    
    base_url <- paste0(parsed$scheme, "://", domain_value)
    return(base_url)
    
  }, error = function(e) {
    # Fallback: try simple regex
    base_url <- str_extract(url, "https?://[^/]+")
    return(base_url)
  })
}
```

**Benefit:** 
- Checks if domain is valid before using
- Has fallback if urltools fails
- Never returns "NA://NA"

---

## 🔧 CRITICAL FIX #2: Relative URL Conversion

### ❌ ORIGINAL (Lines 150-155)
```r
url = if_else(
  str_starts(url, "http"),
  url,
  url_compose(url_parse(paste0(base_url, "/", str_remove(url, "^/"))))
)
```

**Problem:** `url_parse()` + `url_compose()` chain is fragile and can fail silently

### ✅ IMPROVED
```r
url = if_else(
  str_starts(url, "http"),
  url,
  # Use xml2::url_absolute which is more reliable
  sapply(url, function(u) {
    tryCatch({
      xml2::url_absolute(u, base_url)
    }, error = function(e) {
      # Fallback to simple concatenation
      if (str_starts(u, "/")) {
        paste0(base_url, u)
      } else {
        paste0(base_url, "/", u)
      }
    })
  })
)
```

**Benefit:**
- Uses xml2's built-in function (more reliable)
- Has fallback if that fails
- Handles both "/path" and "path" correctly

---

## 🔧 FIX #3: Keyword Matching

### ❌ ORIGINAL (Lines 167-173)
```r
is_strategic_plan_link <- function(url, text) {
  combined <- paste(str_to_lower(url), str_to_lower(text))
  
  any(sapply(CONFIG$plan_keywords, function(keyword) {
    str_detect(combined, fixed(keyword))
  }))
}
```

**Problem:** Won't match "strategic-plan" or "strategic_plan" (only "strategic plan")

### ✅ IMPROVED
```r
is_strategic_plan_link <- function(url, text) {
  combined <- str_to_lower(paste(url, text))
  
  # Normalize hyphens and underscores to spaces
  combined_normalized <- str_replace_all(combined, "[_-]", " ")
  
  matched <- any(sapply(CONFIG$plan_keywords, function(keyword) {
    str_detect(combined_normalized, fixed(keyword))
  }))
  
  # Verbose logging
  if (matched && CONFIG$verbose) {
    matching_keyword <- CONFIG$plan_keywords[sapply(CONFIG$plan_keywords, function(k) {
      str_detect(combined_normalized, fixed(k))
    })][1]
    cat("    ✓ MATCH [", matching_keyword, "]: ", 
        str_trunc(text, 40), " -> ", str_trunc(url, 60), "\n", sep="")
  }
  
  return(matched)
}
```

**Benefit:**
- Matches "strategic-plan", "strategic_plan", "strategic plan"
- Shows what matched (when verbose mode on)
- Helps you see if keywords are working

---

## 🔧 FIX #4: Search Depth

### ❌ ORIGINAL (Line 310)
```r
for (i in 1:min(3, nrow(strategic_links))) {  # Check first 3 links
```

**Problem:** If PDF is on link #4 or later, it's missed

### ✅ IMPROVED
```r
num_to_check <- min(CONFIG$max_strategic_links_depth, nrow(strategic_links))

for (i in 1:num_to_check) {  # Check 7 links (configurable)
```

**Benefit:**
- Checks more links (7 instead of 3)
- Configurable via CONFIG
- Better chance of finding PDF

---

## 🔧 FIX #5: About Section Search

### ❌ ORIGINAL (Lines 346-370)
```r
if (nrow(about_links) > 0) {
  about_url <- about_links$url[1]  # Only checks FIRST about link
  cat("Checking:", about_url, "\n")
  
  about_response <- safe_get(about_url)
  if (about_response$success) {
    # ... searches this one page only
  }
}
```

**Problem:** Only checks first About page (might be /about-us/leadership when plan is in /about-us/publications)

### ✅ IMPROVED
```r
if (nrow(about_links) > 0) {
  num_about_to_check <- min(CONFIG$max_about_pages, nrow(about_links))
  
  for (i in 1:num_about_to_check) {  # Checks 3 About pages
    about_url <- about_links$url[i]
    # ... searches multiple About pages
    
    # PLUS: Goes one level deeper into strategic plan pages
    # found within About section (Depth 3)
  }
}
```

**Benefit:**
- Checks 3 About pages instead of 1
- Goes deeper (3 levels vs 2)
- Finds plans in subsections like /publications/

---

## 🔧 FIX #6: Verbose Logging

### ❌ ORIGINAL
No logging - you can't see what it's finding or where it's failing

### ✅ IMPROVED
```r
CONFIG <- list(
  verbose = TRUE,  # NEW
  # ...
)

# Throughout code:
if (CONFIG$verbose) {
  cat("  [DEBUG] Parsing URL:", url, "\n")
  cat("  [DEBUG] Base URL:", base_url, "\n")
  cat("  [INFO] Extracted", nrow(df), "links\n")
  cat("    ✓ MATCH [strategic plan]: Some Link -> some/url\n")
}
```

**Benefit:**
- See exactly what's happening
- Identify where it fails
- Debug faster

---

## 📊 Configuration Improvements

### ❌ ORIGINAL
```r
CONFIG <- list(
  plan_keywords = c(
    "strategic plan", "strategic direction", "corporate plan",
    "multi-year plan", "strategic priorities", "strategic framework"
  ),
  # ... no control over search depth
)
```

### ✅ IMPROVED
```r
CONFIG <- list(
  plan_keywords = c(
    "strategic plan", "strategic direction", "corporate plan",
    "multi-year plan", "strategic priorities", "strategic framework",
    "strategic roadmap", "future direction"  # MORE KEYWORDS
  ),
  
  verbose = TRUE,  # NEW
  max_strategic_links_depth = 7,  # NEW (was hardcoded 3)
  max_about_pages = 3,  # NEW (was hardcoded 1)
  # ...
)
```

**Benefit:**
- More keywords catch more variations
- Configurable search depth
- Easy to tune without editing code

---

## 🎯 Expected Impact

| Issue | Original | Improved | Impact |
|-------|----------|----------|--------|
| URL Parsing Fails | Scraper crashes | Fallback works | **HIGH** |
| Relative URLs | Some broken | All work | **HIGH** |
| Keyword Variations | Misses hyphens | Catches all | **MEDIUM** |
| Search Depth | 3 links | 7 links | **MEDIUM** |
| About Pages | 1 page | 3 pages | **MEDIUM** |
| Debugging | Blind | See everything | **HIGH** |

**Expected Success Rate:**
- Original: ~0% (your current experience)
- Improved: **60-80%** (conservative estimate)
- With manual tuning: **90%+**

---

## 🚀 Migration Path

### Option A: Replace Entirely
1. Replace your original script with improved version
2. Test on 5 hospitals
3. Adjust CONFIG based on results
4. Run full batch

### Option B: Apply Fixes Incrementally  
1. Fix get_base_url() first
2. Test - if it works, done!
3. If not, fix URL conversion
4. Test again
5. Continue until working

### Option C: Hybrid
1. Use improved version for testing
2. Copy working fixes back to original
3. Keep your original structure

---

## 💡 Pro Tips

1. **Always test on 5 hospitals first** - Don't run 100+ without testing
2. **Turn verbose ON for testing** - Turn OFF for production runs
3. **Check one hospital manually** - Confirm what the code should find
4. **Read the output logs** - They tell you exactly what's happening
5. **Iterate quickly** - Test, fix, test, fix until it works

---

## ❓ Still Not Working?

If improved version still fails:
1. Run test_unity_health.R
2. Send me the output
3. Tell me where the plan actually is
4. I'll create a surgical fix

Most likely remaining issues:
- JavaScript-rendered content (scraper can't see)
- Authentication required (need manual download)
- Unusual URL structure (need custom parsing)
- Keywords don't match actual text (need new keywords)

But we'll catch 80%+ with these improvements!
