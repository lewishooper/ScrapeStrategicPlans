# Ontario Hospital Strategic Plan Scraper - Restart Checklist

**Project Location:** `E:/Hospital_Strategic_Plans`  
**Last Updated:** November 7, 2025  
**Status:** On hold - Ready to restart

---

## Quick Start (When You Return)

### ✅ Step 1: Verify Project Structure
- [ ] Navigate to `E:/Hospital_Strategic_Plans`
- [ ] Confirm these folders exist:
  - `Code/` (contains `hospital_strategic_plan_scraper.R`)
  - `source/` (contains your input CSV)
  - `Outputs/` (will contain results)
  - `strategic_plans/pdfs/` (will contain downloaded PDFs)
  - `strategic_plans/logs/`

### ✅ Step 2: Check R Environment
```r
# Open R/RStudio and run:
setwd("E:/Hospital_Strategic_Plans")

# Verify packages are installed
required_packages <- c("rvest", "httr", "dplyr", "stringr", 
                       "purrr", "robotstxt", "urltools", "lubridate")
missing <- required_packages[!required_packages %in% installed.packages()[,1]]
if(length(missing)) install.packages(missing)

# Load packages to test
library(rvest)
library(httr)
library(dplyr)
```

### ✅ Step 3: Review Your Input Data
- [ ] Open `source/hospital_list.csv` or `source/input_csv.csv`
- [ ] Confirm it has these required columns:
  - `Hospital_Name`
  - `FAC`
  - `URL`
  - `Type`
- [ ] Note how many hospitals are in the file: ______

### ✅ Step 4: Review Previous Results (if any)
- [ ] Check `Outputs/` folder for any existing results files
- [ ] Check `strategic_plans/pdfs/` for any downloaded PDFs
- [ ] If previous run exists, decide whether to:
  - [ ] Continue from where you left off
  - [ ] Start fresh with all hospitals
  - [ ] Process only failed hospitals

---

## Testing Before Full Run

### Option A: Quick Test (5 hospitals)
```r
source("Code/hospital_strategic_plan_scraper.R")

# Create small test file
test_data <- read.csv("source/input_csv.csv")
test_small <- test_data[1:5, ]
write.csv(test_small, "source/test_5_hospitals.csv", row.names = FALSE)

# Run test
results_test <- process_hospitals("source/test_5_hospitals.csv")
View(results_test)
```

### Option B: Medium Test (25 hospitals)
```r
test_medium <- test_data[1:25, ]
write.csv(test_medium, "source/test_25_hospitals.csv", row.names = FALSE)
results_medium <- process_hospitals("source/test_25_hospitals.csv")
```

### Option C: Full Production Run
```r
# Only do this after successful testing!
results_full <- process_hospitals("source/input_csv.csv")
```

---

## What This Script Does (Reminder)

1. **Reads** your CSV file with Ontario hospital information
2. **Extracts** the base domain from any URL you provide (e.g., leadership pages)
3. **Checks** robots.txt to ensure scraping is allowed
4. **Searches** for strategic plan PDFs in two tiers:
   - **Depth 1:** Main homepage
   - **Depth 2:** "About" section (if not found on main page)
5. **Downloads** PDFs automatically when found
6. **Creates** a detailed results CSV with all findings

---

## Key Configuration Settings

Located at top of `hospital_strategic_plan_scraper.R`:

```r
CONFIG <- list(
  # Project base directory - VERIFY THIS MATCHES YOUR SETUP
  base_dir = "E:/Hospital_Strategic_Plans",
  
  # Delay between requests (seconds) - be polite!
  delay_between_requests = 2,
  
  # Search keywords
  plan_keywords = c("strategic plan", "strategic direction", 
                    "corporate plan", "multi-year plan", 
                    "strategic priorities", "strategic framework"),
  
  about_keywords = c("about", "who-we-are", "about-us", 
                     "our-organization")
)
```

**⚠️ Important:** If your project moved to a different drive or folder, update `base_dir`

---

## Expected Outputs

### Results CSV (`Outputs/results_YYYYMMDD_HHMMSS.csv`)
Contains these columns:
- `Hospital_Name`, `FAC`, `Type` - Your input data
- `Base_URL` - Extracted main website
- `Robots_Allowed` - Can we scrape? (TRUE/FALSE)
- `Plan_Found` - Did we find a strategic plan? (TRUE/FALSE)
- `PDF_URL` - Direct link to the PDF
- `Local_Path` - Where the PDF was saved
- `Search_Method` - How we found it
- `Status` - Success, Not Found, Error, etc.
- `Notes` - Additional details
- `Date_Checked` - Timestamp

### Downloaded PDFs (`strategic_plans/pdfs/`)
Filenames follow this pattern:
```
{FAC}_{Hospital_Name}_strategic_plan.pdf
```
Example: `001_Toronto_General_Hospital_strategic_plan.pdf`

---

## Common Status Values

| Status | Meaning | Action Needed |
|--------|---------|---------------|
| **Success** | PDF found and downloaded | None - review PDF quality |
| **Not Found** | No strategic plan found | Manual website review |
| **Blocked by robots.txt** | Scraping not allowed | Manual download required |
| **Error accessing main page** | Website unreachable | Check URL, retry later |
| **Found but download failed** | PDF located but couldn't download | Try manual download |

---

## Analyzing Results

```r
# Load your results (adjust timestamp)
results <- read.csv("Outputs/results_20250107_HHMMSS.csv")

# Quick summary
table(results$Status)

# Success rate by hospital type
library(dplyr)
results %>%
  group_by(Type) %>%
  summarize(
    total = n(),
    found = sum(Plan_Found),
    success_rate = round(found/total * 100, 1)
  )

# Hospitals needing manual review
needs_manual <- results %>%
  filter(Status == "Not Found" | Status == "Blocked by robots.txt") %>%
  select(Hospital_Name, FAC, Base_URL, Status, Notes)

View(needs_manual)
write.csv(needs_manual, "Outputs/manual_review_list.csv", row.names = FALSE)
```

---

## Troubleshooting Quick Reference

### Problem: Script won't run
- Check working directory: `getwd()`
- Should show: `E:/Hospital_Strategic_Plans`
- Set if needed: `setwd("E:/Hospital_Strategic_Plans")`

### Problem: Package errors
```r
# Reinstall all packages
install.packages(c("rvest", "httr", "dplyr", "stringr", 
                   "purrr", "robotstxt", "urltools", "lubridate"))
```

### Problem: Few or no plans found
- Review `Notes` column in results
- Check if `Robots_Allowed = FALSE` for many sites
- Website structure may have changed
- May need to add keywords to `CONFIG$plan_keywords`

### Problem: Script is slow
- This is normal! Default is 2 seconds between requests
- For 100 hospitals: ~3-6 minutes minimum
- For 300 hospitals: ~10-20 minutes
- **Do not reduce delay** - be respectful of hospital servers

---

## Next Steps After Resuming

1. **Run test** with 5 hospitals to verify everything works
2. **Review test results** to ensure PDFs are downloading correctly
3. **Run full scraper** if test succeeds
4. **Analyze results** using code snippets above
5. **Manual review** for "Not Found" hospitals
6. **Document findings** for your research

---

## Files to Keep Track Of

- [ ] `Code/hospital_strategic_plan_scraper.R` - The main script
- [ ] `source/input_csv.csv` - Your hospital list
- [ ] `Outputs/results_*.csv` - Results from each run
- [ ] `strategic_plans/pdfs/*.pdf` - Downloaded strategic plans
- [ ] `README.md` - Full documentation

---

## Contact/Support Reminder

- Review README.md for detailed documentation
- Check console output for error messages
- Always test with small sample first
- Results CSV `Notes` column has specific error details

---

## Quick Command Reference

```r
# Set working directory
setwd("E:/Hospital_Strategic_Plans")

# Load script
source("Code/hospital_strategic_plan_scraper.R")

# Test run (5 hospitals)
test <- read.csv("source/input_csv.csv")[1:5,]
write.csv(test, "source/test.csv", row.names = FALSE)
results <- process_hospitals("source/test.csv")

# Full run
results <- process_hospitals("source/input_csv.csv")

# View results
View(results)
table(results$Status)
```

---

## Pre-Run Checklist ✅

Before starting your scraping session:

- [ ] R/RStudio is open
- [ ] Working directory set to `E:/Hospital_Strategic_Plans`
- [ ] All packages loaded successfully
- [ ] Input CSV file is ready and formatted correctly
- [ ] You have time for the script to run (estimate: # hospitals × 2-3 seconds)
- [ ] Internet connection is stable
- [ ] You've decided: test run or full run?

---

**Ready to restart?** Follow the Quick Start section at the top of this document.

**Good luck with your Ontario hospital strategic plan research!** 🏥📊
