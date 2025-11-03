# Ontario Hospital Strategic Plan Scraper

## Overview
This R script automates the discovery and download of strategic plan PDFs from Ontario Acute Care hospital websites. It implements a smart, tiered search strategy and respects robots.txt policies.

## Features

✅ **Smart Search Strategy**
- Extracts base URL from leadership pages
- Searches main page first (Depth 1)
- Falls back to "About" sections if not found
- Identifies strategic plan variations (strategic plan, corporate plan, multi-year plan, etc.)

✅ **Ethical Scraping**
- Checks robots.txt before scraping
- Implements delays between requests (2 seconds default)
- Uses appropriate user agent

✅ **Comprehensive Tracking**
- Records all search attempts
- Tracks success/failure with detailed notes
- Preserves hospital identifiers (FAC)
- Timestamps all activities

✅ **Robust Error Handling**
- Handles network errors gracefully
- Continues processing even if individual sites fail
- Logs all issues for review

## Requirements

### R Packages
```r
install.packages(c(
  "rvest",      # Web scraping
  "httr",       # HTTP requests
  "dplyr",      # Data manipulation
  "stringr",    # String operations
  "purrr",      # Functional programming
  "robotstxt",  # robots.txt checking
  "urltools",   # URL parsing
  "lubridate"   # Date/time handling
))
```

## Input Format

Your CSV file must contain these columns:
- **Hospital_Name**: Full name of the hospital
- **FAC**: Unique facility identifier (will not change even if hospital name changes)
- **URL**: Link to leadership or any hospital page (will be parsed to main site)
- **Type**: Hospital classification (Teaching, Large, Small, etc.)

### Example Input CSV
```csv
Hospital_Name,FAC,URL,Type
Toronto General Hospital,001,https://www.uhn.ca/TorontoGeneral/AboutUs/Leadership,Teaching
St. Michael's Hospital,002,https://unityhealth.to/about-us/leadership/,Teaching
```

## Usage

### Setup
1. Place `hospital_strategic_plan_scraper.R` in the `Code/` folder
2. Place your hospital CSV file in the `source/` folder
3. Open R/RStudio and set your working directory:

```r
setwd("E:/Hospital_Strategic_Plans")
```

### Basic Usage
```r
# Load the script
source("Code/hospital_strategic_plan_scraper.R")

# Process hospitals from your CSV in the source folder
results <- process_hospitals("source/hospital_list.csv")

# View results
View(results)
```

### Testing Workflow

#### 1. Small Sample Test (5 hospitals)
```r
# Set working directory
setwd("E:/Hospital_Strategic_Plans")

# Create a small test file
test_small <- read.csv("source/hospital_list.csv")
test_small <- test_small[1:5, ]
write.csv(test_small, "source/test_small.csv", row.names = FALSE)

# Run the scraper
source("Code/hospital_strategic_plan_scraper.R")
results_small <- process_hospitals("source/test_small.csv")

# Check results
View(results_small)
table(results_small$Status)
```

#### 2. Medium Sample Test (25 hospitals)
```r
# If small test looks good, try 25
test_medium <- read.csv("source/hospital_list.csv")
test_medium <- test_medium[1:25, ]
write.csv(test_medium, "source/test_medium.csv", row.names = FALSE)

results_medium <- process_hospitals("source/test_medium.csv")
```

#### 3. Full Run
```r
# Process all hospitals
results_full <- process_hospitals("source/hospital_list.csv")
```

## Output

## Project Structure

Your project should be organized as follows:

```
E:/Hospital_Strategic_Plans/
├── Code/
│   └── hospital_strategic_plan_scraper.R  (place script here)
├── source/
│   └── hospital_list.csv  (your input CSV goes here)
├── Outputs/
│   └── results_YYYYMMDD_HHMMSS.csv  (results saved here)
└── strategic_plans/
    ├── pdfs/
    │   ├── 001_Toronto_General_Hospital_strategic_plan.pdf
    │   ├── 002_St_Michaels_Hospital_strategic_plan.pdf
    │   └── ...
    └── logs/
        └── (future log files)
```

**Note:** The script is configured to use `E:/Hospital_Strategic_Plans` as the base directory. If your project is located elsewhere, update the `CONFIG$base_dir` variable at the top of the script.

### Output CSV Columns
- **Hospital_Name**: Hospital name from input
- **FAC**: Facility identifier from input
- **Type**: Hospital type from input
- **Input_URL**: Original URL provided
- **Base_URL**: Extracted main hospital website
- **Robots_Allowed**: TRUE/FALSE - scraping permission
- **Robots_Note**: Any notes from robots.txt check
- **Plan_Found**: TRUE/FALSE - whether strategic plan was found
- **PDF_URL**: Direct link to the strategic plan PDF
- **Local_Path**: Filename where PDF was saved
- **Search_Method**: How the plan was found (e.g., "Main page (Depth 1)")
- **Status**: Success, Not Found, Error, etc.
- **Notes**: Additional details about the search
- **Date_Checked**: Timestamp of the search

### Status Values
- **Success**: Plan found and downloaded
- **Not Found**: No strategic plan found after complete search
- **Blocked by robots.txt**: Scraping not allowed
- **Error accessing main page**: Could not connect to website
- **Found but download failed**: PDF identified but couldn't download

## Configuration

The script is pre-configured for your project structure. The key settings are in the CONFIG list:

```r
CONFIG <- list(
  # Keywords for strategic plans
  plan_keywords = c(
    "strategic plan", "strategic direction", "corporate plan",
    "multi-year plan", "strategic priorities", "strategic framework"
  ),
  
  # Keywords for about sections
  about_keywords = c("about", "who-we-are", "about-us", "our-organization"),
  
  # Delay between requests (be polite!)
  delay_between_requests = 2,  # seconds
  
  # Base project directory - UPDATE THIS if your project is elsewhere
  base_dir = "E:/Hospital_Strategic_Plans",
  
  # Output folders (relative to base_dir)
  output_folder = "Outputs",
  pdf_folder = "strategic_plans/pdfs",
  log_folder = "strategic_plans/logs"
)
```

### Important: Update base_dir if needed
If your project is not at `E:/Hospital_Strategic_Plans`, change the `base_dir` value:
```r
base_dir = "C:/Users/YourName/Documents/Hospital_Strategic_Plans"
```

## Search Strategy Details

### Step 1: Parse to Main Domain
```
Input:  https://hospital.ca/about-us/leadership
Output: https://hospital.ca
```

### Step 2: Search Main Page (Depth 1)
- Scans homepage for strategic plan keywords
- Looks for direct PDF links
- Checks up to 3 promising links (e.g., /publications, /reports)

### Step 3: Fallback - "About" Section
If not found in Step 2:
- Identifies "About" pages
- Searches one level deep into About section
- Looks for strategic plan PDFs

### Step 4: Record Results
- Downloads PDF if found
- Flags for manual review if not found

## Troubleshooting

### No Strategic Plans Found
- Check if `Robots_Allowed` is FALSE - may need manual download
- Review the `Notes` column for specific issues
- Some hospitals may use different terminology - consider adding keywords
- Website may require JavaScript (not supported by this script)

### Download Failures
- Check PDF_URL manually in browser
- Some PDFs may be behind authentication
- File size limits may cause timeouts

### Slow Performance
- Default delay is 2 seconds between requests
- Can reduce `delay_between_requests` but be respectful
- Consider running overnight for large batches

## Manual Review

For hospitals flagged as "Not Found", you can:
1. Visit the `Base_URL` manually
2. Search for strategic plan
3. Update the results CSV with manual findings

## Example: Analyzing Results

```r
# Load results (adjust timestamp in filename)
results <- read.csv("Outputs/results_20250101_120000.csv")

# Summary statistics
table(results$Status)
table(results$Search_Method)

# Success rate by hospital type
library(dplyr)
results %>%
  group_by(Type) %>%
  summarize(
    total = n(),
    found = sum(Plan_Found),
    success_rate = found/total
  )

# Hospitals requiring manual review
manual_review <- results %>%
  filter(Status == "Not Found") %>%
  select(Hospital_Name, FAC, Base_URL)

write.csv(manual_review, "Outputs/manual_review_needed.csv", row.names = FALSE)
```

## Ethical Considerations

This script:
- ✅ Respects robots.txt
- ✅ Implements polite delays
- ✅ Only accesses public information
- ✅ Does not overwhelm servers
- ✅ Identifies itself with user agent

## Support & Issues

For questions or issues:
1. Check the output CSV `Notes` column
2. Review console output for specific errors
3. Test with small sample first
4. Ensure all packages are installed

## Version History

- **v1.0** (2025-11-02): Initial release
  - Smart two-tier search strategy
  - robots.txt checking
  - Comprehensive tracking
  - FAC identifier support

## License

This script is provided for research and administrative purposes related to Ontario healthcare.
