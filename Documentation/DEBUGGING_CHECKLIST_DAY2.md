# Tomorrow's Debugging Checklist
## Phase 2 Script Testing - Day 2

---

## Pre-Session Setup ☑️

- [ ] Open RStudio
- [ ] Set working directory: `setwd("E:/Hospital_Strategic_Plans")`
- [ ] Confirm latest script is in place: `code/IdentifyAndDownloadStrategicPDF.R`
- [ ] Verify YAML file has `hospitals:` key at top
- [ ] Confirm 5 test hospitals in YAML file
- [ ] Have this checklist open for reference
- [] set parameters for "Claude"

---

## Test Run #1 - Fixed Keyword Matching 🔍

### Run the Script
- [ ] Source the script: `source("code/IdentifyAndDownloadStrategicPDF.R")`
- [ ] Execute: `results <- run_phase2()`
- [ ] Let it complete (or note where it fails)

### Observe Console Output
- [ ] Are 5 hospitals loaded from YAML? (Should see: "Loaded 5 hospitals from YAML")
- [ ] Does keyword matching work without errors?
- [ ] Are strategic plan pages found for any hospitals?
- [ ] Do PDFs get downloaded?

### Check Specific Hospitals

**FAC 593 (Most likely to succeed):**
- [ ] Are links extracted from main page?
- [ ] Is the link `https://mhalliance.on.ca/about/strategic-plan/` found?
- [ ] Is it identified as a strategic plan page?
- [ ] Is the PDF found on that page?
- [ ] Does download succeed?

**FAC 596 (Manual URL test):**
- [ ] Does it use the manual_pdf_url?
- [ ] Does download succeed?
- [ ] Is requires_manual_review set to FALSE?

**FAC 592 (Needs depth-2):**
- [ ] Is `/about/` link found?
- [ ] Note: This one probably won't find the PDF yet (needs depth-2 search)

---

## Analyze Results 📊

### Review Outputs
- [ ] Open diagnostic log: `Outputs/diagnostic_log_YYYYMMDD_HHMMSS.txt`
- [ ] Open summary CSV: `Outputs/phase2_summary_YYYYMMDD_HHMMSS.csv`
- [ ] Check if manual review CSV was created

### Success Metrics
- [ ] How many strategic plan pages found? ___ / 5
- [ ] How many PDFs found? ___ / 5
- [ ] How many PDFs downloaded? ___ / 5
- [ ] Success rate: ____%

### Failure Analysis
For each failed hospital, document:
- [ ] Hospital FAC: ___
- [ ] Strategic plan page found? Yes / No
- [ ] If yes, why no PDF? ___________
- [ ] If no, why no page found? ___________

---

## Diagnostic Review 🔬

### For Each Hospital, Check Diagnostic Log:

**Link Extraction:**
- [ ] Are links being extracted? (Should see: "[PARSE] Found XX raw <a> tags")
- [ ] Are they being filtered properly? (Should see: "[PARSE] After filtering: XX valid links")

**Keyword Matching:**
- [ ] Are the first 20 links displayed?
- [ ] Do you see obvious strategic plan links in the list?
- [ ] Are they being matched? (Should see: "Found X potential strategy pages")

**If Keywords Not Matching:**
- [ ] List the URLs that SHOULD match but don't: ___________
- [ ] Check if keyword needs adjustment (hyphenation, spacing, etc.)

---

## Decision Points 🤔

### If >60% Success Rate:
- [ ] Test with 25 hospitals: `CONFIG$test_sample_size <- 25`
- [ ] Review patterns in successful matches
- [ ] Document what worked well

### If 40-60% Success Rate:
- [ ] Review diagnostic logs for patterns
- [ ] Identify common failure modes
- [ ] Decide: Add keywords OR Implement depth-2 search OR Both

### If <40% Success Rate:
- [ ] Deep dive into diagnostic logs
- [ ] Check if fundamental approach needs revision
- [ ] Consider whether depth-2 search is essential

---

## Specific Issues to Check ✓

### Issue: No Strategic Plan Pages Found
**Symptoms:** "No strategic plan pages found" or "No strategy keywords found"

**Debug Steps:**
- [ ] Review first 20 links in diagnostic output
- [ ] Manually search for keywords in the URL/text columns
- [ ] Are "About" pages present but not matching?
- [ ] Check current keyword list matches what you expect

**Possible Fixes:**
- [ ] Add missing keyword variations
- [ ] Implement case-insensitive substring matching (currently using fixed())
- [ ] Check if "About" should be added to tier 2 keywords

### Issue: Strategic Plan Page Found But No PDF
**Symptoms:** "Strategy page found but no PDF"

**Debug Steps:**
- [ ] Note the strategy_url from console output
- [ ] Manually visit that URL in browser
- [ ] Is there a PDF on that page?
- [ ] How is the PDF linked? (text, button, image)

**Possible Fixes:**
- [ ] Check if PDF is on a sub-page (needs depth-2)
- [ ] Check if PDF link has no text (image link issue)
- [ ] Check if PDF filename has no keywords (should still download with medium confidence)

### Issue: PDF Found But Download Failed
**Symptoms:** "PDF found but download failed"

**Debug Steps:**
- [ ] Note the PDF URL from error message
- [ ] Try to access PDF URL directly in browser
- [ ] Check file size (might be timeout)
- [ ] Check if authentication required

**Possible Fixes:**
- [ ] Increase timeout: `CONFIG$request_timeout <- 60`
- [ ] Note for manual download if behind auth wall

---

## Implementation Tasks (If Needed) 🛠️

### Add More Keywords
If diagnostic shows obvious strategic plan links not matching:
- [ ] List keywords to add: ___________
- [ ] Add to CONFIG$tier1_keywords or CONFIG$tier2_keywords
- [ ] Re-run test

### Implement Depth-2 Search
If strategic plans are consistently one level deeper (e.g., /about/strategic-plan/):
- [ ] Create `search_depth_2()` function
- [ ] Search within "About" or similar pages
- [ ] Test with FAC 592 specifically

### Adjust Confidence Logic
If too many false positives with "first PDF" approach:
- [ ] Review medium-confidence downloads
- [ ] Decide if "first PDF" is too aggressive
- [ ] Consider adding filename filtering

---

## Documentation Tasks 📝

### Update Session Log
- [ ] Record success rate
- [ ] Document any new keywords added
- [ ] Note any implementation changes
- [ ] List hospitals requiring manual review

### Update Code Comments
- [ ] If keywords changed, update CONFIG section comments
- [ ] If logic changed, update function documentation

### Prepare for Next Iteration
- [ ] If <100% success, identify hospitals for manual PDF URL addition
- [ ] Create list of hospitals needing depth-2 search
- [ ] Plan next testing phase (25 hospitals or more iteration on 5)

---

## End of Session Checklist ✅

- [ ] Save all code changes
- [ ] Commit diagnostic logs and outputs
- [ ] Update PROJECT_SESSION_LOG.md with today's activities
- [ ] Update SESSION_SUMMARY if significant progress
- [ ] Note any blocking issues for next session
- [ ] Document any "aha moments" or key insights

---

## Quick Reference Commands

```r
# Start session
setwd("E:/Hospital_Strategic_Plans")
source("code/IdentifyAndDownloadStrategicPDF.R")

# Run test
results <- run_phase2()

# View results
View(results[[1]]$strategy_search)  # First hospital results

# Check summary CSV
summary <- read.csv("Outputs/phase2_summary_LATEST.csv")
table(summary$PDF_Downloaded)

# Adjust settings if needed
CONFIG$diagnostic_mode <- FALSE  # Reduce output
CONFIG$test_sample_size <- 25    # Test more hospitals
CONFIG$test_mode <- FALSE        # Full run

# Re-run after changes
results <- run_phase2()
```

---

## Success Criteria for Tomorrow

**Minimum Acceptable:**
- [ ] Script runs without errors
- [ ] At least 1 hospital successfully downloads PDF

**Good Progress:**
- [ ] 2-3 hospitals successfully download PDFs (40-60%)
- [ ] Clear patterns emerge for failures
- [ ] Path forward is obvious

**Excellent Progress:**
- [ ] 3-4 hospitals successfully download PDFs (60-80%)
- [ ] Ready to test with 25 hospitals

**Outstanding:**
- [ ] 4-5 hospitals successfully download PDFs (80-100%)
- [ ] Proceed directly to full deployment testing

---

## Notes Section

Use this space during debugging to jot down observations:

**Observations:**
- 
- 
- 

**Ideas to try:**
- 
- 
- 

**Questions for later:**
- 
- 
- 
