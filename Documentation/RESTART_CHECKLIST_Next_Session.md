# RESTART CHECKLIST - Next Session
## Hospital Strategic Plan Extraction Project

---

## 📋 Pre-Session Setup (5 minutes)

### Environment Setup
- [ ] Open RStudio
- [ ] Set working directory: `setwd("E:/Hospital_Strategic_Plans")`
- [ ] Load latest script: `source("code/IdentifyAndDownloadStrategicPDF_with_Depth2.R")`
- [ ] Verify script loaded without errors

### Verify File Structure
- [ ] Check master YAML exists: `E:/Hospital_Strategic_Plans/code/base_hospitals_validated.yaml`
- [ ] Check test YAML exists: `E:/Hospital_Strategic_Plans/code/base_hospitals_validated_TEST.yaml`
- [ ] Verify Backups folder has recent backups
- [ ] Check Outputs folder for last session's files

### Review Last Session Results
- [ ] Open `SESSION_SUMMARY_Nov29_2025.md` - Read achievements and current status
- [ ] Open `phase2_summary_20251129_110239.csv` - Last run results
- [ ] Open `manual_review_needed_20251129_110239.csv` - 7 hospitals needing review
- [ ] Open `diagnostic_log_20251129_110140.txt` - Detailed diagnostics

---

## 🎯 Session Objectives Reminder

**Primary Goal:** Improve PDF extraction from 40% → 60-80%

**Current Status:**
- Strategy URL finding: 80% ✅ (8 of 10 hospitals)
- PDF download: 40% ⚠️ (4 of 10 hospitals)

**Focus:** Analyze why 4 hospitals find strategy URL but NO PDF

---

## 🔍 PHASE 1: Analyze PDF Extraction Failures (20-30 minutes)

### Step 1: Identify the 4 Failure Cases

From `manual_review_needed_20251129_110239.csv`, find hospitals where:
- `Strategy_URL_Found = TRUE`
- `PDF_Found = FALSE`

Document for each:
- [ ] FAC number
- [ ] Hospital name
- [ ] Strategy URL that was found
- [ ] What the Notes column says

### Step 2: Review Diagnostic Logs

For each of the 4 failures, find their section in `diagnostic_log_20251129_110140.txt`:

**For each hospital, document:**
- [ ] Was strategy URL found at depth-1 or depth-2?
- [ ] What does it say when checking that strategy URL?
- [ ] "No PDFs found on page" OR "No links found on page"?
- [ ] How many links were found on the strategy page?

### Step 3: Manual Investigation

For **2-3** of the failures, manually visit their strategy URLs:

**Visit the Strategy_URL in browser and document:**

1. **Is there a PDF visible on the page?**
   - [ ] Yes → How is it linked? (Continue to #2)
   - [ ] No → Are there sub-pages to check? (Note which ones)

2. **If PDF exists, how is it linked?**
   - [ ] Text link (e.g., "Download Strategic Plan")
   - [ ] Button/image link (no text, just icon)
   - [ ] Embedded iframe/viewer
   - [ ] Behind a JavaScript action
   - [ ] On a sub-page (which one?)

3. **What's the actual PDF URL?**
   - [ ] Copy the PDF URL
   - [ ] Does URL contain "strategic" or "plan" keywords?
   - [ ] Is the filename descriptive or generic?

### Step 4: Pattern Identification

After reviewing 2-3 failures, answer:

- [ ] **Common issue #1:** ____________________
- [ ] **Common issue #2:** ____________________
- [ ] **Are patterns similar or different?** ____________________

**Pattern Examples to Look For:**
- PDFs are all linked via images/buttons (no text)
- PDFs are all one more click away (depth-3 needed)
- PDFs are embedded in viewers (can't be downloaded)
- PDF links have no strategic plan keywords in filename

---

## 🛠️ PHASE 2: Implement Improvements (30-45 minutes)

Based on patterns identified in Phase 1, choose improvement strategy:

### Option A: Enhanced PDF Link Detection
**If issue is:** PDFs linked via images/buttons without text

**Solution:**
- [ ] Modify `find_strategy_pdf()` to look for:
  - All links ending in `.pdf` (even with no text)
  - Download buttons/icons
  - Links with "download" in URL

### Option B: Depth-3 Search for PDFs
**If issue is:** PDFs are on sub-pages from strategy page

**Solution:**
- [ ] Add `search_depth_3_for_pdf()` function
- [ ] From strategy page, identify promising sub-pages
- [ ] Search those sub-pages for PDFs

### Option C: Alternative PDF Patterns
**If issue is:** PDFs have different naming/linking patterns

**Solution:**
- [ ] Add broader PDF filename patterns
- [ ] Look for "plan", "report", "strategy" in ANY PDF
- [ ] Accept first PDF if on known strategic plan page

### Option D: Combination Approach
**If issues are mixed:**
- [ ] Implement multiple improvements
- [ ] Test incrementally

**Claude will provide code snippets based on your findings from Phase 1**

---

## 🧪 PHASE 3: Test Improvements (20-30 minutes)

### Run Updated Script

```r
# Ensure CONFIG is set for testing
CONFIG$test_mode <- TRUE
CONFIG$test_sample_size <- 10
CONFIG$output_yaml <- "code/base_hospitals_validated_TEST.yaml"

# Source the updated script
source("code/IdentifyAndDownloadStrategicPDF_with_Depth2.R")

# Run the test
results <- run_phase2()
```

### Review Results

- [ ] Check console output - any improvements?
- [ ] Open new `phase2_summary_[timestamp].csv`
- [ ] Compare to previous run:
  - Previous: 4 of 10 PDFs downloaded (40%)
  - Current: ___ of 10 PDFs downloaded (__%)
- [ ] Check `diagnostic_log_[timestamp].txt` for new patterns

### Success Metrics

**If 6+ PDFs downloaded (60%+):**
- [ ] ✅ SUCCESS - Ready to expand to 25 hospitals
- [ ] Document what worked
- [ ] Plan 25-hospital test

**If 5-6 PDFs downloaded (50-60%):**
- [ ] ⚠️ PROGRESS - Consider one more iteration
- [ ] Review remaining failures
- [ ] Decide: expand to 25 or refine further?

**If still 4 PDFs (40%):**
- [ ] 🔍 INVESTIGATE - Review diagnostic logs
- [ ] Check if improvements are working
- [ ] May need different approach

---

## 🎯 PHASE 4: Decide Next Steps (10 minutes)

### If 60%+ Success Rate

**Expand to 25 Hospitals:**
```r
CONFIG$test_sample_size <- 25
```
- [ ] Update CONFIG
- [ ] Run test
- [ ] Review results
- [ ] Check if patterns hold with larger sample

**Goals for 25-hospital test:**
- Maintain 60%+ success rate
- Identify any new failure patterns
- Build confidence for full deployment

### If 40-60% Success Rate

**Options:**
1. **One more refinement iteration**
   - [ ] Identify remaining issues
   - [ ] Implement targeted fix
   - [ ] Test again with 10 hospitals

2. **Hybrid approach**
   - [ ] Accept 50-60% automation
   - [ ] Develop efficient manual review workflow
   - [ ] Test with 25 hospitals anyway

3. **Add more hospitals to 10-sample**
   - [ ] Test with 15-20 instead
   - [ ] Get more pattern data
   - [ ] Then decide on improvements

### If <40% Success Rate

**Debug mode:**
- [ ] Review implementation - did changes work?
- [ ] Check diagnostic logs for errors
- [ ] Verify depth-2 search is still working
- [ ] May need to revert and try different approach

---

## 📊 Data to Collect During Session

### Before Running Tests
- [ ] Current date/time: _______________
- [ ] Script version being used: _______________
- [ ] Number of hospitals in test: _______________

### After Each Test Run
- [ ] Timestamp of run: _______________
- [ ] Strategy URLs found: ___ / ___
- [ ] PDFs downloaded: ___ / ___
- [ ] Success rate: ___%
- [ ] High confidence downloads: ___
- [ ] Medium confidence downloads: ___
- [ ] Manual review needed: ___

### Patterns Observed
- [ ] Common failure type #1: _______________
- [ ] Common failure type #2: _______________
- [ ] Hospital types with best success: _______________
- [ ] Hospital types with worst success: _______________

---

## 💾 End of Session Checklist

### Save All Work
- [ ] Save any modified R scripts
- [ ] Commit recent backups
- [ ] Note filenames of key outputs:
  - Latest summary CSV: _______________
  - Latest diagnostic log: _______________
  - Latest test YAML: _______________

### Document Session
- [ ] Update session log with:
  - Success rate achieved
  - Improvements implemented
  - Patterns identified
  - Next steps planned

### Prepare for Next Session
- [ ] If 60%+ achieved → Prepare for 25-hospital test
- [ ] If <60% → Document blocking issues
- [ ] Create note with quick-start commands for next time

---

## 🚀 Quick-Start Commands (Copy-Paste Ready)

### Standard Test Run (10 hospitals)
```r
setwd("E:/Hospital_Strategic_Plans")
source("code/IdentifyAndDownloadStrategicPDF_with_Depth2.R")
# Results will auto-run and save
```

### Check Last Results
```r
# Read last summary CSV
last_results <- read.csv("Outputs/phase2_summary_20251129_110239.csv")
View(last_results)

# Check success rate
table(last_results$PDF_Downloaded)
table(last_results$Strategy_URL_Found)
```

### Manual URL Investigation Template
```r
# For investigating specific hospital
hospitals <- read_yaml("code/base_hospitals_validated_TEST.yaml")
fac_592 <- hospitals[[1]]  # Adjust index as needed
cat("Base URL:", fac_592$base_url, "\n")
cat("Strategy URL:", fac_592$strategy_search$strategy_url, "\n")
cat("Notes:", fac_592$strategy_search$strategy_notes, "\n")
```

---

## 📞 Key Reference Information

### File Locations
- **Master YAML:** `E:/Hospital_Strategic_Plans/code/base_hospitals_validated.yaml`
- **Test YAML:** `E:/Hospital_Strategic_Plans/code/base_hospitals_validated_TEST.yaml`
- **Current Script:** `E:/Hospital_Strategic_Plans/code/IdentifyAndDownloadStrategicPDF_with_Depth2.R`
- **Outputs:** `E:/Hospital_Strategic_Plans/Outputs/`
- **Downloaded PDFs:** `E:/Hospital_Strategic_Plans/strategic_plans/`
- **Backups:** `E:/Hospital_Strategic_Plans/Backups/`
- **Documentation:** `E:/Hospital_Strategic_Plans/Documentation/`

### Current Performance
- **Strategy URL Finding:** 80% (8/10) ✅
- **PDF Download:** 40% (4/10) ⚠️
- **Target:** 60-80% for PDF download

### Success Thresholds
- **60%** = Expand to 25 hospitals
- **70%** = Strong performance, near deployment ready
- **80%** = Excellent, ready for full deployment

---

## ❓ Questions to Answer This Session

Priority questions:
1. [ ] Why do 4 hospitals find strategy URL but no PDF?
2. [ ] What specific patterns cause PDF extraction to fail?
3. [ ] Can we improve PDF detection to reach 60%+?
4. [ ] Should we expand to 25 hospitals or iterate further?

Nice-to-know questions:
5. [ ] Do certain hospital types have better success rates?
6. [ ] Are there website platform patterns (WordPress, custom, etc.)?
7. [ ] What's the optimal balance of automation vs. manual review?

---

## 🎯 Session Success Criteria

**Minimum Success:**
- [ ] Understand why PDFs aren't being found (pattern identification)
- [ ] Attempt at least one improvement strategy
- [ ] Document findings clearly

**Good Success:**
- [ ] Identify clear patterns in failures
- [ ] Implement targeted improvements
- [ ] Achieve 50-60% PDF download rate
- [ ] Have clear path to 60%+

**Excellent Success:**
- [ ] Achieve 60%+ PDF download rate
- [ ] Run successful 25-hospital test
- [ ] Ready for next phase (full deployment or manual review workflow)

---

**Remember:** Focus on **pattern identification first**, then **targeted improvements**. Don't try to solve everything at once!

Good luck! 🚀
