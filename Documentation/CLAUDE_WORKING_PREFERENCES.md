# Claude Working Preferences - Hospital_Strategic_Plans Project

## Project Structure

**Project Root:** `E:\Hospital_Strategic_Plans`

### Folder Organization
- **`E:\Hospital_Strategic_Plans\code`** - All R scripts and YAML files
- **`E:\Hospital_Strategic_Plans\Documentation`** - Project documentation
- **`E:\Hospital_Strategic_Plans\output`** - Temporary outputs (transitory, not pushed to GitHub, can be regenerated)
- **`E:\Hospital_Strategic_Plans\processed`** - Monthly reports from process_hospital_data.R (permanent storage)
- **`E:\Hospital_Strategic_Plans\ArchiveCode`** - Archived/deprecated code
- **`E:\Hospital_Strategic_Plans\tracking`** - Issue tracking files
- **`E:\Hospital_Strategic_Plans\Backups`** -backups
- **`E:\Hospital_Strategic_Plans\strategic_plans`** - respository for recovered plans
### Version Control
- Complete Backups maintained on separate server with versioning system
- Output& backups  folder not pushed to GitHub (transitory/regenerable)

## Working Methodology

### Debugging Approach
- **User debugs in own R/RStudio environment**
- Claude provides code snippets and guidance, not full execution
- User executes code and reports results
- User outlines issues with data and code samples when appropriate
- This approach gives user better insight into troubleshooting logic

### Code Output Standards

#### YAML Formatting
When outputting YAML for hospital cards:
- **Add 2 spaces at the beginning of each row**
- This fits R/RStudio formatting styles
- Example:
  ```yaml
    - FAC: '980'
      name: 'TORONTO UNITY HEALTH TORONTO'
      pattern: div_classes
  ```

#### R Code Preferences
- Provide snippets for user to run in their environment
- Focus on problem isolation and correction
- Allow user to execute and verify results

### Documentation
- **Do not write additional documentation without asking first**
- User prefers to control documentation scope and timing
- Exception: When explicitly requested

## Communication Style

### Problem-Solving Flow
1. User describes issue with context
2. User provides relevant data/code samples
3. Claude suggests diagnostic approach
4. User executes in their R environment
5. User reports results
6. Iterate until resolved

### Code Delivery
- Provide code as snippets for user integration
- Explain reasoning behind approach
- Let user decide on implementation

## Project-Specific Context

### Key Files
- **base_hospitals_validated** - Master hospital configuration (in `code/`)
- **IdentifyAndDowloadStrategicPDF.R** idenfity and download strategic pdf (in 'code/)
- Helper scripts and testing utilities (in `code/`)

### Workflow
- Quarterly process
- Results stored in `strategic_plans/` folder with quarterly review and organization

## Session Startup

Quick reminders for new sessions:
- "Debug in my R environment"
- "our working directory is E:/Hospital_Strategic_Plans"
- Reference this document Claude_working_preferences.MD for full context
- pls review these files:
      identifyAndDownloadStratetgicPDF.R and the
      Hospital_strategy.yaml 
---

*Last Updated: December 1 2025*
*This document should be uploaded to the Project Knowledge for persistent reference*
