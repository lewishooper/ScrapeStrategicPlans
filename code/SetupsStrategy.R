#rm(list=ls())



# Load the download script
setwd("E:/Hospital_Strategic_Plans/code/")  # Or wherever script is saved
source("download_strategic_pdfs.R")
CONFIG$test_mode <- FALSE
CONFIG$baseline_mode<-FALSE
results<-main()



# Load the detailed log
log <- read.csv("E:/Hospital_Strategic_Plans/outputs/download_logs/download_log_20260120_090913.csv")

# Check failures
failures <- log %>% filter(action %in% c("download_failed", "validation_failed"))
View(failures)


# reRun for specific failures afeter correction. 
# Test with 3-5 hospitals including known problematic ones
CONFIG$test_mode <- TRUE
CONFIG$test_fac_codes <- c("592", "627", "632", "640", "695")  
CONFIG$baseline_mode <- TRUE  # Simpler for testing
CONFIG$verbose <- TRUE

results <- main()

library(tidyverse)
## Trim to ManualWorkload
ManualSearch<-download_log_20260122_140438 %>%
  filter(action!="baseline_saved")
write.csv(ManualSearch,"E:/Hospital_Strategic_Plans/outputs/ManualSearch.csv")
saveRDS()