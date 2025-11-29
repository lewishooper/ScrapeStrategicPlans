setwd("E:/Hospital_Strategic_Plans")
source("code/IdentifyAndDownloadStrategicPDF.R")
results <- run_phase2()  



urfAILS<-manual_review_needed_20251129_110239 %>%
  filter(Strategy_URL_Found==FALSE)%>%
  select(FAC,Hospital_Name,Base_URL,Notes)
