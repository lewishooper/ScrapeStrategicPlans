#
# etf files
rm(list=ls())
source<-"E:/Hospital_Strategic_Plans/outputs"
library(tidyverse)
strategies<-readRDS(file.path(source,"hospitals_tibble_20251209_143348.rds"))
saveRDS(strategies,file.path(source,"HospitalStragiesV5.rds"))

library(dplyr)
library(stringr)
library(lubridate)
library(tibble)
FindME<-strategies %>%
  filter(Strategy_URL_Found==FALSE)

  
NoPDF<-strategies %>%
  anti_join(FindME,by='FAC') %>%
  filter(PDF_Downloaded==FALSE) %>%
  filter(Content_Type!="html") %>%
  select(FAC,Hospital_Name,PDF_Downloaded, Manual_PDF_URL)
  
MediumConfidence<-strategies %>%
  filter(Download_Confidence=="medium")
 BADhtml<-strategies %>%
   filter(str_detect(Strategy_Notes,"HTTP"))

 HTMLOnly<- strategies %>%
   filter(Content_Type=='html')
 
 ManualDownloads<-strategies %>%
   filter(str_length(Manual_PDF_URL)>0)
 
  
Failures<-strategies %>%
# filter(str_detect(PDF_URL,"h")) %>%
  filter(PDF_Downloaded==FALSE)%>%
  filter(Content_Type!="html") %>%
  
  select(FAC,Hospital_Name,Manual_PDF_URL, Strategy_Notes)

           
 