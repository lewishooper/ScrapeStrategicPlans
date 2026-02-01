# Data derivied from deepseek
# Gave it the data from the processed folder which was all the strategic 
# I have recieved

# it returns two files. 
# the first being the hospital information 128 rows (all hospitals)
# the second being its extraction of the strategic directions,1832 rows of strategic directions
#rm(list=ls())
source<-"E:/Hospital_Strategic_Plans/source"
library(tidyverse)
batch1<-read.csv2(file.path(source,"batch1.csv"))
batch2<-read.csv2(file.path(source,"batch2.csv"))
