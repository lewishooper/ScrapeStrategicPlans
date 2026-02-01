source("parse_extraction_output_v2.R")

data <- parse_and_export(
  input_file = "E:/Hospital_Strategic_Plans/processed/Phase3_L1_Extraction_20260131.txt",
  output_dir = "E:/Hospital_Strategic_Plans/parsed_data"
)

View(data$hospitals)
View(data$priorities)

#source("test_parser.R")
