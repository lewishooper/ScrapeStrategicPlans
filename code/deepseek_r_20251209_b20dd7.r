data <- readRDS("10_hospital_strategic_plans.rds")
plans_metadata <- data$plans_metadata
key_directions <- data$key_directions


# Read the CSV files
plans_metadata <- read.csv("plans_metadata.csv")
key_directions <- read.csv("key_directions.csv")

# Convert date columns to proper date format
plans_metadata$extraction_date <- as.Date(plans_metadata$extraction_date)

# View the data
head(plans_metadata)
head(key_directions)