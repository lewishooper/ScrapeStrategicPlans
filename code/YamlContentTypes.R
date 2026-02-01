#Quick YAML content_type check
library(yaml)
library(dplyr)
library(tidyr)

# Load YAML
hospitals <- read_yaml("E:/Hospital_Strategic_Plans/code/Hospital_strategy.yaml")

# Extract to dataframe
df <- data.frame(
  FAC = sapply(hospitals, function(h) h$FAC),
  Hospital_Name = sapply(hospitals, function(h) h$name),
  content_type = sapply(hospitals, function(h) {
    ct <- h$strategy_search$content_type
    if (is.null(ct)) return(NA)
    return(ct)
  }),
  stringsAsFactors = FALSE
)

# View
View(df)

# Check distribution
table(df$content_type, useNA = "ifany")

list.dirs("E:/Hospital_Strategic_Plans/strategic_plans")
# Load YAML

#Lets checkNumberof hospitals in yaml.load()
DFStrategy<-tibble(list.dirs("E:/Hospital_Strategic_Plans/strategic_plans")) %>%
  rename(FullString=1) %>%
  mutate(HospitalString=str_remove(FullString,"E:/Hospital_Strategic_Plans/strategic_plans/")) %>%
  filter(HospitalString!="E:/Hospital_Strategic_Plans/strategic_plans") %>%
  separate(
    HospitalString,
    into = c("FAC", "Hospital"),
    sep = "_",
    extra = "merge",
    remove = TRUE
  )

anti_join(df,DFStrategy,by="FAC")
