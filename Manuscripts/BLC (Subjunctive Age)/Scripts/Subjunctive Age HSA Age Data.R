library(tidyverse)
library(here)


# Load CSVs
SDB <- read_csv(here("./CSV Files/SDB/SDB Age and Origin.csv"))
HSA <- read_csv(here("./CSV Files/Adult HS/HSA Age and Origin.csv"))


# Generate averages
SDB_Age <- SDB %>%
  filter(!is.na(Age)) %>% 
  summarize(Mean = mean(Age), SD = sd(Age))

HSA_Age <- HSA %>%
  filter(!is.na(Age)) %>% 
  summarize(Mean = mean(Age), SD = sd(Age))
