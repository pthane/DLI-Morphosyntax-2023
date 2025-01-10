library(tidyverse)
library(dplyr)
library(here)


# Load data
## EPT
SDB_EPT <- read_csv(here("./CSV Files/SDB/SDB Subjunctive EPT.csv")) %>%
  filter(Property == "Intensional subjunctive")

HSA_EPT <- read_csv(here("./CSV Files/Adult HS/Adult HS Subjunctive EPT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HSA")

DLI78_EPT <- read_csv(here("./CSV Files/DLI-78/DLI-78 Subjunctive EPT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS7/8")

MLS78_EPT <- read_csv(here("./CSV Files/MLS-78/MLS-78 Subjunctive EPT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS7/8")

DLI5_EPT <- read_csv(here("./CSV Files/DLI-5/DLI-5 Subjunctive EPT.csv"))  %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS5")

MLS5_EPT <- read_csv(here("./CSV Files/MLS-5/MLS-5 Subjunctive EPT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS5")


## Merge
EPT <- rbind(SDB_EPT, HSA_EPT, DLI78_EPT, MLS78_EPT, DLI5_EPT, MLS5_EPT) %>% 
  filter(!is.na(Mood_Use))

EPT_Non_Subj <- EPT %>% 
  filter(Mood_Use == 0)


# Create table with non-subjunctive responses
Alternative_Forms_Table <- EPT_Non_Subj %>% 
  count(Prod_Code) %>% 
  tibble()
