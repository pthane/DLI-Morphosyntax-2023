library(tidyverse)
library(dplyr)
library(here)


# Load data
DLI78_EPT <- read_csv(here("./CSV Files/DLI-78/DLI-78 Subjunctive EPT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "DLE-7/8")

MLS78_EPT <- read_csv(here("./CSV Files/MLS-78/MLS-78 Subjunctive EPT.csv")) %>%
  filter(!School == "GBCS") %>% 
  filter(Property == "Intensional subjunctive")

DLI5_EPT <- read_csv(here("./CSV Files/DLI-5/DLI-5 Subjunctive EPT.csv"))  %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "DLE-5")

MLS5_EPT <- read_csv(here("./CSV Files/MLS-5/MLS-5 Subjunctive EPT.csv")) %>%
  filter(!School == "GBCS") %>% 
  filter(Property == "Intensional subjunctive")


## Merge
EPT <- rbind(DLI78_EPT, MLS78_EPT, DLI5_EPT, MLS5_EPT)

EPT_Responses <- EPT %>% 
  filter(!is.na(Mood_Use))

EPT_Non_Subj <- EPT_Responses %>% 
  filter(Mood_Use == 0)


# Create table with non-subjunctive responses
Alternative_Forms_Table <- EPT_Non_Subj %>% 
  count(Prod_Code) %>% 
  tibble()