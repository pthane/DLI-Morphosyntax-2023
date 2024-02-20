library(tidyverse)
library(dplyr)


# Load data
DLI78_EPT <- read_csv("./CSV Files/DLI-78/DLI-78 Subjunctive EPT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "DLE-7/8")

MLS78_EPT <- read_csv("./CSV Files/MLS-78/MLS-78 Subjunctive EPT.csv") %>%
  filter(!School == "GBCS") %>% 
  filter(Property == "Intensional subjunctive")

DLI5_EPT <- read_csv("./CSV Files/DLI-5/DLI-5 Subjunctive EPT.csv")  %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "DLE-5")

MLS5_EPT <- read_csv("./CSV Files/MLS-5/MLS-5 Subjunctive EPT.csv") %>%
  filter(!School == "GBCS") %>% 
  filter(Property == "Intensional subjunctive")

SDB_EPT <- read_csv("./CSV Files/SDB/SDB Subjunctive EPT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "SDBA")


## Merge
EPT <- rbind(SDB_EPT, DLI78_EPT, MLS78_EPT, DLI5_EPT, MLS5_EPT)

EPT_Responses <- EPT %>% 
  filter(!is.na(Mood_Use))

EPT_Heritage <- EPT_Responses %>% 
  filter(!Group == "SDBA")

EPT_Non_Subj <- EPT_Responses %>% 
  filter(Mood_Use == 0) %>% 
  filter(!Group == "SDBA")


# Create table with non-subjunctive responses
Alternative_Forms_Table <- EPT_Non_Subj %>% 
  count(Prod_Code) %>% 
  tibble()

