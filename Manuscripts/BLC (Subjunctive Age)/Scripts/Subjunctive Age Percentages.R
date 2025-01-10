library(tidyverse)
library(here)


# Load data
## EPT
SDB_EPT <- read_csv(here("./CSV Files/SDB/SDB Subjunctive EPT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "SDBA")

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


## FCT
SDB_FCT <- read_csv(here("./CSV Files/SDB/SDB Subjunctive FCT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "SDBA")

HSA_FCT <- read_csv(here("./CSV Files/Adult HS/Adult HS Subjunctive FCT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HSA")

DLI78_FCT <- read_csv(here("./CSV Files/DLI-78/DLI-78 Subjunctive FCT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS7/8")

MLS78_FCT <- read_csv(here("./CSV Files/MLS-78/MLS-78 Subjunctive FCT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS7/8")

DLI5_FCT <- read_csv(here("./CSV Files/DLI-5/DLI-5 Subjunctive FCT.csv"))  %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS5")

MLS5_FCT <- read_csv(here("./CSV Files/MLS-5/MLS-5 Subjunctive FCT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS5")


## Merge
EPT <- rbind(SDB_EPT, HSA_EPT, DLI78_EPT, MLS78_EPT, DLI5_EPT, MLS5_EPT)
FCT <- rbind(SDB_FCT, HSA_FCT, DLI78_FCT, MLS78_FCT, DLI5_FCT, MLS5_FCT)

Joint <- rbind(EPT, FCT) %>% 
  filter(!is.na(Mood_Use))


# Generate statistics
Descriptives <- Joint %>% 
  group_by(Group, Task) %>% 
  summarize(Mean = mean(Mood_Use), SD = sd(Mood_Use))
