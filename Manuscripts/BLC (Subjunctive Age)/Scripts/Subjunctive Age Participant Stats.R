library(tidyverse)


# Load data
## Load CSVs
SDB <- read_csv("./CSV Files/SDB/SDB Subjunctive FCT.csv") %>%
  filter(Item == "FCT-02") %>%
  filter(!is.na(DELE))

HSA <- read_csv("./CSV Files/Adult HS/Adult HS Subjunctive FCT.csv") %>%
  filter(Item == "FCT-02") %>% 
  mutate(Group = "HSA")

DLI78 <- read_csv("./CSV Files/DLI-78/DLI-78 Subjunctive FCT.csv") %>%
  filter(Item == "FCT-02") %>% 
  mutate(Group = "HS7/8")

MLS78 <- read_csv("./CSV Files/MLS-78/MLS-78 Subjunctive FCT.csv") %>%
  filter(Item == "FCT-02") %>% 
  mutate(Group = "HS7/8")

DLI5 <- read_csv("./CSV Files/DLI-5/DLI-5 Subjunctive FCT.csv") %>%
  filter(Item == "FCT-02") %>% 
  mutate(Group = "HS5")

MLS5 <- read_csv("./CSV Files/MLS-5/MLS-5 Subjunctive FCT.csv") %>%
  filter(Item == "FCT-02") %>% 
  mutate(Group = "HS5")


## Join CSVs
Master <- rbind(SDB, HSA, DLI78, MLS78, DLI5, MLS5)
Adults <- rbind(SDB, HSA)


# Generate statistics
BESA <- Master %>% 
  group_by(Group) %>% 
  summarize(Mean = mean(BESA_Other), SD = sd(BESA_Other))

DELE <- Adults %>%
  group_by(Group) %>% 
  summarize(Mean = mean(DELE), SD = sd(DELE))

Use <- Master %>% 
  group_by(Group) %>% 
  summarize(Mean = mean(Use_Joint), SD = sd(Use_Joint))

Parents <- Master %>% 
  group_by(Group) %>% 
  summarize(Mean = mean(Exposure_Score), SD = sd(Exposure_Score))
