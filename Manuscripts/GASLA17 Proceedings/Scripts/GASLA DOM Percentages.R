library(tidyverse)
library(here)


# Load data
## EPT
SDB_EPT <- read_csv(here("./CSV Files/SDB/SDB DOM EPT.csv")) %>%
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>% 
  mutate(Group = "SDBA")

HSA_EPT <- read_csv(here("./CSV Files/Adult HS/Adult HS DOM EPT.csv")) %>%
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>% 
  mutate(Group = "HSA")

DLI78_EPT <- read_csv(here("./CSV Files/DLI-78/DLI-78 DOM EPT.csv")) %>%
  mutate(Group = "HS7/8")

MLS78_EPT <- read_csv(here("./CSV Files/MLS-78/MLS-78 DOM EPT.csv")) %>%
  mutate(Group = "HS7/8")

DLI5_EPT <- read_csv(here("./CSV Files/DLI-5/DLI-5 DOM EPT.csv"))  %>%
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>% 
  mutate(Group = "HS5")

MLS5_EPT <- read_csv(here("./CSV Files/MLS-5/MLS-5 DOM EPT.csv")) %>%
  mutate(Group = "HS5")


## FCT
SDB_FCT <- read_csv(here("./CSV Files/SDB/SDB DOM FCT.csv")) %>%
  filter(Item %in% c("FCT-02", "FCT-08", "FCT-12", "FCT-14", "FCT-20", "FCT-24", "FCT-26", "FCT-32", "FCT-38", "FCT-42")) %>% 
  mutate(Group = "SDBA")

HSA_FCT <- read_csv(here("./CSV Files/Adult HS/Adult HS DOM FCT.csv")) %>%
  filter(Item %in% c("FCT-02", "FCT-08", "FCT-12", "FCT-14", "FCT-20", "FCT-24", "FCT-26", "FCT-32", "FCT-38", "FCT-42")) %>% 
  mutate(Group = "HSA")

DLI78_FCT <- read_csv(here("./CSV Files/DLI-78/DLI-78 DOM FCT.csv")) %>%
  mutate(Group = "HS7/8")

MLS78_FCT <- read_csv(here("./CSV Files/MLS-78/MLS-78 DOM FCT.csv")) %>%
  mutate(Group = "HS7/8")

DLI5_FCT <- read_csv(here("./CSV Files/DLI-5/DLI-5 DOM FCT.csv"))  %>%
  filter(Item %in% c("FCT-02", "FCT-08", "FCT-12", "FCT-14", "FCT-20", "FCT-24", "FCT-26", "FCT-32", "FCT-38", "FCT-42")) %>% 
  mutate(Group = "HS5")

MLS5_FCT <- read_csv(here("./CSV Files/MLS-5/MLS-5 DOM FCT.csv")) %>%
  mutate(Group = "HS5")


## Merge
EPT <- rbind(SDB_EPT, HSA_EPT, DLI78_EPT, MLS78_EPT, DLI5_EPT, MLS5_EPT)
FCT <- rbind(SDB_FCT, HSA_FCT, DLI78_FCT, MLS78_FCT, DLI5_FCT, MLS5_FCT)

Joint <- rbind(EPT, FCT) %>% 
  filter(!is.na(DOM_Use))


# Generate statistics
Descriptives <- Joint %>% 
  group_by(Group, Task) %>% 
  summarize(Mean = mean(DOM_Use), SD = sd(DOM_Use))
