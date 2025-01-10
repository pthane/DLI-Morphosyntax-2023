library(tidyverse)
library(here)

# Load data
## EPT
SDB_SCT<- read_csv(here("./CSV Files/SDB/SDB DOM EPT.csv")) %>%
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>%
  mutate(Group = "SDBA",
         Task = "SCT")

HSA_SCT <- read_csv(here("./CSV Files/Adult HS/Adult HS DOM EPT.csv")) %>%
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>%
  mutate(Group = "HSA",
         Task = "SCT")

DLI78_SCT <- read_csv(here("./CSV Files/DLI-78/DLI-78 DOM EPT.csv")) %>%
  mutate(Group = "HS7/8",
         Task = "SCT")

MLS78_SCT <- read_csv(here("./CSV Files/MLS-78/MLS-78 DOM EPT.csv")) %>%
  mutate(Group = "HS7/8",
         Task = "SCT")

DLI5_SCT <- read_csv(here("./CSV Files/DLI-5/DLI-5 DOM EPT.csv"))  %>%
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>%
  mutate(Group = "HS5",
         Task = "SCT")

MLS5_SCT <- read_csv(here("./CSV Files/MLS-5/MLS-5 DOM EPT.csv")) %>%
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>%
  mutate(Group = "HS5",
         Task = "SCT")


## FCT
SDB_MST<- read_csv(here("./CSV Files/SDB/SDB DOM FCT.csv")) %>%
  mutate(Group = "SDBA",
         Task = "MST")

HSA_MST <- read_csv(here("./CSV Files/Adult HS/Adult HS DOM FCT.csv")) %>%
  mutate(Group = "HSA",
         Task = "MST")

DLI78_MST <- read_csv(here("./CSV Files/DLI-78/DLI-78 DOM FCT.csv")) %>% 
  mutate(Group = "HS7/8",
         Task = "MST")

MLS78_MST <- read_csv(here("./CSV Files/MLS-78/MLS-78 DOM FCT.csv")) %>% 
  mutate(Group = "HS7/8",
         Task = "MST")

DLI5_MST <- read_csv(here("./CSV Files/DLI-5/DLI-5 DOM FCT.csv"))  %>%
  mutate(Group = "HS5",
         Task = "MST")

MLS5_MST <- read_csv(here("./CSV Files/MLS-5/MLS-5 DOM FCT.csv")) %>%
  mutate(Group = "HS5",
         Task = "MST")

## Rejoin datasets
SCT <- rbind(SDB_SCT, HSA_SCT, DLI78_SCT, MLS78_SCT, DLI5_SCT, MLS5_SCT)
MST <- rbind(SDB_MST, HSA_MST, DLI78_MST, MLS78_MST, DLI5_MST, MLS5_MST)


# Generate production averages
SCT_Group <- SCT %>%
  filter(!is.na(DOM_Use)) %>%
  group_by(Part_ID, Group) %>%
  summarize(DOM_Production = sum(DOM_Use))

DOM_Production <- SCT_Group %>% 
  group_by(Group) %>% 
  summarize(Mean = mean(DOM_Production), SD = sd(DOM_Production))


# Generate selection averages
MST_Group <- MST %>%
  filter(!is.na(DOM_Use)) %>%
  group_by(Part_ID, Group) %>%
  summarize(DOM_Production = sum(DOM_Use))

DOM_Selection <- MST_Group %>% 
  group_by(Group) %>% 
  summarize(Mean = mean(DOM_Production), SD = sd(DOM_Production))

