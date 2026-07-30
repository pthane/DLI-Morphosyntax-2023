library(tidyverse)
library(here)
library(patchwork)


# Load DOM data
## Production
DLI78_EPT_DOM <- read_csv(here("./CSV Files/DLI-78/DLI-78 DOM EPT.csv")) %>% 
  mutate(Age = "7/8",
         Group = "DLBE-7/8",
         Task = "Production",
         Response = DOM_Use)

MLS78_EPT_DOM <- read_csv(here("./CSV Files/MLS-78/MLS-78 DOM EPT.csv")) %>%
  filter(!School == "GBCS") %>% 
  mutate(Age = "7/8",
         Group = "MLS-7/8",
         Task = "Production",
         Response = DOM_Use)

DLI5_EPT_DOM <- read_csv(here("./CSV Files/DLI-5/DLI-5 DOM EPT.csv")) %>% 
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>% 
  mutate(Age = "5",
         Group = "DLBE-5",
         Task = "Production",
         Response = DOM_Use)

MLS5_EPT_DOM <- read_csv(here("./CSV Files/MLS-5/MLS-5 DOM EPT.csv")) %>%
  filter(!School == "GBCS") %>% 
  mutate(Age = "5",
         Group = "MLS-5",
         Task = "Production",
         Response = DOM_Use)

HSA_EPT_DOM <- read_csv(here("./CSV Files/Adult HS/Adult HS DOM EPT.csv")) %>%
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>% 
  mutate(Age = "Adults",
         Group = "Adults",
         Task = "Production",
         Response = DOM_Use)


## Selection
DLI78_FCT_DOM <- read_csv(here("./CSV Files/DLI-78/DLI-78 DOM FCT.csv")) %>% 
  mutate(Age = "7/8",
         Group = "DLBE-7/8",
         Task = "Selection",
         Response = DOM_Use)

MLS78_FCT_DOM <- read_csv(here("./CSV Files/MLS-78/MLS-78 DOM FCT.csv")) %>%
  filter(!School == "GBCS") %>% 
  mutate(Age = "7/8",
         Group = "MLS-7/8",
         Task = "Selection",
         Response = DOM_Use)

DLI5_FCT_DOM <- read_csv(here("./CSV Files/DLI-5/DLI-5 DOM FCT.csv")) %>% 
  mutate(Age = "5",
         Group = "DLBE-5",
         Task = "Selection",
         Response = DOM_Use)

MLS5_FCT_DOM <- read_csv(here("./CSV Files/MLS-5/MLS-5 DOM FCT.csv")) %>%
  filter(!School == "GBCS") %>% 
  mutate(Age = "5",
         Group = "MLS-5",
         Task = "Selection",
         Response = DOM_Use)

HSA_FCT_DOM <- read_csv(here("./CSV Files/Adult HS/Adult HS DOM FCT.csv")) %>%
  mutate(Group = "Adults",
         Task = "Selection",
         Response = DOM_Use)


## Save files
Production_DOM <- rbind(DLI78_EPT_DOM, MLS78_EPT_DOM, DLI5_EPT_DOM, MLS5_EPT_DOM, HSA_EPT_DOM) %>% 
  write_csv(here("Manuscripts", "SLR (Multiple Baselines)", "Data", "SLR Multiple Baselines DOM Production.csv"))

Selection_DOM <- rbind(DLI78_FCT_DOM, MLS78_FCT_DOM, DLI5_FCT_DOM, MLS5_FCT_DOM, HSA_FCT_DOM) %>% 
  write_csv(here("Manuscripts", "SLR (Multiple Baselines)", "Data", "SLR Multiple Baselines DOM Selection.csv"))