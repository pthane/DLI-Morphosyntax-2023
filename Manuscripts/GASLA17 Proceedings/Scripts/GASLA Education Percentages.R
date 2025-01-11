library(tidyverse)

# Prepare subjunctive data
## Load production
DLI78_Subjunctive_EPT <- read_csv(here("./CSV Files/DLI-78/DLI-78 Subjunctive EPT.csv")) %>% 
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Child_Group = "DLI-7/8",
         Group = "HS7/8",
         Task = "Production",
         School_Group = "Immersion",
         Age = "7th/8th")

MLS78_Subjunctive_EPT <- read_csv(here("./CSV Files/MLS-78/MLS-78 Subjunctive EPT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Child_Group = "MLS-7/8",
         Group = "HS7/8",
         Task = "Production",
         School_Group = "Monolingual",
         Age = "7th/8th")

DLI5_Subjunctive_EPT <- read_csv(here("./CSV Files/DLI-5/DLI-5 Subjunctive EPT.csv")) %>% 
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Child_Group = "DLI-5",
         Group = "HS5",
         Task = "Production",
         School_Group = "Immersion",
         Age = "5th")

MLS5_Subjunctive_EPT <- read_csv(here("./CSV Files/MLS-5/MLS-5 Subjunctive EPT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Child_Group = "MLS-5",
         Group = "HS5",
         Task = "Production",
         School_Group = "Monolingual",
         Age = "5th")


## Load selection
DLI78_Subjunctive_FCT <- read_csv(here("./CSV Files/DLI-78/DLI-78 Subjunctive FCT.csv")) %>% 
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Child_Group = "DLI-7/8",
         Group = "HS7/8",
         Task = "Selection",
         School_Group = "Immersion",
         Age = "7th/8th")

MLS78_Subjunctive_FCT <- read_csv(here("./CSV Files/MLS-78/MLS-78 Subjunctive FCT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Child_Group = "MLS-7/8",
         Group = "HS7/8",
         Task = "Selection",
         School_Group = "Monolingual",
         Age = "7th/8th")

DLI5_Subjunctive_FCT <- read_csv(here("./CSV Files/DLI-5/DLI-5 Subjunctive FCT.csv")) %>% 
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Child_Group = "DLI-5",
         Group = "HS5",
         Task = "Selection",
         School_Group = "Immersion",
         Age = "5th")

MLS5_Subjunctive_FCT <- read_csv(here("./CSV Files/MLS-5/MLS-5 Subjunctive FCT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Child_Group = "MLS-5",
         Group = "HS5",
         Task = "Selection",
         School_Group = "Monolingual",
         Age = "5th")


## Join datasets
Subjunctive_EPT <- rbind(DLI78_Subjunctive_EPT, MLS78_Subjunctive_EPT, DLI5_Subjunctive_EPT, MLS5_Subjunctive_EPT)
Subjunctive_FCT <- rbind(DLI78_Subjunctive_FCT, MLS78_Subjunctive_FCT, DLI5_Subjunctive_FCT, MLS5_Subjunctive_FCT)

Subjunctive_Aggregate <- rbind(Subjunctive_EPT, Subjunctive_FCT) %>% 
  filter(!is.na(Mood_Use)) %>% 
  mutate(Accuracy = Mood_Use,
         Structure = "Subjunctive")


# DOM data
## Load production
DLI78_DOM_EPT <- read_csv(here("./CSV Files/DLI-78/DLI-78 DOM EPT.csv")) %>% 
  mutate(Child_Group = "DLI-7/8",
         Group = "HS7/8",
         Task = "Production",
         School_Group = "Immersion",
         Age = "7th/8th")

MLS78_DOM_EPT <- read_csv(here("./CSV Files/MLS-78/MLS-78 DOM EPT.csv")) %>%
  mutate(Child_Group = "MLS-7/8",
         Group = "HS7/8",
         Task = "Production",
         School_Group = "Monolingual",
         Age = "7th/8th")

DLI5_DOM_EPT <- read_csv(here("./CSV Files/DLI-5/DLI-5 DOM EPT.csv")) %>% 
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>% 
  mutate(Child_Group = "DLI-5",
         Group = "HS5",
         Task = "Production",
         School_Group = "Immersion",
         Age = "5th")

MLS5_DOM_EPT <- read_csv(here("./CSV Files/MLS-5/MLS-5 DOM EPT.csv")) %>%
  mutate(Child_Group = "MLS-5",
         Group = "HS5",
         Task = "Production",
         School_Group = "Monolingual",
         Age = "5th")


# DOM data
## Load production
DLI78_DOM_EPT <- read_csv(here("./CSV Files/DLI-78/DLI-78 DOM EPT.csv")) %>% 
  mutate(Child_Group = "DLI-7/8",
         Group = "HS7/8",
         Task = "Production",
         School_Group = "Immersion",
         Age = "7th/8th")

MLS78_DOM_EPT <- read_csv(here("./CSV Files/MLS-78/MLS-78 DOM EPT.csv")) %>%
  mutate(Child_Group = "MLS-7/8",
         Group = "HS7/8",
         Task = "Production",
         School_Group = "Monolingual",
         Age = "7th/8th")

DLI5_DOM_EPT <- read_csv(here("./CSV Files/DLI-5/DLI-5 DOM EPT.csv")) %>% 
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>% 
  mutate(Child_Group = "DLI-5",
         Group = "HS5",
         Task = "Production",
         School_Group = "Immersion",
         Age = "5th")

MLS5_DOM_EPT <- read_csv(here("./CSV Files/MLS-5/MLS-5 DOM EPT.csv")) %>%
  mutate(Child_Group = "MLS-5",
         Group = "HS5",
         Task = "Production",
         School_Group = "Monolingual",
         Age = "5th")


## Load selection
DLI78_DOM_FCT <- read_csv(here("./CSV Files/DLI-78/DLI-78 DOM FCT.csv")) %>% 
  mutate(Child_Group = "DLI-7/8",
         Group = "HS7/8",
         Task = "Selection",
         School_Group = "Immersion",
         Age = "7th/8th")

MLS78_DOM_FCT <- read_csv(here("./CSV Files/MLS-78/MLS-78 DOM FCT.csv")) %>%
  mutate(Child_Group = "MLS-7/8",
         Group = "HS7/8",
         Task = "Selection",
         School_Group = "Monolingual",
         Age = "7th/8th")

DLI5_DOM_FCT <- read_csv(here("./CSV Files/DLI-5/DLI-5 DOM FCT.csv")) %>% 
  filter(Item %in% c("FCT-02", "FCT-08", "FCT-12", "FCT-14", "FCT-20", "FCT-24", "FCT-26", "FCT-32", "FCT-38", "FCT-42")) %>% 
  mutate(Child_Group = "DLI-5",
         Group = "HS5",
         Task = "Selection",
         School_Group = "Immersion",
         Age = "5th")

MLS5_DOM_FCT <- read_csv(here("./CSV Files/MLS-5/MLS-5 DOM FCT.csv")) %>%
  mutate(Child_Group = "MLS-5",
         Group = "HS5",
         Task = "Selection",
         School_Group = "Monolingual",
         Age = "5th")


## Join datasets
DOM_EPT <- rbind(DLI78_DOM_EPT, MLS78_DOM_EPT, DLI5_DOM_EPT, MLS5_DOM_EPT)
DOM_FCT <- rbind(DLI78_DOM_FCT, MLS78_DOM_FCT, DLI5_DOM_FCT, MLS5_DOM_FCT)

DOM_Aggregate <- rbind(DOM_EPT, DOM_FCT) %>% 
  filter(!is.na(DOM_Use)) %>% 
  select(!Mood) %>% 
  mutate(Accuracy = DOM_Use,
         Structure = "DOM")


## Merge
Joint <- rbind(Subjunctive_Aggregate, DOM_Aggregate)
Joint$Child_Group <- factor(Joint$Child_Group, levels = c("MLS-5", "DLI-5", "MLS-7/8", "DLI-7/8"))


# Generate statistics
Descriptives <- Joint %>% 
  group_by(Child_Group, Task, Structure) %>% 
  summarize(Mean = mean(Accuracy), SD = sd(Accuracy))
