library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(here)

options(scipen = 99)


# Prepare subjunctive data
## Load production
SDB_Subjunctive_EPT <- read_csv(here("./CSV Files/SDB/SDB Subjunctive EPT.csv")) %>% 
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Child_Group = "SDBA",
         Group = "SDBA",
         Task = "Production",
         School_Group = "SDBA",
         Age = "Adults")

HSA_Subjunctive_EPT <- read_csv(here("./CSV Files/Adult HS/Adult HS Subjunctive EPT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Child_Group = "HSA",
         Group = "HSA",
         Task = "Production",
         School_Group = "HSA",
         Age = "Adults")


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
SDB_Subjunctive_FCT <- read_csv(here("./CSV Files/SDB/SDB Subjunctive FCT.csv")) %>% 
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Child_Group = "SDBA",
         Group = "SDBA",
         Task = "Selection",
         School_Group = "SDBA",
         Age = "Adults")

HSA_Subjunctive_FCT <- read_csv(here("./CSV Files/Adult HS/Adult HS Subjunctive FCT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Child_Group = "HSA",
         Group = "HSA",
         Task = "Selection",
         School_Group = "HSA",
         Age = "Adults")


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
Subjunctive_EPT <- rbind(SDB_Subjunctive_EPT, HSA_Subjunctive_EPT, DLI78_Subjunctive_EPT, MLS78_Subjunctive_EPT, DLI5_Subjunctive_EPT, MLS5_Subjunctive_EPT)
Subjunctive_FCT <- rbind(SDB_Subjunctive_FCT, HSA_Subjunctive_FCT, DLI78_Subjunctive_FCT, MLS78_Subjunctive_FCT, DLI5_Subjunctive_FCT, MLS5_Subjunctive_FCT)

Subjunctive_Aggregate <- rbind(Subjunctive_EPT, Subjunctive_FCT) %>% 
  mutate(Accuracy = Mood_Use)


# DOM data
## Load production
SDB_DOM_EPT <- read_csv(here("./CSV Files/SDB/SDB DOM EPT.csv")) %>%
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>% 
  mutate(Child_Group = "SDBA",
         Group = "SDBA",
         Task = "Production",
         School_Group = "SDBA",
         Age = "Adults")

HSA_DOM_EPT <- read_csv(here("./CSV Files/Adult HS/Adult HS DOM EPT.csv")) %>%
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>% 
  mutate(Child_Group = "SDBA",
         Group = "HSA",
         Task = "Production",
         School_Group = "SDBA",
         Age = "Adults")

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
SDB_DOM_EPT <- read_csv(here("./CSV Files/SDB/SDB DOM EPT.csv")) %>%
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>% 
  mutate(Child_Group = "SDBA",
         Group = "SDBA",
         Task = "Production",
         School_Group = "SDBA",
         Age = "Adults")

HSA_DOM_EPT <- read_csv(here("./CSV Files/Adult HS/Adult HS DOM EPT.csv")) %>%
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>% 
  mutate(Child_Group = "HSA",
         Group = "HSA",
         Task = "Production",
         School_Group = "HSA",
         Age = "Adults")

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
SDB_DOM_FCT <- read_csv(here("./CSV Files/SDB/SDB DOM FCT.csv")) %>%
  filter(Item %in% c("FCT-02", "FCT-08", "FCT-12", "FCT-14", "FCT-20", "FCT-24", "FCT-26", "FCT-32", "FCT-38", "FCT-42")) %>% 
  mutate(Child_Group = "SDBA",
         Group = "SDBA",
         Task = "Selection",
         School_Group = "SDBA",
         Age = "Adults")

HSA_DOM_FCT <- read_csv(here("./CSV Files/Adult HS/Adult HS DOM FCT.csv")) %>%
  filter(Item %in% c("FCT-02", "FCT-08", "FCT-12", "FCT-14", "FCT-20", "FCT-24", "FCT-26", "FCT-32", "FCT-38", "FCT-42")) %>% 
  mutate(Child_Group = "HSA",
         Group = "HSA",
         Task = "Selection",
         School_Group = "HSA",
         Age = "Adults")

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
DOM_EPT <- rbind(SDB_DOM_EPT, HSA_DOM_EPT, DLI78_DOM_EPT, MLS78_DOM_EPT, DLI5_DOM_EPT, MLS5_DOM_EPT)
DOM_FCT <- rbind(SDB_DOM_FCT, HSA_DOM_FCT, DLI78_DOM_FCT, MLS78_DOM_FCT, DLI5_DOM_FCT, MLS5_DOM_FCT)

DOM_Aggregate <- rbind(DOM_EPT, DOM_FCT) %>% 
  select(!Mood) %>% 
  mutate(Accuracy = DOM_Use)


## Final Master
Master <- rbind(Subjunctive_Aggregate, DOM_Aggregate)
Master$Group <- factor(Master$Group, levels = c("SDBA", "HSA", "HS7/8", "HS5"))
Master$Structure <- factor(Master$Structure, levels = c("Subjunctive", "DOM"))
Master$Task <- factor(Master$Task, levels = c("Production", "Selection"))


## Generate education datasets
Master_Education_Filtered <- Master %>% 
  filter(!Child_Group == "SDBA") %>% 
  filter(!Child_Group == "HSA")

Master_Education_Bilingual <- Master_Education_Filtered %>% 
  filter(School_Group == "Immersion")

Master_Education_Monolingual <- Master_Education_Filtered %>% 
  filter(School_Group == "Monolingual") %>% 
  filter(!School == "GBCS")

Master_Education <- rbind(Master_Education_Bilingual, Master_Education_Monolingual)
Master_Education$Age <- factor(Master_Education$Age, levels = c("5th", "7th/8th"))
Master_Education$School_Group <- factor(Master_Education$School_Group, levels = c("Monolingual", "Immersion"))


# Joint correlations
## General
Joint_General <- glmer(Accuracy ~ Group + Structure + Task + Use_Joint_Std + Task:Use_Joint_Std +
                            (1 | Part_ID) + (1 | Item),
                         family = "binomial",
                         data = Master)

summary(Joint_General)


## Education
Joint_Education <- glmer(Accuracy ~ School_Group * Task * Structure +
                           (1 | Part_ID) + (1 | Item),
                         family = "binomial",
                         data = Master_Education)

summary(Joint_Education)


# Post-hoc group comparisons
Joint_Pairwise <- emmeans(Joint_General, spec = "Group")
Joint_Tukey <- contrast(Joint_Pairwise, method = "pairwise")

summary(Joint_Tukey)
