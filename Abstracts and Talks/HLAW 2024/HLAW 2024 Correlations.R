library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)

options(scipen = 99)


# Load DOM data
## EPT
SDB_DOM_EPT <- read_csv("./CSV Files/SDB/SDB DOM EPT.csv") %>% 
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>% 
  mutate(Group = "Adults",
         Task = "Production",
         School = "Adults",
         Age = "Adults")

DLI78_DOM_EPT <- read.csv("./CSV Files/DLI-78/DLI-78 DOM EPT.csv") %>% 
  mutate(Group = "Immersion-7/8",
         Task = "Production",
         School = "DLI",
         Age = "7th/8th")

MLS78_DOM_EPT <- read.csv("./CSV Files/MLS-78/MLS-78 DOM EPT.csv") %>% 
  filter(!School == "GBCS") %>%
  mutate(Group = "Monolingual-7/8",
         Task = "Production",
         School = "MLS",
         Age = "7th/8th")

DLI5_DOM_EPT <- read.csv("./CSV Files/MLS-5/MLS-5 DOM EPT.csv") %>% 
  mutate(Group = "Immersion-5",
         Task = "Production",
         School = "DLI",
         Age = "5th")

MLS5_DOM_EPT <- read.csv("./CSV Files/MLS-5/MLS-5 DOM EPT.csv") %>% 
  filter(!School == "GBCS") %>% 
  mutate(Group = "Monolingual-5",
         Task = "Production",
         School = "MLS",
         Age = "5th")


## FCT
SDB_DOM_FCT <- read_csv("./CSV Files/SDB/SDB DOM FCT.csv") %>% 
  mutate(Group = "Adults",
         Task = "Selection",
         School = "Adults",
         Age = "Adults")

DLI78_DOM_FCT <- read.csv("./CSV Files/DLI-78/DLI-78 DOM FCT.csv") %>% 
  mutate(Group = "Immersion-7/8",
         Task = "Selection",
         School = "DLI",
         Age = "7th/8th")

MLS78_DOM_FCT <- read.csv("./CSV Files/MLS-78/MLS-78 DOM FCT.csv") %>% 
  filter(!School == "GBCS") %>%
  mutate(Group = "Monolingual-7/8",
         Task = "Selection",
         School = "MLS",
         Age = "7th/8th")

DLI5_DOM_FCT <- read.csv("./CSV Files/MLS-5/MLS-5 DOM FCT.csv") %>% 
  mutate(Group = "Immersion-5",
         Task = "Selection",
         School = "DLI",
         Age = "5th")

MLS5_DOM_FCT <- read.csv("./CSV Files/MLS-5/MLS-5 DOM FCT.csv") %>% 
  filter(!School == "GBCS") %>% 
  mutate(Group = "Monolingual-5",
         Task = "Selection",
         School = "MLS",
         Age = "5th")


## Join datasets
DOM_EPT <- rbind(SDB_DOM_EPT, DLI78_DOM_EPT, MLS78_DOM_EPT, DLI5_DOM_EPT, MLS5_DOM_EPT)
DOM_FCT <- rbind(SDB_DOM_FCT, DLI78_DOM_FCT, MLS78_DOM_FCT, DLI5_DOM_FCT, MLS5_DOM_FCT)

DOM_Master <- rbind(DOM_EPT, DOM_FCT)
DOM_Master$Group <- factor(DOM_Master$Group, levels = c("Adults", "Immersion-7/8", "Monolingual-7/8", "Immersion-5", "Monolingual-5"))

DOM_Master_HS <- DOM_Master %>% 
  filter(!Group == "Adults")


# Load subjunctive data
SDB_Subj_EPT <- read_csv("./CSV Files/SDB/SDB Subjunctive EPT.csv") %>%
  filter(Short_Prop == "Intensional") %>% 
  mutate(Group = "Adults",
         Task = "Production",
         Mood = "Subjunctive",
         School = "Adults",
         Age = "Adults")

DLI78_Subj_EPT <- read.csv("./CSV Files/DLI-78/DLI-78 Subjunctive EPT.csv") %>% 
  filter(Short_Prop == "Intensional") %>% 
  mutate(Group = "Immersion-7/8",
         Task = "Production",
         Mood = "Subjunctive",
         School = "DLI",
         Age = "7th/8th")

MLS78_Subj_EPT <- read.csv("./CSV Files/MLS-78/MLS-78 Subjunctive EPT.csv") %>% 
  filter(!School == "GBCS") %>% 
  filter(Short_Prop == "Intensional") %>% 
  mutate(Group = "Monolingual-7/8",
         Task = "Production",
         Mood = "Subjunctive",
         School = "MLS",
         Age = "7th/8th")

DLI5_Subj_EPT <- read.csv("./CSV Files/MLS-5/MLS-5 Subjunctive EPT.csv") %>% 
  filter(Short_Prop == "Intensional") %>% 
  mutate(Group = "Immersion-5",
         Task = "Production",
         Mood = "Subjunctive",
         School = "DLI",
         Age = "5th")

MLS5_Subj_EPT <- read.csv("./CSV Files/MLS-5/MLS-5 Subjunctive EPT.csv") %>% 
  filter(!School == "GBCS") %>% 
  filter(Short_Prop == "Intensional") %>% 
  mutate(Group = "Monolingual-5",
         Task = "Production",
         Mood = "Subjunctive",
         School = "MLS",
         Age = "5th")


# FCT
SDB_Subj_FCT <- read_csv("./CSV Files/SDB/SDB Subjunctive FCT.csv") %>%
  filter(Short_Prop == "Intensional") %>% 
  mutate(Group = "Adults",
         Task = "Selection",
         Mood = "Subjunctive",
         School = "Adults",
         Age = "Adults")

DLI78_Subj_FCT <- read.csv("./CSV Files/DLI-78/DLI-78 Subjunctive FCT.csv") %>% 
  filter(Short_Prop == "Intensional") %>% 
  mutate(Group = "Immersion-7/8",
         Task = "Selection",
         Mood = "Subjunctive",
         School = "DLI",
         Age = "7th/8th")

MLS78_Subj_FCT <- read.csv("./CSV Files/MLS-78/MLS-78 Subjunctive FCT.csv") %>% 
  filter(!School == "GBCS") %>% 
  filter(Short_Prop == "Intensional") %>% 
  mutate(Group = "Monolingual-7/8",
         Task = "Selection",
         Mood = "Subjunctive",
         School = "MLS",
         Age = "7th/8th")

DLI5_Subj_FCT <- read.csv("./CSV Files/MLS-5/MLS-5 Subjunctive FCT.csv") %>% 
  filter(Short_Prop == "Intensional") %>% 
  mutate(Group = "Immersion-5",
         Task = "Selection",
         Mood = "Subjunctive",
         School = "DLI",
         Age = "5th")

MLS5_Subj_FCT <- read.csv("./CSV Files/MLS-5/MLS-5 Subjunctive FCT.csv") %>% 
  filter(!School == "GBCS") %>% 
  filter(Short_Prop == "Intensional") %>% 
  mutate(Group = "Monolingual-5",
         Task = "Selection",
         Mood = "Subjunctive",
         School = "MLS",
         Age = "5th")


## Join datasets
Subj_EPT <- rbind(SDB_Subj_EPT, DLI78_Subj_EPT, MLS78_Subj_EPT, DLI5_Subj_EPT, MLS5_Subj_EPT)
Subj_FCT <- rbind(SDB_Subj_FCT, DLI78_Subj_FCT, MLS78_Subj_FCT, DLI5_Subj_FCT, MLS5_Subj_FCT)

Subj_Master <- rbind(Subj_EPT, Subj_FCT)
Subj_Master$Group <- factor(Subj_Master$Group, levels = c("Adults", "Immersion-7/8", "Monolingual-7/8", "Immersion-5", "Monolingual-5"))

Subj_Master_HS <- Subj_Master %>% 
  filter(!Group == "Adults")



# DOM correlation
## General model
DOM_Correlation <- glmer(DOM_Use ~ School + Age + Task + School:Age +
                           (1 | Part_ID) + (1 | Item),
                         data = DOM_Master_HS,
                         family = "binomial")

summary(DOM_Correlation)


# Subjunctive correlation
## General model
Subj_Correlation <- glmer(Mood_Use ~ School + Age + Task + School:Age +
                            (1 | Part_ID) + (1 | Item),
                          data = Subj_Master_HS,
                          family = "binomial")

summary(Subj_Correlation)