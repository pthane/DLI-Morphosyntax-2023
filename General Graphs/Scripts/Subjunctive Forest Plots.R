library(tidyverse)
library(lme4)
library(lmerTest)
library(sjPlot)
library(emmeans)
library(here)

options(scipen = 99)


# Load data
## EPT
SDB_EPT <- read_csv(here("./CSV Files/SDB/SDB Subjunctive EPT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Education = "SDBA",
         Grade = "Adult",
         ChildGroup = "SDBA")

HSA_EPT <- read_csv(here("./CSV Files/Adult HS/Adult HS Subjunctive EPT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HSA",
         Education = "HSA",
         Grade = "Adult",
         ChildGroup = "HSA")

DLI78_EPT <- read_csv(here("./CSV Files/DLI-78/DLI-78 Subjunctive EPT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS7/8",
         Education = "Bilingual",
         Grade = "7th/8th",
         ChildGroup = "DLI-7/8")

MLS78_EPT <- read_csv(here("./CSV Files/MLS-78/MLS-78 Subjunctive EPT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS7/8",
         Education = "Monolingual",
         Grade = "7th/8th",
         ChildGroup = "MLS-7/8")

DLI5_EPT <- read_csv(here("./CSV Files/DLI-5/DLI-5 Subjunctive EPT.csv"))  %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS5",
         Education = "Bilingual",
         Grade = "5th",
         ChildGroup = "DLI-5")

MLS5_EPT <- read_csv(here("./CSV Files/MLS-5/MLS-5 Subjunctive EPT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS5",
         Education = "Monolingual",
         Grade = "5th",
         ChildGroup = "MLS-5")


## FCT
SDB_FCT <- read_csv(here("./CSV Files/SDB/SDB Subjunctive FCT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Education = "SDBA",
         Grade = "Adult",
         ChildGroup = "SDBA")

HSA_FCT <- read_csv(here("./CSV Files/Adult HS/Adult HS Subjunctive FCT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HSA",
         Education = "HSA",
         Grade = "Adult",
         ChildGroup = "HSA")

DLI78_FCT <- read_csv(here("./CSV Files/DLI-78/DLI-78 Subjunctive FCT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS7/8",
         Education = "Bilingual",
         Grade = "7th/8th",
         ChildGroup = "DLI-7/8")

MLS78_FCT <- read_csv(here("./CSV Files/MLS-78/MLS-78 Subjunctive FCT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS7/8",
         Education = "Monolingual",
         Grade = "7th/8th",
         ChildGroup = "MLS-7/8")

DLI5_FCT <- read_csv(here("./CSV Files/DLI-5/DLI-5 Subjunctive FCT.csv"))  %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS5",
         Education = "Bilingual",
         Grade = "5th",
         ChildGroup = "DLI-5")

MLS5_FCT <- read_csv(here("./CSV Files/MLS-5/MLS-5 Subjunctive FCT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS5",
         Education = "Monolingual",
         Grade = "5th",
         ChildGroup = "MLS-5")


# Rejoin data
## Join age group datasets
EPT <- rbind(SDB_EPT, HSA_EPT, DLI78_EPT, MLS78_EPT, DLI5_EPT, MLS5_EPT)
FCT <- rbind(SDB_FCT, HSA_FCT, DLI78_FCT, MLS78_FCT, DLI5_FCT, MLS5_FCT)

Aggregate <- rbind(EPT, FCT)
Aggregate$Group <- factor(Aggregate$Group, levels = c("SDB", "HSA", "HS7/8", "HS5"))

## Create HS datasets
Aggregate_HS <- Aggregate %>% 
  filter(!Grade == "Adult")
Aggregate_HS$Grade <- factor(Aggregate_HS$Grade, levels = c("5th", "7th/8th"))
Aggregate_HS$Education <- factor(Aggregate_HS$Education, levels = c("Monolingual", "Bilingual"))
Aggregate_HS$ChildGroup <- factor(Aggregate_HS$ChildGroup, levels = c("MLS-5", "DLI-5", "MLS-7/8", "DLI-7/8"))


# Subjunctive overall
## Model
Subjunctive_Overall <- glmer(Mood_Use ~ 1 + Group + Use_Joint_Std + Task + Use_Joint_Std:Task +
                               (1 | Part_ID) + (1 | Item),
                             data = Aggregate,
                             family = "binomial")

summary(Subjunctive_Overall)

## Plot model
plot_model(Subjunctive_Overall, show.values = TRUE, show.intercept = TRUE, value.offset = .3, transform = NULL, vline.color = "black") +
  scale_y_continuous(breaks = seq(-9, 9, 3),
                     limits = c(-9, 9)) +
  labs(title = "Overall Subjunctive Production and Selection", y = "Log odds") +
  scale_x_discrete(labels = c("Use : Selection", "Selection task", "Use of Spanish", "5th grade heritage", "7th/8th grade heritage", "Adult heritage", "(Intercept)")) +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"))


# Subjunctive education
Subjunctive_HS <- glmer(Mood_Use ~ ChildGroup +
                          (1 | Part_ID) + (1 | Item),
                        data = Aggregate_HS,
                        family = "binomial")

summary(Subjunctive_HS)


## Plot model
plot_model(Subjunctive_HS, show.values = TRUE, show.intercept = TRUE, value.offset = .3, transform = NULL, vline.color = "black") +
  scale_y_continuous(breaks = seq(-9, 9, 3),
                     limits = c(-9, 9)) +
  scale_x_discrete(labels = c("7th/8th, bilingual", "7th/8th, monolingual", "5th, bilingual", "(Intercept)")) +
  labs(title = "Subjunctive Use by Child Heritage Speakers", y = "Log odds") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"))
