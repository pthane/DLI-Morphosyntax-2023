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
  filter(Property == "Intensional subjunctive")

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
  filter(Property == "Intensional subjunctive")

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


# Rejoin data
## Join age group datasets
EPT <- rbind(SDB_EPT, HSA_EPT, DLI78_EPT, MLS78_EPT, DLI5_EPT, MLS5_EPT)
FCT <- rbind(SDB_FCT, HSA_FCT, DLI78_FCT, MLS78_FCT, DLI5_FCT, MLS5_FCT)

Aggregate <- rbind(EPT, FCT)
Aggregate$Group <- factor(Aggregate$Group, levels = c("SDB", "HSA", "HS7/8", "HS5"))

## Create HS datasets
Aggregate_HS <- Aggregate %>% 
  filter(!Group == "SDB")
Aggregate_HS$Group <- factor(Aggregate_HS$Group, levels = c("HSA", "HS7/8", "HS5"))


# Omnibus correlation
## General model
Subjunctive_Omnibus <- glmer(Mood_Use ~ 1 + Group +
                      (1 | Part_ID) + (1 | Item),
                    data = Aggregate,
                    family = "binomial")

summary(Subjunctive_Omnibus)


## Tukey post-hoc comparisons
Subjunctive_Pairwise <- emmeans(Subjunctive_Omnibus, spec = "Group")
Subjunctive_Tukey <- contrast(Subjunctive_Pairwise, method = "pairwise")

summary(Subjunctive_Tukey)
confint(Subjunctive_Tukey)


## Plot model
plot_model(Subjunctive_Omnibus, show.values = TRUE, show.intercept = TRUE, value.offset = .3, transform = NULL, vline.color = "black") +
  scale_y_continuous(breaks = seq(-7.5, 7.5, 2.5),
                     limits = c(-8, 8)) +
  labs(title = "Summary of Generalized Linear Mixed Methods Model", y = "Beta estimates") +
  scale_x_discrete(labels = c("HS5 Group", "HS7/8 Group", "HSA Group", "(Intercept)")) +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"))


# Overall HS correlation
Subjunctive_HS <- glmer(Mood_Use ~ Task + BESA_Other_Std + Use_Joint_Std + Task:BESA_Other_Std + Task:Use_Joint_Std +
                          (1 | Part_ID) + (1 | Item),
                        data = Aggregate_HS,
                        family = "binomial")

summary(Subjunctive_HS)


## Plot model
plot_model(Subjunctive_HS, show.values = TRUE, show.intercept = TRUE, value.offset = .3, transform = NULL, vline.color = "black") +
  scale_y_continuous(breaks = seq(-1.5, 1.5, 0.5),
                     limits = c(-1.5, 1.5)) +
  scale_x_discrete(labels = c("Task : Freq. of use", "Task : BESA", "Frequency of use", "BESA proficiency", "FCT", "(Intercept)")) +
  labs(title = "Summary of Generalized Linear Mixed Methods Model", y = "Beta estimates") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"))
