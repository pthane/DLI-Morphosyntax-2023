library(brms)
library(tidyverse)
library(emmeans)


library(tidyverse)
library(lme4)
library(lmerTest)
library(sjPlot)
library(emmeans)

options(scipen = 99)


# Load data
## EPT
SDB_EPT <- read_csv("./CSV Files/SDB/SDB Subjunctive EPT.csv") %>%
  filter(Property == "Intensional subjunctive")

HSA_EPT <- read_csv("./CSV Files/Adult HS/Adult HS Subjunctive EPT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HSA")

DLI78_EPT <- read_csv("./CSV Files/DLI-78/DLI-78 Subjunctive EPT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS7/8")

MLS78_EPT <- read_csv("./CSV Files/MLS-78/MLS-78 Subjunctive EPT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS7/8")

DLI5_EPT <- read_csv("./CSV Files/DLI-5/DLI-5 Subjunctive EPT.csv")  %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS5")

MLS5_EPT <- read_csv("./CSV Files/MLS-5/MLS-5 Subjunctive EPT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS5") 


## FCT
SDB_FCT <- read_csv("./CSV Files/SDB/SDB Subjunctive FCT.csv") %>%
  filter(Property == "Intensional subjunctive")

HSA_FCT <- read_csv("./CSV Files/Adult HS/Adult HS Subjunctive FCT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HSA")

DLI78_FCT <- read_csv("./CSV Files/DLI-78/DLI-78 Subjunctive FCT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS7/8")

MLS78_FCT <- read_csv("./CSV Files/MLS-78/MLS-78 Subjunctive FCT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS7/8")

DLI5_FCT <- read_csv("./CSV Files/DLI-5/DLI-5 Subjunctive FCT.csv")  %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS5")

MLS5_FCT <- read_csv("./CSV Files/MLS-5/MLS-5 Subjunctive FCT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS5")


## Rejoin datasets
EPT <- rbind(SDB_EPT, HSA_EPT, DLI78_EPT, MLS78_EPT, DLI5_EPT, MLS5_EPT)
FCT <- rbind(SDB_FCT, HSA_FCT, DLI78_FCT, MLS78_FCT, DLI5_FCT, MLS5_FCT)

Aggregate <- rbind(EPT, FCT)
Aggregate$Group <- factor(Aggregate$Group, levels = c("SDB", "HSA", "HS7/8", "HS5"))


# Run Bayesian model on all data
Subjunctive_Bayesian <- brm(Mood_Use ~ 1 + Group + Task + BESA_Other_Std + Use_Joint_Std + Group:Task +
                       (1 | Part_ID) + (1 | Item),
                     data = Aggregate,
                     family = "binomial")

summary(Subjunctive_Bayesian)
conditional_effects(Subjunctive_Bayesian)
confint(Subjunctive_Bayesian)

