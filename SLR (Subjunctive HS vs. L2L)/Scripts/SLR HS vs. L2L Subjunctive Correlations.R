library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(sjPlot)

options(scipen = 99)


# Load and prepare data
## EPT
SDB_EPT <- read_csv("./CSV Files/SDB/SDB Subjunctive EPT.csv") %>% 
  filter(Short_Prop == "Intensional") %>% 
  mutate(Group = "SDBA",
         Task = "SCT")

HS_EPT <- read_csv("./CSV Files/Adult HS/Adult HS Subjunctive EPT.csv") %>% 
  filter(Short_Prop == "Intensional") %>% 
  mutate(Group = "HS",
         Task = "SCT")

L2L_EPT <- read_csv("./CSV Files/Adult L2L/Adult L2L Subjunctive EPT.csv") %>% 
  filter(Short_Prop == "Intensional") %>% 
  mutate(Group = "L2L",
         Task = "SCT")


## FCT
SDB_FCT <- read_csv("./CSV Files/SDB/SDB Subjunctive FCT.csv") %>% 
  filter(Short_Prop == "Intensional") %>% 
  mutate(Group = "SDBA",
         Task = "MRT")

HS_FCT <- read_csv("./CSV Files/Adult HS/Adult HS Subjunctive FCT.csv") %>% 
  filter(Short_Prop == "Intensional") %>% 
  mutate(Group = "HS",
         Task = "MRT")

L2L_FCT <- read_csv("./CSV Files/Adult L2L/Adult L2L Subjunctive FCT.csv") %>% 
  filter(Short_Prop == "Intensional") %>% 
  mutate(Group = "L2L",
         Task = "MRT")


## Join datasets
EPT <- rbind(SDB_EPT, HS_EPT, L2L_EPT)
EPT$Group <- factor(EPT$Group, levels = c("SDBA", "HS", "L2L"))

FCT <- rbind(SDB_FCT, HS_FCT, L2L_FCT)
FCT$Group <- factor(FCT$Group, levels = c("SDBA", "HS", "L2L"))

Master <- rbind(EPT, FCT)
Master$Group <- factor(Master$Group, levels = c("SDBA", "HS", "L2L"))
Master$Task <- factor(Master$Task, levels = c("SCT", "MRT"))


## Create heritage datasets
EPT_Bilingual <- EPT %>% 
  filter(Group != "SDBA")

FCT_Bilingual <- FCT %>% 
  filter(Group != "SDBA")

Master_Bilingual <- Master %>% 
  filter(Group != "SDBA")


# Omnibus correlation w/SDB
## Model
Omnibus <- glmer(Mood_Use ~ Group +
                  (1 | Part_ID) + (1 | Item),
                data = Master,
                family = binomial)

summary(Omnibus)


## Tukey post-hoc comparisons
Omnibus_Pairwise <- emmeans(Omnibus, spec = "Group")
Omnibus_Tukey <- contrast(Omnibus_Pairwise, method = "pairwise")

summary(Omnibus_Tukey)


# HS vs. L2L correlation, both tasks
HS_L2L_Both_Tasks <- glmer(Mood_Use ~ Group + Task + DELE_Std + Composite_Freq +
                             (1 | Part_ID) + (1 | Item),
                           data = Master_Bilingual,
                           family = binomial)


summary(HS_L2L_Both_Tasks)


# HS vs. L2L correlation, production
HS_L2L_Production <- glmer(Mood_Use ~ Group + DELE_Std + Composite_Freq +
                             (1 | Part_ID) + (1 | Item),
                           data = EPT_Bilingual,
                           family = binomial)

summary(HS_L2L_Production)


# HS vs. L2L correlation, selection
HS_L2L_Selection <- glmer(Mood_Use ~ Group + DELE_Std + Composite_Freq +
                             (1 | Part_ID) + (1 | Item),
                           data = FCT_Bilingual,
                           family = binomial)

summary(HS_L2L_Selection)
