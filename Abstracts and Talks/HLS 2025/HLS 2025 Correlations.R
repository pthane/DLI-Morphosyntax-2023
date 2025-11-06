library(tidyverse)
library(here)
library(lme4)
library(lmerTest)
library(emmeans)


options(scipen = 99)


# Load data
## Production
DLI78_EPT <- read_csv(here("./CSV Files/DLI-78/DLI-78 DOM EPT.csv")) %>% 
  mutate(Group = "DLI-7/8",
         Task = "Production",
         School = "Immersion",
         Age = "7th/8th")

MLS78_EPT <- read_csv(here("./CSV Files/MLS-78/MLS-78 DOM EPT.csv")) %>%
  filter(!School == "GBCS") %>% 
  mutate(Group = "MLS-7/8",
         Task = "Production",
         School = "Monolingual",
         Age = "7th/8th")

DLI5_EPT <- read_csv(here("./CSV Files/DLI-5/DLI-5 DOM EPT.csv")) %>% 
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>% 
  mutate(Group = "DLI-5",
         Task = "Production",
         School = "Immersion",
         Age = "5th Grade")

MLS5_EPT <- read_csv(here("./CSV Files/MLS-5/MLS-5 DOM EPT.csv")) %>%
  filter(!School == "GBCS") %>% 
  mutate(Group = "MLS-5",
         Task = "Production",
         School = "Monolingual",
         Age = "5th Grade")

HSA_EPT <- read_csv(here("./CSV Files/Adult HS/Adult HS DOM EPT.csv")) %>%
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>%
  mutate(Group = "Adults",
         Task = "Production",
         School = "Adults",
         Age = "Adults")


## Selection
DLI78_FCT <- read_csv(here("./CSV Files/DLI-78/DLI-78 DOM FCT.csv")) %>% 
  mutate(Group = "DLI-7/8",
         Task = "Selection",
         School = "Immersion",
         Age = "7th/8th")

MLS78_FCT <- read_csv(here("./CSV Files/MLS-78/MLS-78 DOM FCT.csv")) %>%
  filter(!School == "GBCS") %>% 
  mutate(Group = "MLS-7/8",
         Task = "Selection",
         School = "Monolingual",
         Age = "7th/8th")

DLI5_FCT <- read_csv(here("./CSV Files/DLI-5/DLI-5 DOM FCT.csv")) %>% 
  mutate(Group = "DLI-5",
         Task = "Selection",
         School = "Immersion",
         Age = "5th Grade")

MLS5_FCT <- read_csv(here("./CSV Files/MLS-5/MLS-5 DOM FCT.csv")) %>%
  filter(!School == "GBCS") %>% 
  mutate(Group = "MLS-5",
         Task = "Selection",
         School = "Monolingual",
         Age = "5th Grade")

HSA_FCT <- read_csv(here("./CSV Files/Adult HS/Adult HS DOM FCT.csv")) %>%
  mutate(Group = "Adults",
         Task = "Selection",
         School = "Adults",
         Age = "Adults")


## Join and tidy data
## Task-specific
Production <- rbind(DLI78_EPT, MLS78_EPT, DLI5_EPT, MLS5_EPT, HSA_EPT)
Production$Group <- factor(Production$Group, levels = c("Adults", "DLI-7/8", "MLS-7/8", "DLI-5", "MLS-5"))


Selection <- rbind(DLI78_FCT, MLS78_FCT, DLI5_FCT, MLS5_FCT, HSA_FCT)
Selection$Group <- factor(Selection$Group, levels = c("Adults", "DLI-7/8", "MLS-7/8", "DLI-5", "MLS-5"))


## Master with all
Master <- rbind(Production, Selection)
Master$Group <- factor(Master$Group, levels = c("Adults", "DLI-7/8", "MLS-7/8", "DLI-5", "MLS-5"))


## Children only
Master_Children <- Master %>% 
  filter(!Group == "Adults")


# Conduct correlations by group
## GLMM
Correlation_Group <- glmer(DOM_Use ~ Group +
                       (1 | Part_ID) + (1 | Item),
                     family = "binomial",
                     data = Master)

summary(Correlation_Group)


## Post-hoc comparisons
Pairwise_Group <- emmeans(Correlation_Group, spec = "Group")
Pairwise_Tukey <- contrast(Pairwise_Group, method = "pairwise")

summary(Pairwise_Tukey)


# Conduct correlations by schooling
Correlation_Schooling <- glmer(DOM_Use ~ School + Age + Task + School:Task +
                                 (1 | Part_ID) + (1 | Item),
                     family = "binomial",
                     data = Master_Children)

summary(Correlation_Schooling)
