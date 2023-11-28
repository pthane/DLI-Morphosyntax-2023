library(tidyverse)
library(lme4)
library(lmerTest)

options(scipen = 99)


# Load DOM data
## EPT
HSA_EPT <- read.csv("./CSV Files/Adult HS/Adult HS DOM EPT.csv") %>% 
  mutate(Group = "HSA",
         Task = "Production",
         School = "English")

DLI78_EPT <- read.csv("./CSV Files/DLI-78/DLI-78 DOM EPT.csv") %>% 
  mutate(Group = "HS7/8",
         Task = "Production",
         School = "Bilingual")

MLS78_EPT <- read.csv("./CSV Files/MLS-78/MLS-78 DOM EPT.csv") %>% 
  filter(!School == "GBCS") %>%
  mutate(Group = "HS7/8",
         Task = "Production",
         School = "English")

DLI5_EPT <- read.csv("./CSV Files/MLS-5/MLS-5 DOM EPT.csv") %>% 
  mutate(Group = "HS5",
         Task = "Production",
         School = "Bilingual")

MLS5_EPT <- read.csv("./CSV Files/MLS-5/MLS-5 DOM EPT.csv") %>% 
  filter(!School == "GBCS") %>% 
  mutate(Group = "HS5",
         Task = "Production",
         School = "English")


## FCT
HSA_FCT <- read.csv("./CSV Files/Adult HS/Adult HS DOM FCT.csv") %>% 
  mutate(Group = "HSA",
         Task = "Forced Choice",
         School = "English")

DLI78_FCT <- read.csv("./CSV Files/DLI-78/DLI-78 DOM FCT.csv") %>% 
  mutate(Group = "HS7/8",
         Task = "Forced Choice",
         School = "Bilingual")

MLS78_FCT <- read.csv("./CSV Files/MLS-78/MLS-78 DOM FCT.csv") %>% 
  filter(!School == "GBCS") %>%
  mutate(Group = "HS7/8",
         Task = "Forced Choice",
         School = "English")

DLI5_FCT <- read.csv("./CSV Files/MLS-5/MLS-5 DOM FCT.csv") %>% 
  mutate(Group = "HS5",
         Task = "Forced Choice",
         School = "Bilingual")

MLS5_FCT <- read.csv("./CSV Files/MLS-5/MLS-5 DOM FCT.csv") %>% 
  filter(!School == "GBCS") %>% 
  mutate(Group = "HS5",
         Task = "Forced Choice",
         School = "English")


## Join datasets
EPT <- rbind(HSA_EPT, DLI78_EPT, MLS78_EPT, DLI5_EPT, MLS78_EPT)
FCT <- rbind(HSA_FCT, DLI78_FCT, MLS78_FCT, DLI5_FCT, MLS78_FCT)

Master <- rbind(EPT, FCT)
Master$Task <- factor(Master$Task, levels = c("Production", "Forced Choice"))
Master$Group <- factor(Master$Group, levels = c("HSA", "HS7/8", "HS5"))


# Generate correlation
DOM_Correlation <- glmer(DOM_Use ~ Group + Use_Joint_Std + School + Task + Use_Joint_Std:Task +
                           (1 | Part_ID) + (1 | Item),
                         data = Master,
                         family = "binomial")

summary(DOM_Correlation)
