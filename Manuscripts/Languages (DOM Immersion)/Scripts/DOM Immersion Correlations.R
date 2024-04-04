library(tidyverse)
library(lme4)
library(lmerTest)
library(sjPlot)
library(emmeans)

options(scipen = 99)


# Load data
## Production
SDB_EPT <- read_csv("./CSV Files/SDB/SDB DOM EPT.csv") %>% 
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>% 
  mutate(Group = "SDBA",
         Task = "SCT")

DLI78_EPT <- read_csv("./CSV Files/DLI-78/DLI-78 DOM EPT.csv") %>% 
  mutate(Group = "BES-7/8",
         Task = "SCT",
         School = "Immersion",
         Age = "7th/8th")

MLS78_EPT <- read_csv("./CSV Files/MLS-78/MLS-78 DOM EPT.csv") %>%
  filter(!School == "GBCS") %>% 
  mutate(Group = "ME-7/8",
         Task = "SCT",
         School = "Monolingual",
         Age = "7th/8th")

DLI5_EPT <- read_csv("./CSV Files/DLI-5/DLI-5 DOM EPT.csv") %>% 
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>% 
  mutate(Group = "BES-5",
         Task = "SCT",
         School = "Immersion",
         Age = "5th")

MLS5_EPT <- read_csv("./CSV Files/MLS-5/MLS-5 DOM EPT.csv") %>%
  filter(!School == "GBCS") %>% 
  mutate(Group = "ME-5",
         Task = "SCT",
         School = "Monolingual",
         Age = "5th")


# Selection
SDB_FCT <- read_csv("./CSV Files/SDB/SDB DOM FCT.csv") %>% 
  mutate(Group = "SDBA",
         Task = "MST")

DLI78_FCT <- read_csv("./CSV Files/DLI-78/DLI-78 DOM FCT.csv") %>% 
  mutate(Group = "BES-7/8",
         Task = "MST",
         School = "Immersion",
         Age = "7th/8th")

MLS78_FCT <- read_csv("./CSV Files/MLS-78/MLS-78 DOM FCT.csv") %>%
  filter(!School == "GBCS") %>% 
  mutate(Group = "ME-7/8",
         Task = "MST",
         School = "Monolingual",
         Age = "7th/8th")

DLI5_FCT <- read_csv("./CSV Files/DLI-5/DLI-5 DOM FCT.csv") %>% 
  mutate(Group = "BES-5",
         Task = "MST",
         School = "Immersion",
         Age = "5th")

MLS5_FCT <- read_csv("./CSV Files/MLS-5/MLS-5 DOM FCT.csv") %>%
  filter(!School == "GBCS") %>% 
  mutate(Group = "ME-5",
         Task = "MST",
         School = "Monolingual",
         Age = "5th")


# Rejoin datasets
## Production
EPT <- rbind(SDB_EPT, DLI78_EPT, MLS78_EPT, DLI5_EPT, MLS5_EPT)

EPT_Heritage <- EPT %>% 
  filter(!Group == "SDBA")
EPT_Heritage$Age <- factor(EPT_Heritage$Age, levels = c("5th", "7th/8th"))
EPT_Heritage$School <- factor(EPT$School, levels = c("Immersion", "Monolingual"))


## Receptive task
FCT <- rbind(SDB_FCT, DLI78_FCT, MLS78_FCT, DLI5_FCT, MLS5_FCT)

FCT_Heritage <- FCT %>% 
  filter(!Group == "SDBA")
FCT_Heritage$Age <- factor(FCT_Heritage$Age, levels = c("5th", "7th/8th"))
FCT_Heritage$School <- factor(FCT$School, levels = c("Immersion", "Monolingual"))


## Joint
Aggregate <- rbind(EPT, FCT)
Aggregate$Group <- factor(Aggregate$Group, levels = c("SDBA", "BES-7/8", "ME-7/8", "BES-5", "ME-5"))
Aggregate$Task <- factor(Aggregate$Task, levels = c("SCT", "MST"))

Aggregate_Heritage <- Aggregate %>% 
  filter(!Group == "SDBA")


# Omnibus model
## Generate omnibus model
Omnibus <- glmer(DOM_Use ~ Group + Task +
                   (1 | Part_ID) + (1 | Item),
                 data = Aggregate,
                 family = "binomial")

summary(Omnibus)


## Plot omnibus model
plot_model(Omnibus, show.values = TRUE, show.intercept = TRUE, value.offset = .3, transform = NULL, vline.color = "black") +
  labs(title = "Summary of GLMM Model for Group and Task", y = "Estimates") +
  scale_y_continuous(breaks = seq(-9, 9, 3),
                     limits = c(-9, 9)) +
  scale_x_discrete(labels = c("MST (Task)", "MES-5 (Group)", "BES-5 (Group)", "ME-7/8 (Group)", "BES-7/8 (Group)", "(Intercept)")) +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"))


## Tukey post-hoc comparisons for omnibus
Omnibus_Pairwise <- emmeans(Omnibus, spec = "Group")
Omnibus_Tukey <- contrast(Omnibus_Pairwise, method = "pairwise")

summary(Omnibus_Tukey)


# Heritage model
## Generate heritage model
Heritage <- glmer(DOM_Use ~ School + Age + Task + Use_Joint_Std + BESA_Total_Std + School:Age +
               (1 | Part_ID) + (1 | Item),
             data = Aggregate_Heritage,
             family = "binomial")

summary(Heritage)


## Plot heritage model
plot_model(Heritage, show.values = TRUE, show.intercept = TRUE, value.offset = .3, transform = NULL, vline.color = "black") +
  labs(title = "Summary of GLMM Model for Heritage Speakers", y = "Estimates") +
  scale_y_continuous(breaks = seq(-9, 9, 3),
                     limits = c(-9, 9)) +
  scale_x_discrete(labels = c("School : Age", "Proficiency", "Frequency of use", "MST (Task)", "7th/8th grade", "Monolingual school", "(Intercept)")) +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"))


# Production model
## Generate production model
Production <- glmer(DOM_Use ~ School + Age + Use_Joint_Std + BESA_Total_Std + School:Age +
                      (1 | Part_ID) + (1 | Item),
                    data = EPT_Heritage,
                    family = "binomial")

summary(Production)


## Plot production model
plot_model(Production, show.values = TRUE, show.intercept = TRUE, value.offset = .3, transform = NULL, vline.color = "black") +
  labs(title = "Summary of GLMM Model for Production", y = "Estimates") +
  scale_y_continuous(breaks = seq(-9, 9, 3),
                     limits = c(-9, 9)) +
  scale_x_discrete(labels = c("School : Age", "Proficiency", "Frequency of use", "7th/8th grade", "Monolingual school", "(Intercept)")) +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"))


# Receptive model
## Generate receptive model
MST <- glmer(DOM_Use ~ School + Age + Use_Joint_Std + BESA_Total_Std + School:Age +
                      (1 | Part_ID) + (1 | Item),
                    data = FCT_Heritage,
                    family = "binomial")

summary(MST)


## Plot production model
plot_model(MST, show.values = TRUE, show.intercept = TRUE, value.offset = .3, transform = NULL, vline.color = "black") +
  labs(title = "Summary of GLMM Model for Selection", y = "Estimates") +
  scale_y_continuous(breaks = seq(-9, 9, 3),
                     limits = c(-9, 9)) +
  scale_x_discrete(labels = c("School : Age", "Proficiency", "Frequency of use", "7th/8th grade", "Monolingual school", "(Intercept)")) +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"))

