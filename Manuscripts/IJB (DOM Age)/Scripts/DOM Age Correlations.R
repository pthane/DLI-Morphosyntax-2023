library(tidyverse)
library(lme4)
library(lmerTest)
library(sjPlot)
library(emmeans)

options(scipen = 99)


# Load data
## EPT
SDB_EPT <- read_csv("./CSV Files/SDB/SDB DOM EPT.csv") %>%
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>% 
  mutate(Group = "SDB")

HSA_EPT <- read_csv("./CSV Files/Adult HS/Adult HS DOM EPT.csv") %>%
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>% 
  mutate(Group = "HSA")

DLI78_EPT <- read_csv("./CSV Files/DLI-78/DLI-78 DOM EPT.csv") %>%
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>% 
  mutate(Group = "HS7/8")

MLS78_EPT <- read_csv("./CSV Files/MLS-78/MLS-78 DOM EPT.csv") %>%
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>% 
  mutate(Group = "HS7/8")

DLI5_EPT <- read_csv("./CSV Files/DLI-5/DLI-5 DOM EPT.csv") %>%
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>% 
  mutate(Group = "HS5")

MLS5_EPT <- read_csv("./CSV Files/MLS-5/MLS-5 DOM EPT.csv") %>%
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>% 
  mutate(Group = "HS5")


## FCT
SDB_FCT <- read_csv("./CSV Files/SDB/SDB DOM FCT.csv") %>%
  mutate(Group = "SDB")

HSA_FCT <- read_csv("./CSV Files/Adult HS/Adult HS DOM FCT.csv") %>%
  mutate(Group = "HSA")

DLI78_FCT <- read_csv("./CSV Files/DLI-78/DLI-78 DOM FCT.csv") %>%
  mutate(Group = "HS7/8")

MLS78_FCT <- read_csv("./CSV Files/MLS-78/MLS-78 DOM FCT.csv") %>%
  mutate(Group = "HS7/8")

DLI5_FCT <- read_csv("./CSV Files/DLI-5/DLI-5 DOM FCT.csv") %>%
  mutate(Group = "HS5")

MLS5_FCT <- read_csv("./CSV Files/MLS-5/MLS-5 DOM FCT.csv") %>%
  mutate(Group = "HS5")


## Rejoin datasets for all participants
EPT <- rbind(SDB_EPT, HSA_EPT, DLI78_EPT, MLS78_EPT, DLI5_EPT, MLS5_EPT)
FCT <- rbind(SDB_FCT, HSA_FCT, DLI78_FCT, MLS78_FCT, DLI5_FCT, MLS5_FCT)

Aggregate <- rbind(EPT, FCT)
Aggregate$Group <- factor(Aggregate$Group, levels = c("SDB", "HSA", "HS7/8", "HS5"))

Aggregate_Heritage <- Aggregate %>% 
  filter(!Group == "SDB")
  

# Master model
## Generate model and summary
DOM_Omnibus <- glmer(DOM_Use ~ Group + 
                    (1 | Part_ID) + (1 | Item),
                  data = Aggregate,
                  family = "binomial")

summary(DOM_Omnibus)
confint(DOM_Omnibus)


## Tukey post-hoc comparisons
DOM_Pairwise <- emmeans(DOM_Omnibus, spec = "Group")
DOM_Tukey <- contrast(DOM_Pairwise, method = "pairwise")

summary(DOM_Tukey)
confint(DOM_Tukey)


## Plot model
plot_model(DOM_Omnibus, show.values = TRUE, show.intercept = TRUE, value.offset = .3, transform = NULL, vline.color = "black") +
  scale_y_continuous(breaks = seq(-8, 8, 2),
                     limits = c(-8, 8)) +
  scale_x_discrete(labels = c("HS5 Group", "HS7/8 Group", "HSA Group", "(Intercept)")) +
  labs(title = "Summary of GLMM Model for Group-Level Differences", y = "Estimate effect size") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"))


# HS-specific model
DOM_HS <- glmer(DOM_Use ~ Task + BESA_Total_Std + Use_Joint_Std + Task:BESA_Total_Std + Task:Use_Joint_Std +
                  (1 | Part_ID) + (1 | Item),
                data = Aggregate_Heritage,
                family = "binomial")

summary(DOM_HS)


## Plot model
plot_model(DOM_HS, show.values = TRUE, show.intercept = TRUE, value.offset = .3, transform = NULL, vline.color = "black") +
  scale_x_discrete(labels = c("MST : Frequency of use", "MST : BESA Proficiency", "Frequency of use", "BESA Proficiency", "MST (Task)", "(Intercept)")) +
  labs(title = "Summary of GLMM Model for Heritage Data", y = "Estimate effect size") +
  scale_y_continuous(breaks = seq(-4, 4, 2),
                     limits = c(-4, 4)) +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"))
