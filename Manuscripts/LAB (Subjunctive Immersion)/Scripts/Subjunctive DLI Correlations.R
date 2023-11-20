library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(sjPlot)

options(scipen = 99)


# Load data
## EPT
DLI78_EPT <- read_csv("./CSV Files/DLI-78/DLI-78 Subjunctive EPT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "DLI-7/8",
         Grade = "Older",
         School = "DLE",
         Task = "Production")

MLS78_EPT <- read_csv("./CSV Files/MLS-78/MLS-78 Subjunctive EPT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  filter(!School == "GBCS") %>% 
  mutate(Group = "MLE-7/8",
         Grade = "Older",
         School = "MLS",
         Task = "Production")

DLI5_EPT <- read_csv("./CSV Files/DLI-5/DLI-5 Subjunctive EPT.csv")  %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "DLI-5",
         Grade = "Fifth",
         School = "DLE",
         Task = "Production")

MLS5_EPT <- read_csv("./CSV Files/MLS-5/MLS-5 Subjunctive EPT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  filter(!School == "GBCS") %>% 
  mutate(Group = "MLE-5",
         Grade = "Fifth",
         School = "MLS",
         Task = "Production")

SDB_EPT <- read_csv("./CSV Files/SDB/SDB Subjunctive EPT.csv") %>% 
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "SDB",
         Task = "Production")


## FCT
DLI78_FCT <- read_csv("./CSV Files/DLI-78/DLI-78 Subjunctive FCT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "DLI-7/8",
         Grade = "Older",
         School = "DLE",
         Task = "Preference")

MLS78_FCT <- read_csv("./CSV Files/MLS-78/MLS-78 Subjunctive FCT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  filter(!School == "GBCS") %>% 
  mutate(Group = "MLE-7/8",
         Grade = "Older",
         School = "MLS",
         Task = "Preference")

DLI5_FCT <- read_csv("./CSV Files/DLI-5/DLI-5 Subjunctive FCT.csv")  %>%
  filter(Property == "Intensional subjunctive")%>% 
  mutate(Group = "DLI-5",
         Grade = "Fifth",
         School = "DLE",
         Task = "Preference")

MLS5_FCT <- read_csv("./CSV Files/MLS-5/MLS-5 Subjunctive FCT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  filter(!School == "GBCS") %>% 
  mutate(Group = "MLE-5",
         Grade = "Fifth",
         School = "MLS",
         Task = "Preference")

SDB_FCT <- read_csv("./CSV Files/SDB/SDB Subjunctive FCT.csv") %>% 
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "SDB",
         Task = "Preference")


# Rejoin datasets for all participants
## Heritage datasets
EPT_Heritage <- rbind(DLI78_EPT, MLS78_EPT, DLI5_EPT, MLS5_EPT)
FCT_Heritage <- rbind(DLI78_FCT, MLS78_FCT, DLI5_FCT, MLS5_FCT)

Aggregate_Heritage <- rbind(EPT_Heritage, FCT_Heritage)
Aggregate_Heritage$School <- factor(Aggregate_Heritage$School, levels = c("MLS", "DLE"))
Aggregate_Heritage$Grade <- factor(Aggregate_Heritage$Grade, levels = c("Fifth", "Older"))
Aggregate_Heritage$Task <- factor(Aggregate_Heritage$Task, levels = c("Production", "Preference"))


## Datasets with SDB
EPT_Master <- rbind(EPT_Heritage, SDB_EPT)
FCT_Master <- rbind(FCT_Heritage, SDB_FCT)

Aggregate_Master <- rbind(EPT_Master, FCT_Master)
Aggregate_Master$Group <- factor(Aggregate_Master$Group, levels = c("SDB", "DLI-5", "MLE-5", "DLI-7/8", "MLE-7/8"))
Aggregate_Master$Task <- factor(Aggregate_Master$Task, levels = c("Production", "Preference"))


# Group comparisons
## Generate group-level model
Omnibus_Correlation <- glmer(Mood_Use ~ Group +
                               (1 | Part_ID) + (1 | Item),
                             data = Aggregate_Master,
                             family = "binomial")

summary(Omnibus_Correlation)


## Generate forest plot
plot_model(Omnibus_Correlation, show.values = TRUE, show.intercept = TRUE, value.offset = .3, transform = NULL, vline.color = "black") +
  labs(title = "Summary of GLMM Model #1", y = "Parameter estimates") +
  scale_y_continuous(breaks = seq(-9, 9, 3),
                     limits = c(-9, 9)) +
  scale_x_discrete(labels = c("MLE-7/8", "DLI-7/8", "MLE-5", "DLI-5", "(Intercept)")) +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"))


## Tukey post-hoc comparisons
Omnibus_Pairwise <- emmeans(Omnibus_Correlation, spec = "Group")
Omnibus_Tukey <- contrast(Omnibus_Pairwise, method = "pairwise")

summary(Omnibus_Tukey)


# Model with HS data only
## Generate model
HS_Correlation <- glmer(Mood_Use ~ Use_Joint_Std + School + BESA_Other_Std + Grade + Task + Use_Joint_Std:Task + School:Grade + School:Task +
                  (1 | Part_ID) + (1 | Item),
                data = Aggregate_Heritage,
                family = "binomial")

summary(HS_Correlation)


## Generate forest plot
plot_model(HS_Correlation, show.values = TRUE, show.intercept = TRUE, value.offset = .3, transform = NULL, vline.color = "black") +
  scale_y_continuous(breaks = seq(-3, 3, 1),
                     limits = c(-3, 3)) +
  scale_x_discrete(labels = c("DLI school : Preference task", "DLI school : 7th/8th grade", "Freq. of use : Preference task", "Preference task", "7th/8th grade", "BESA proficiecy", "DLE school", "Frequency of use", "(Intercept)")) +
  labs(title = "Summary of GLMM Model #2", y = "Parameter estimates") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"))


# Individual item analysis
## Generate model
Lexical_Analysis <- glmer(Mood_Use ~ Verb +
                            (1 | Part_ID) + (1 | Item),
                          data = Aggregate_Heritage,
                          family = "binomial")

summary(Lexical_Analysis)
