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
         Task = "Selection")

MLS78_FCT <- read_csv("./CSV Files/MLS-78/MLS-78 Subjunctive FCT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  filter(!School == "GBCS") %>% 
  mutate(Group = "MLE-7/8",
         Grade = "Older",
         School = "MLS",
         Task = "Selection")

DLI5_FCT <- read_csv("./CSV Files/DLI-5/DLI-5 Subjunctive FCT.csv")  %>%
  filter(Property == "Intensional subjunctive")%>% 
  mutate(Group = "DLI-5",
         Grade = "Fifth",
         School = "DLE",
         Task = "Selection")

MLS5_FCT <- read_csv("./CSV Files/MLS-5/MLS-5 Subjunctive FCT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  filter(!School == "GBCS") %>% 
  mutate(Group = "MLE-5",
         Grade = "Fifth",
         School = "MLS",
         Task = "Selection")

SDB_FCT <- read_csv("./CSV Files/SDB/SDB Subjunctive FCT.csv") %>% 
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "SDB",
         Task = "Selection")


# Rejoin datasets for all participants
## Heritage datasets
EPT_Heritage <- rbind(DLI78_EPT, MLS78_EPT, DLI5_EPT, MLS5_EPT)
FCT_Heritage <- rbind(DLI78_FCT, MLS78_FCT, DLI5_FCT, MLS5_FCT)

Aggregate_Heritage <- rbind(EPT_Heritage, FCT_Heritage)
Aggregate_Heritage$School <- factor(Aggregate_Heritage$School, levels = c("MLS", "DLE"))
Aggregate_Heritage$Grade <- factor(Aggregate_Heritage$Grade, levels = c("Fifth", "Older"))
Aggregate_Heritage$Task <- factor(Aggregate_Heritage$Task, levels = c("Production", "Selection"))


## Datasets with SDB
EPT_Master <- rbind(EPT_Heritage, SDB_EPT)
FCT_Master <- rbind(FCT_Heritage, SDB_FCT)

Aggregate_Master <- rbind(EPT_Master, FCT_Master)
Aggregate_Master$Group <- factor(Aggregate_Master$Group, levels = c("SDB", "DLI-7/8", "MLE-7/8", "DLI-5", "MLE-5"))
Aggregate_Master$Task <- factor(Aggregate_Master$Task, levels = c("Production", "Selection"))


# Group model
## Generate model
Omnibus <- glmer(Mood_Use ~ Group +
                   (1 | Part_ID) + (1 | Item),
                 data = Aggregate_Master,
                 family = "binomial")

summary(Omnibus)


## Generate forest plot
plot_model(Omnibus, show.values = TRUE, show.intercept = TRUE, value.offset = .3, transform = NULL, vline.color = "black") +
  labs(title = "Summary of HS Group Differences from SDBA", y = "Parameter estimates") +
  scale_y_continuous(breaks = seq(-9, 9, 3),
                     limits = c(-9, 9)) +
  scale_x_discrete(labels = c("MLE-7/8", "DLI-7/8", "MLE-5", "DLI-5", "(Intercept)")) +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"))


## Tukey post-hoc comparisons
Omnibus_Pairwise <- emmeans(Omnibus, spec = "Group")
Omnibus_Tukey <- contrast(Omnibus_Pairwise, method = "pairwise")

summary(Omnibus_Tukey)



# HS model
## Generate model
HS_Correlation_Null <- glmer(Mood_Use ~ 1 +
                               (1 | Part_ID) + (1 | Item),
                             data = Aggregate_Heritage,
                             family = "binomial")

HS_Correlation_School <- glmer(Mood_Use ~ 1 + School +
                               (1 | Part_ID) + (1 | Item),
                             data = Aggregate_Heritage,
                             family = "binomial")

HS_Correlation_Grade <- glmer(Mood_Use ~ 1 + School + Grade +
                                (1 | Part_ID) + (1 | Item),
                              data = Aggregate_Heritage,
                              family = "binomial")

HS_Correlation_BESA <- glmer(Mood_Use ~ School + Grade + BESA_Other_Std +
                               (1 | Part_ID) + (1 | Item),
                             data = Aggregate_Heritage,
                             family = "binomial")

HS_Correlation_Use <- glmer(Mood_Use ~ School + Grade + BESA_Other_Std + Use_Joint_Std +
                               (1 | Part_ID) + (1 | Item),
                             data = Aggregate_Heritage,
                             family = "binomial")

HS_Correlation_Task <- glmer(Mood_Use ~ School + Grade + BESA_Other_Std + Use_Joint_Std + Task +
                              (1 | Part_ID) + (1 | Item),
                            data = Aggregate_Heritage,
                            family = "binomial")

HS_Correlation_School_Grade <- glmer(Mood_Use ~ School + Grade + BESA_Other_Std + Use_Joint_Std + Task + School:Grade +
                          (1 | Part_ID) + (1 | Item),
                        data = Aggregate_Heritage,
                        family = "binomial")

HS_Correlation_School_Task <- glmer(Mood_Use ~ School + Grade + BESA_Other_Std + Use_Joint_Std + Task + School:Grade + School:Task +
                                       (1 | Part_ID) + (1 | Item),
                                     data = Aggregate_Heritage,
                                     family = "binomial")

HS_Correlation_Full <- glmer(Mood_Use ~ School + Grade + BESA_Other_Std + Use_Joint_Std + Task + School:Grade + School:Task + Use_Joint_Std:Task +
                                      (1 | Part_ID) + (1 | Item),
                                    data = Aggregate_Heritage,
                                    family = "binomial")

anova(HS_Correlation_Null, HS_Correlation_School, HS_Correlation_Grade, HS_Correlation_BESA, HS_Correlation_Use, HS_Correlation_Task, HS_Correlation_School_Grade, HS_Correlation_School_Task, HS_Correlation_Full)


## Final model
HS_Correlation_Final <- glmer(Mood_Use ~ Grade + Use_Joint_Std + Task +
                                (1 | Part_ID) + (1 | Item),
                              data = Aggregate_Heritage,
                              family = "binomial")

summary(HS_Correlation_Final)


## Generate forest plot
plot_model(HS_Correlation_Final, show.values = TRUE, show.intercept = TRUE, value.offset = .3, transform = NULL, vline.color = "black") +
  labs(title = "Summary of HS GLMM Model", y = "Parameter estimates") +
  scale_y_continuous(breaks = seq(-2.5, 2.5, 1.25),
                     limits = c(-2.5, 2.5)) +
  scale_x_discrete(labels = c("Preference task", "Frequency of use", "7th/8th grade group", "(Intercept)")) +
    theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"))


# Frequency model
Frequency_Model <- glmer(Mood_Freq ~ Davies_Centered_Std +
                          (1 | Part_ID) + (1 | Item),
                        data = Aggregate_Heritage,
                        family = "binomial")

summary(Frequency_Model)
