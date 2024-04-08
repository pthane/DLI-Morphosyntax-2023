library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(sjPlot)
library(performance)


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


# Rejoin datasets for all participants
## Heritage datasets
EPT <- rbind(DLI78_EPT, MLS78_EPT, DLI5_EPT, MLS5_EPT)
FCT <- rbind(DLI78_FCT, MLS78_FCT, DLI5_FCT, MLS5_FCT)

Aggregate <- rbind(EPT, FCT)
Aggregate$School <- factor(Aggregate$School, levels = c("MLS", "DLE"))
Aggregate$Grade <- factor(Aggregate$Grade, levels = c("Fifth", "Older"))
Aggregate$Task <- factor(Aggregate$Task, levels = c("Production", "Selection"))


# HS model
## Generate model
HS_Correlation_Null <- glmer(Mood_Use ~ 1 +
                               (1 | Part_ID) + (1 | Item),
                             data = Aggregate,
                             family = "binomial")

HS_Correlation_School <- glmer(Mood_Use ~ 1 + School +
                               (1 | Part_ID) + (1 | Item),
                             data = Aggregate,
                             family = "binomial")

HS_Correlation_Grade <- glmer(Mood_Use ~ 1 + School + Grade +
                                (1 | Part_ID) + (1 | Item),
                              data = Aggregate,
                              family = "binomial")

HS_Correlation_BESA <- glmer(Mood_Use ~ School + Grade + BESA_Other_Std +
                               (1 | Part_ID) + (1 | Item),
                             data = Aggregate,
                             family = "binomial")

HS_Correlation_Use <- glmer(Mood_Use ~ School + Grade + BESA_Other_Std + Use_Joint_Std +
                               (1 | Part_ID) + (1 | Item),
                             data = Aggregate,
                             family = "binomial")

HS_Correlation_Task <- glmer(Mood_Use ~ School + Grade + BESA_Other_Std + Use_Joint_Std + Task +
                              (1 | Part_ID) + (1 | Item),
                            data = Aggregate,
                            family = "binomial")

HS_Correlation_LexFreq <- glmer(Mood_Use ~ School + Grade + BESA_Other_Std + Use_Joint_Std + Task + Davies_Std +
                               (1 | Part_ID) + (1 | Item),
                             data = Aggregate,
                             family = "binomial")

HS_Correlation_School_Grade <- glmer(Mood_Use ~ School + Grade + BESA_Other_Std + Use_Joint_Std + Task + Davies_Std + School:Grade +
                          (1 | Part_ID) + (1 | Item),
                        data = Aggregate,
                        family = "binomial")

HS_Correlation_School_Task <- glmer(Mood_Use ~ School + Grade + BESA_Other_Std + Use_Joint_Std + Task + Davies_Std + School:Grade + School:Task +
                                       (1 | Part_ID) + (1 | Item),
                                     data = Aggregate,
                                     family = "binomial")

HS_Correlation_Full <- glmer(Mood_Use ~ School + Grade + BESA_Other_Std + Use_Joint_Std + Task + Davies_Std + School:Grade + School:Task + Use_Joint_Std:Task +
                                      (1 | Part_ID) + (1 | Item),
                                    data = Aggregate,
                                    family = "binomial")

anova(HS_Correlation_Null, HS_Correlation_School, HS_Correlation_Grade, HS_Correlation_BESA, HS_Correlation_Use, HS_Correlation_Task, HS_Correlation_LexFreq, HS_Correlation_School_Grade, HS_Correlation_School_Task, HS_Correlation_Full)


## Final model
summary(HS_Correlation_Task)
r2_nakagawa(HS_Correlation_Task)


## Forest plot
plot_model(HS_Correlation_Task, show.values = TRUE, show.intercept = TRUE, value.offset = .3, transform = NULL, vline.color = "black") +
  labs(title = "Summary of HS GLMM Model", y = "Parameter estimates") +
  scale_y_continuous(breaks = seq(-2.5, 2.5, 1.25),
                     limits = c(-2.5, 2.5)) +
  scale_x_discrete(labels = c("Selection task", "Frequency of use", "BESA proficiency", "7th/8th grade group", "MLE school", "(Intercept)")) +
    theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"))
