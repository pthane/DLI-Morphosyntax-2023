library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(sjPlot)


options(scipen = 99)


# Load subjunctive data
## EPT
SDB_EPT_Subj <- read_csv("./CSV Files/SDB/SDB Subjunctive EPT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "SDBA")

HSA_EPT_Subj <- read_csv("./CSV Files/Adult HS/Adult HS Subjunctive EPT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HSA")

DLI78_EPT_Subj <- read_csv("./CSV Files/DLI-78/DLI-78 Subjunctive EPT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS7/8")

MLS78_EPT_Subj <- read_csv("./CSV Files/MLS-78/MLS-78 Subjunctive EPT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS7/8")

DLI5_EPT_Subj <- read_csv("./CSV Files/DLI-5/DLI-5 Subjunctive EPT.csv")  %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS5")

MLS5_EPT_Subj <- read_csv("./CSV Files/MLS-5/MLS-5 Subjunctive EPT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS5")


## FCT
SDB_FCT_Subj <- read_csv("./CSV Files/SDB/SDB Subjunctive FCT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "SDBA")

HSA_FCT_Subj <- read_csv("./CSV Files/Adult HS/Adult HS Subjunctive FCT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HSA")

DLI78_FCT_Subj <- read_csv("./CSV Files/DLI-78/DLI-78 Subjunctive FCT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS7/8")

MLS78_FCT_Subj <- read_csv("./CSV Files/MLS-78/MLS-78 Subjunctive FCT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS7/8")

DLI5_FCT_Subj <- read_csv("./CSV Files/DLI-5/DLI-5 Subjunctive FCT.csv")  %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS5")

MLS5_FCT_Subj <- read_csv("./CSV Files/MLS-5/MLS-5 Subjunctive FCT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS5")


# Load DOM data
## EPT
SDB_EPT_DOM <- read_csv("./CSV Files/SDB/SDB DOM EPT.csv") %>%
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>% 
  mutate(Group = "SDBA")

HSA_EPT_DOM <- read_csv("./CSV Files/Adult HS/Adult HS DOM EPT.csv") %>%
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>% 
  mutate(Group = "HSA")

DLI78_EPT_DOM <- read_csv("./CSV Files/DLI-78/DLI-78 DOM EPT.csv") %>%
  mutate(Group = "HS7/8")

MLS78_EPT_DOM <- read_csv("./CSV Files/MLS-78/MLS-78 DOM EPT.csv") %>%
  mutate(Group = "HS7/8")

DLI5_EPT_DOM <- read_csv("./CSV Files/DLI-5/DLI-5 DOM EPT.csv")  %>%
  mutate(Group = "HS5")

MLS5_EPT_DOM <- read_csv("./CSV Files/MLS-5/MLS-5 DOM EPT.csv") %>%
  mutate(Group = "HS5")


## FCT
SDB_FCT_DOM <- read_csv("./CSV Files/SDB/SDB DOM FCT.csv") %>%
  filter(Item %in% c("FCT-02", "FCT-08", "FCT-12", "FCT-14", "FCT-20", "FCT-24", "FCT-26", "FCT-32", "FCT-38", "FCT-42")) %>% 
  mutate(Group = "SDBA")

HSA_FCT_DOM <- read_csv("./CSV Files/Adult HS/Adult HS DOM FCT.csv") %>%
  mutate(Group = "HSA")

DLI78_FCT_DOM <- read_csv("./CSV Files/DLI-78/DLI-78 DOM FCT.csv") %>%
  mutate(Group = "HS7/8")

MLS78_FCT_DOM <- read_csv("./CSV Files/MLS-78/MLS-78 DOM FCT.csv") %>%
  mutate(Group = "HS7/8")

DLI5_FCT_DOM <- read_csv("./CSV Files/DLI-5/DLI-5 DOM FCT.csv")  %>%
  mutate(Group = "HS5")

MLS5_FCT_DOM <- read_csv("./CSV Files/MLS-5/MLS-5 DOM FCT.csv") %>%
  mutate(Group = "HS5")


# Merge datasets
## Subjunctive
EPT_Subj <- rbind(SDB_EPT_Subj, HSA_EPT_Subj, DLI78_EPT_Subj, MLS78_EPT_Subj, DLI5_EPT_Subj, MLS5_EPT_Subj)
FCT_Subj <- rbind(SDB_FCT_Subj, HSA_FCT_Subj, DLI78_FCT_Subj, MLS78_FCT_Subj, DLI5_FCT_Subj, MLS5_FCT_Subj)

Aggregate_Subj <- rbind(EPT_Subj, FCT_Subj)
Aggregate_Subj$Group <- factor(Aggregate_Subj$Group, levels = c("SDBA", "HSA", "HS7/8", "HS5"))


## DOM
EPT_DOM <- rbind(SDB_EPT_DOM, HSA_EPT_DOM, DLI78_EPT_DOM, MLS78_EPT_DOM, DLI5_EPT_DOM, MLS5_EPT_DOM)
FCT_DOM <- rbind(SDB_FCT_DOM, HSA_FCT_DOM, DLI78_FCT_DOM, MLS78_FCT_DOM, DLI5_FCT_DOM, MLS5_FCT_DOM)

Aggregate_DOM <- rbind(EPT_DOM, FCT_DOM)
Aggregate_DOM$Group <- factor(Aggregate_DOM$Group, levels = c("SDBA", "HSA", "HS7/8", "HS5"))


# Subjunctive correlation
## Model
Subj_GLMM <- glmer(Mood_Use ~ Group + Use_Joint_Std + Task + Use_Joint_Std:Task +
                    (1 | Part_ID) + (1 | Item),
                  data = Aggregate_Subj,
                  family = "binomial")

summary(Subj_GLMM)


## Tukey post-hoc comparisons
Subj_Pairwise <- emmeans(Subj_GLMM, spec = "Group")
Subj_Tukey <- contrast(Subj_Pairwise, method = "pairwise")
summary(Subj_Tukey)


## Forest plot in English
plot_model(Subj_GLMM, show.values = TRUE, show.intercept = TRUE, value.offset = .3, transform = NULL, vline.color = "black") +
  labs(title = "Results of GLMM Model for Subjunctive", y = "Estimate effect size") +
  scale_y_continuous(breaks = seq(-10, 10, 2.5),
                     limits = c(-10, 10)) +
  scale_x_discrete(labels = c("Use : Selection", "Selection task", "Frequency of use", "HS5 group", "HS7/8 group", "HSA group", "(Intercept)")) +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"))


## Forest plot in Spanish
plot_model(Subj_GLMM, show.values = TRUE, show.intercept = TRUE, value.offset = .3, transform = NULL, vline.color = "black") +
  labs(title = "Resultados del modelo GLMM del subjuntivo", y = "Tamaño de efecto") +
  scale_y_continuous(breaks = seq(-10, 10, 2.5),
                     limits = c(-10, 10)) +
  scale_x_discrete(labels = c("Uso : Selección", "Tarea de selección", "Frequencia of uso", "Grupo HH 5° grado", "Grupo HH 7°/8° grado", "Grupo HH adultos", "(Intersección)")) +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"))


# DOM correlations
## Model
DOM_GLMM <- glmer(DOM_Use ~ Group + Use_Joint_Std + Task + Use_Joint_Std:Task +
                    (1 | Part_ID) + (1 | Item),
                  data = Aggregate_DOM,
                  family = "binomial")

summary(DOM_GLMM)


## Tukey post-hoc comparisons
DOM_Pairwise <- emmeans(DOM_GLMM, spec = "Group")
DOM_Tukey <- contrast(DOM_Pairwise, method = "pairwise")
summary(DOM_Tukey)


## Forest plot in English
plot_model(DOM_GLMM, show.values = TRUE, show.intercept = TRUE, value.offset = .3, transform = NULL, vline.color = "black") +
  labs(title = "Results of GLMM Model for DOM", y = "Estimate effect size") +
  scale_y_continuous(breaks = seq(-10, 10, 2.5),
                     limits = c(-10, 10)) +
  scale_x_discrete(labels = c("Use : Selection", "Selection task", "Frequency of use", "HS5 group", "HS7/8 group", "HSA group", "(Intercept)")) +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"))


## Forest plot in Spanish
plot_model(DOM_GLMM, show.values = TRUE, show.intercept = TRUE, value.offset = .3, transform = NULL, vline.color = "black") +
  labs(title = "Resultados del modelo GLMM del MDO", y = "Tamaño de efecto") +
  scale_y_continuous(breaks = seq(-10, 10, 2.5),
                     limits = c(-10, 10)) +
  scale_x_discrete(labels = c("Uso : Selección", "Tarea de selección", "Frequencia of uso", "Grupo HH 5° grado", "Grupo HH 7°/8° grado", "Grupo HH adultos", "(Intersección)")) +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"))
