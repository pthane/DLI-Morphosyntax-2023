library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(sjPlot)


options(scipen = 99)


# Load subjunctive data
## EPT
DLI78_EPT_Subj <- read_csv("./CSV Files/DLI-78/DLI-78 Subjunctive EPT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(School = "DLI",
         Age = "7th/8th")

MLS78_EPT_Subj <- read_csv("./CSV Files/MLS-78/MLS-78 Subjunctive EPT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(School = "ML",
         Age = "7th/8th")

DLI5_EPT_Subj <- read_csv("./CSV Files/DLI-5/DLI-5 Subjunctive EPT.csv")  %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(School = "DLI",
         Age = "5th")

MLS5_EPT_Subj <- read_csv("./CSV Files/MLS-5/MLS-5 Subjunctive EPT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(School = "ML",
         Age = "5th")


## FCT
DLI78_FCT_Subj <- read_csv("./CSV Files/DLI-78/DLI-78 Subjunctive FCT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(School = "DLI",
         Age = "7th/8th")

MLS78_FCT_Subj <- read_csv("./CSV Files/MLS-78/MLS-78 Subjunctive FCT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(School = "ML",
         Age = "7th/8th")

DLI5_FCT_Subj <- read_csv("./CSV Files/DLI-5/DLI-5 Subjunctive FCT.csv")  %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(School = "DLI",
         Age = "5th")

MLS5_FCT_Subj <- read_csv("./CSV Files/MLS-5/MLS-5 Subjunctive FCT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(School = "ML",
         Age = "5th")


# Load DOM data
## EPT
DLI78_EPT_DOM <- read_csv("./CSV Files/DLI-78/DLI-78 DOM EPT.csv") %>% 
  mutate(School = "DLI",
         Age = "7th/8th")

MLS78_EPT_DOM <- read_csv("./CSV Files/MLS-78/MLS-78 DOM EPT.csv") %>% 
  mutate(School = "ML",
         Age = "7th/8th")

DLI5_EPT_DOM <- read_csv("./CSV Files/DLI-5/DLI-5 DOM EPT.csv") %>% 
  mutate(School = "DLI",
         Age = "5th")

MLS5_EPT_DOM <- read_csv("./CSV Files/MLS-5/MLS-5 DOM EPT.csv") %>% 
  mutate(School = "ML",
         Age = "5th")


## FCT
DLI78_FCT_DOM <- read_csv("./CSV Files/DLI-78/DLI-78 DOM FCT.csv") %>% 
  mutate(School = "DLI",
         Age = "7th/8th")

MLS78_FCT_DOM <- read_csv("./CSV Files/MLS-78/MLS-78 DOM FCT.csv") %>% 
  mutate(School = "ML",
         Age = "7th/8th")

DLI5_FCT_DOM <- read_csv("./CSV Files/DLI-5/DLI-5 DOM FCT.csv") %>% 
  mutate(School = "DLI",
         Age = "5th")


MLS5_FCT_DOM <- read_csv("./CSV Files/MLS-5/MLS-5 DOM FCT.csv") %>% 
  mutate(School = "ML",
         Age = "5th")


# Merge datasets
## Subjunctive
EPT_Subj <- rbind(DLI78_EPT_Subj, MLS78_EPT_Subj, DLI5_EPT_Subj, MLS5_EPT_Subj)
FCT_Subj <- rbind(DLI78_FCT_Subj, MLS78_FCT_Subj, DLI5_FCT_Subj, MLS5_FCT_Subj)

Aggregate_Subj <- rbind(EPT_Subj, FCT_Subj)
Aggregate_Subj$Group <- factor(Aggregate_Subj$Group, levels = c("DLI-7/8", "MLS-7/8", "DLI-5", "MLS-5"))
Aggregate_Subj$School <- factor(Aggregate_Subj$School, levels = c("DLI", "ML"))
Aggregate_Subj$Age <- factor(Aggregate_Subj$Age, levels = c("7th/8th", "5th"))


## DOM
EPT_DOM <- rbind(DLI78_EPT_DOM, MLS78_EPT_DOM, DLI5_EPT_DOM, MLS5_EPT_DOM)
FCT_DOM <- rbind(DLI78_FCT_DOM, MLS78_FCT_DOM, DLI5_FCT_DOM, MLS5_FCT_DOM)

Aggregate_DOM <- rbind(EPT_DOM, FCT_DOM)
Aggregate_DOM$Group <- factor(Aggregate_DOM$Group, levels = c("DLI-7/8", "MLS-7/8", "DLI-5", "MLS-5"))
Aggregate_DOM$School <- factor(Aggregate_DOM$School, levels = c("DLI", "ML"))
Aggregate_DOM$Age <- factor(Aggregate_DOM$Age, levels = c("7th/8th", "5th"))


# Subjunctive correlation
## Model with separate independent variables
Subj_GLMM <- glmer(Mood_Use ~ School * Age +
                     (1 | Part_ID) + (1 | Item),
                   data = Aggregate_Subj,
                   family = "binomial")

summary(Subj_GLMM)


## Model with group only
Subj_Group <- glmer(Mood_Use ~ Group +
                     (1 | Part_ID) + (1 | Item),
                   data = Aggregate_Subj,
                   family = "binomial")

summary(Subj_Group)


## Tukey post-hoc comparisons
Subj_Pairwise <- emmeans(Subj_Group, spec = "Group")
Subj_Tukey <- contrast(Subj_Pairwise, method = "pairwise")
summary(Subj_Tukey)


## Forest plot in English
plot_model(Subj_GLMM, show.values = TRUE, show.intercept = TRUE, value.offset = .3, transform = NULL, vline.color = "black") +
  labs(title = "Results of GLMM Model for Subjunctive with Child HS Only", y = "Estimate effect size") +
  scale_y_continuous(breaks = seq(-4, 4, 2),
                     limits = c(-4, 4)) +
  scale_x_discrete(labels = c("Monolingual : 5th grade", "5th grade group", "Monolingual school", "(Intercept)")) +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"))


## Forest plot in Spanish
plot_model(Subj_GLMM, show.values = TRUE, show.intercept = TRUE, value.offset = .3, transform = NULL, vline.color = "black") +
  labs(title = "Resultados del modelo GLMM para el subjuntivo con niños", y = "Tamaño de efecto") +
  scale_y_continuous(breaks = seq(-4, 4, 2),
                     limits = c(-4, 4)) +
  scale_x_discrete(labels = c("EML : 5° grado", "HH 5° grado", "EML", "(Intercept)")) +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"))


# DOM correlations
## Model with separate independent variables
DOM_GLMM <- glmer(DOM_Use ~ School * Age +
                    (1 | Part_ID) + (1 | Item),
                  data = Aggregate_DOM,
                  family = "binomial")

summary(DOM_GLMM)


## Model with group only
DOM_Group <- glmer(DOM_Use ~ Group +
                     (1 | Part_ID) + (1 | Item),
                   data = Aggregate_DOM,
                   family = "binomial")

summary(DOM_Group)


## Tukey post-hoc comparisons
DOM_Pairwise <- emmeans(DOM_Group, spec = "Group")
DOM_Tukey <- contrast(DOM_Pairwise, method = "pairwise")
summary(DOM_Tukey)


## Forest plot in English
plot_model(DOM_GLMM, show.values = TRUE, show.intercept = TRUE, value.offset = .3, transform = NULL, vline.color = "black") +
  labs(title = "Results of GLMM Model for Subjunctive with Child HS Only", y = "Estimate effect size") +
  scale_y_continuous(breaks = seq(-4, 4, 2),
                     limits = c(-4, 4)) +
  scale_x_discrete(labels = c("Monolingual : 5th grade", "5th grade group", "Monolingual school", "(Intercept)")) +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"))


## Forest plot in Spanish
plot_model(DOM_GLMM, show.values = TRUE, show.intercept = TRUE, value.offset = .3, transform = NULL, vline.color = "black") +
  labs(title = "Resultados del modelo GLMM para el subuntivo con niños", y = "Tamaño de efecto") +
  scale_y_continuous(breaks = seq(-4, 4, 2),
                     limits = c(-4, 4)) +
  scale_x_discrete(labels = c("EML : 5° grado", "HH 5° grado", "EML", "(Intercept)")) +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"))
