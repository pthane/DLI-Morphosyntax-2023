library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(sjPlot)

options(scipen = 99)


# Load data
#EPT
SDB_EPT <- read_csv("./CSV Files/SDB/SDB Infinitive EPT.csv") %>% 
  mutate(Group = "SDB")

HSP_EPT <- read_csv("./CSV Files/Adult HS/Adult HS Infinitive EPT.csv") %>% 
  mutate(Group = "Heritage Speakers") %>% 
  filter(!Part_ID == "HSP11") %>% 
  mutate(DELE_Std = (DELE - mean(DELE))/sd(DELE))

L2L_EPT <- read_csv("./CSV Files/Adult L2L/Adult L2L Infinitive EPT.csv") %>% 
  mutate(Group = "L2 Learners")


## FCT
SDB_FCT <- read_csv("./CSV Files/SDB/SDB Infinitive FCT.csv") %>% 
  mutate(Group = "SDB")

HSP_FCT <- read_csv("./CSV Files/Adult HS/Adult HS Infinitive FCT.csv") %>% 
  mutate(Group = "Heritage Speakers") %>% 
  filter(!Part_ID == "HSP11") %>% 
  mutate(DELE_Std = (DELE - mean(DELE))/sd(DELE))

L2L_FCT <- read_csv("./CSV Files/Adult L2L/Adult L2L Infinitive FCT.csv") %>% 
  mutate(Group = "L2 Learners")


## Rejoin and generate joined datasets
EPT <- rbind(SDB_EPT, HSP_EPT, L2L_EPT)
FCT <- rbind(SDB_FCT, HSP_FCT, L2L_FCT)

Aggregate <- rbind(EPT, FCT)
Aggregate$Group <- factor(Aggregate$Group, levels = c("SDB", "Heritage Speakers", "L2 Learners"))
Aggregate$Short_Prop <- factor(Aggregate$Short_Prop, levels = c("Subject", "Object", "Preposition"))

Aggregate_Bilingual <- rbind(HSP_EPT, L2L_EPT, HSP_FCT, L2L_FCT)
Aggregate_Bilingual$Short_Prop <- factor(Aggregate_Bilingual$Short_Prop, levels = c("Subject", "Object", "Preposition"))


# Run omnibus correlation
## Prepare model
Omnibus <- glmer(Comp_Use ~ Group + Task + Short_Prop + Group:Task + Group:Short_Prop +
                   (1 | Part_ID) + (1 | Item),
                 data = Aggregate,
                 family = "binomial")

summary(Omnibus)


## Tukey comparisons by group
Omnibus_Pairwise_Group <- emmeans(Omnibus, spec = "Group")
Omnibus_Tukey_Group <- contrast(Omnibus_Pairwise_Group, method = "pairwise")
summary(Omnibus_Tukey_Group)


## Tukey comparisons by structure
Omnibus_Pairwise_Property <- emmeans(Omnibus, spec = "Short_Prop")
Omnibus_Tukey_Property <- contrast(Omnibus_Pairwise_Property, method = "pairwise")
summary(Omnibus_Tukey_Property)


plot_model(Omnibus, show.values = TRUE, show.intercept = TRUE, value.offset = .3, transform = NULL, vline.color = "black") +
  scale_y_continuous(breaks = seq(-1.5, 1.5, 0.5),
                     limits = c(-1.25, 1.25)) +
  scale_x_discrete(labels = c("L2L : Preposition", "L2L : Object", "Heritage : Preposition", "Heritage : Object", "L2L : FCT", "Heritage : FCT", "Prepositional infinitives", "Object infinitives", "FCT", "L2 Learners", "Heritage Speakers", "(Intercept)")) +
  labs(title = "Results of Omnibus Model", y = "Parameter Estimates") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"))


# Bilingual model
Bilingual <- lmer(Comp_Use ~ Group + Short_Prop + DELE_Std + Group:DELE_Std + Short_Prop:DELE_Std +
                     (1 | Part_ID) + (1 | Item),
                   data = Aggregate_Bilingual)

summary(Bilingual)
