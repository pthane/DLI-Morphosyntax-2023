library(tidyverse)
library(here)
library(lme4)
library(lmerTest)
library(emmeans)


options(scipen = 99)


# Create dataframes
## Load CSVs
DOM_EPT <- read_csv(here("Manuscripts", "SLR (Multiple Baselines)", "Data", "SLR Multiple Baselines DOM Production.csv")) %>% 
  mutate(Structure = "DOM") %>% 
  select(!Mood) %>% 
  drop_na(DOM_Use)
DOM_EPT$Group <- factor(DOM_EPT$Group, levels = c("Adults", "DLBE-7/8", "MLS-7/8", "DLBE-5", "MLS-5"))

DOM_FCT <- read_csv(here("Manuscripts", "SLR (Multiple Baselines)", "Data", "SLR Multiple Baselines DOM Selection.csv")) %>% 
  mutate(Structure = "DOM") %>% 
  select(!Mood) %>% 
  drop_na(DOM_Use)


## Join dataframes
DOM <- rbind(DOM_EPT, DOM_FCT)
DOM$Group <- factor(DOM$Group, levels = c("Adults", "DLBE-7/8", "MLS-7/8", "DLBE-5", "MLS-5"))


## Create child-only dataframe
DOM_Children <- DOM %>% 
  filter(!Group == "Adults")

DOM_Children$Group <- factor(DOM_Children$Group, levels = c("DLBE-7/8", "MLS-7/8", "DLBE-5", "MLS-5"))


# Adult correlations
## GLMM
Correlation_DOM_Adults <- glmer(Response ~ Group + Use_Std + Task + Use_Std:Task +
                                  (1 | Part_ID) + (1 | Item),
                                family = "binomial",
                                data = DOM,
                                control=glmerControl(optimizer="bobyqa",
                                                     optCtrl=list(maxfun=100000)))

summary(Correlation_DOM_Adults)


## Post-hoc comparisons
Pairwise_DOM_Adults <- emmeans(Correlation_DOM_Adults, spec = "Group")
Tukey_DOM_Adults <- contrast(Pairwise_DOM_Adults, method = "pairwise")

summary(Tukey_DOM_Adults)


# Child correlations
## GLMM
Correlation_DOM_Children <- glmer(Response ~ School + Age + Use_Std + Task + School:Use_Std + School:Task + Use_Std:Task +
                                    (1 | Part_ID) + (1 | Item),
                                  family = "binomial",
                                  data = DOM_Children)

summary(Correlation_DOM_Children)


## Post-hoc analysis of interaction
Correlation_DOM_Children_Post_Hoc <- emmeans(Correlation_DOM_Children, ~ School * Task, type = "link")

summary(Correlation_DOM_Children_Post_Hoc)
pairs(Correlation_DOM_Children_Post_Hoc, by = "Task")


# Argue against mood effect for DOM
Correlation_DOM_Mood <- glmer(Response ~ Property +
                                (1 | Part_ID) + (1 | Item),
                              family = "binomial",
                              data = DOM_EPT)

summary(Correlation_DOM_Mood)


# Final three-way interaction with children
Correlation_DOM_School_Use <- glmer(Response ~ School * Use_Std * Task +
                                      (1 | Part_ID) + (1 | Item),
                                    family = "binomial",
                                    data = DOM,
                                    control=glmerControl(optimizer="bobyqa",
                                                         optCtrl=list(maxfun=100000)))

summary(Correlation_DOM_School_Use)
