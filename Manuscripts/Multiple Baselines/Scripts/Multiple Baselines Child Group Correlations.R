library(tidyverse)
library(here)
library(lme4)
library(lmerTest)
library(emmeans)


options(scipen = 99)


# Create dataframes
## Load CSVs
DOM_EPT <- read_csv(here("Manuscripts", "Multiple Baselines", "Data", "Multiple Baselines DOM Production.csv")) %>% 
  mutate(Structure = "DOM") %>% 
  select(!Mood)

DOM_FCT <- read_csv(here("Manuscripts", "Multiple Baselines", "Data", "Multiple Baselines DOM Selection.csv")) %>% 
  mutate(Structure = "DOM") %>% 
  select(!Mood)

Subjunctive_EPT <- read_csv(here("Manuscripts", "Multiple Baselines", "Data", "Multiple Baselines Subjunctive Production.csv")) %>% 
  mutate(Structure = "Subjunctive")

Subjunctive_FCT <- read_csv(here("Manuscripts", "Multiple Baselines", "Data", "Multiple Baselines Subjunctive Selection.csv")) %>% 
  mutate(Structure = "Subjunctive")


## Join dataframes
DOM <- rbind(DOM_EPT, DOM_FCT) %>% 
  filter(!Group == "Adults")
DOM$Group <- factor(DOM$Group, levels = c("DLBE-7/8", "MLS-7/8", "DLBE-5", "MLS-5"))

Subjunctive <- rbindDOM_EPTSubjunctive <- rbind(Subjunctive_EPT, Subjunctive_FCT) %>% 
  filter(!Group == "Adults")
Subjunctive$Group <- factor(Subjunctive$Group, levels = c("DLBE-7/8", "MLS-7/8", "DLBE-5", "MLS-5"))


Master <- rbind(Subjunctive, DOM)
Master$Group <- factor(Master$Group, levels = c("DLBE-7/8", "MLS-7/8", "DLBE-5", "MLS-5"))


# Correlation for DOM
## GLMM
Correlation_DOM_Children <- glmer(Response ~ Age + School + Task +
                             (1 | Part_ID) + (1 | Item),
                           family = "binomial",
                           data = DOM)

summary(Correlation_DOM_Children)


# Correlation for subjunctive
## GLMM
Correlation_Subjunctive_Children <- glmer(Response ~ Age + School + Task +
                                   (1 | Part_ID) + (1 | Item),
                                 family = "binomial",
                                 data = Subjunctive)

summary(Correlation_Subjunctive_Children)


# Correlation for composite
## GLMM
Correlation_Composite_Children <- glmer(Response ~ Age + School + Task +
                                 (1 | Part_ID) + (1 | Item),
                               family = "binomial",
                               data = Master)

summary(Correlation_Composite_Children)


# Post-hoc DOM correlation: education and task
## Generate main model
Correlation_DOM_Interaction <- glmer(Response ~ School * Task +
                                       (1 | Part_ID) + (1 | Item),
                                     family = "binomial",
                                     data = DOM)

summary(Correlation_DOM_Interaction)


## Post-hoc comparisons
Post_Hoc <- emmeans(Correlation_DOM_Interaction, ~ School * Task, type = "link")

summary(Post_Hoc)
pairs(Post_Hoc, by = "Task")
