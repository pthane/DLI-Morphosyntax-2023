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
  select(!Mood) %>% 
  drop_na(DOM_Use)

DOM_FCT <- read_csv(here("Manuscripts", "Multiple Baselines", "Data", "Multiple Baselines DOM Selection.csv")) %>% 
  mutate(Structure = "DOM") %>% 
  select(!Mood) %>% 
  drop_na(DOM_Use)


## Join dataframes
DOM <- rbind(DOM_EPT, DOM_FCT)
DOM$Group <- factor(DOM$Group, levels = c("Adults", "DLBE-7/8", "MLS-7/8", "DLBE-5", "MLS-5"))


# Correlation for DOM
## GLMM
Correlation_DOM <- glmer(Response ~ Group + Task +
                             (1 | Part_ID) + (1 | Item),
                           family = "binomial",
                           data = DOM)

summary(Correlation_DOM)


## Post-hoc comparisons
Pairwise_DOM <- emmeans(Correlation_DOM, spec = "Group")
Tukey_DOM <- contrast(Pairwise_DOM, method = "pairwise")

summary(Tukey_DOM)


## Confirm no mood effect for DOM
DOM_Mood_Correlation <- glmer(Response ~ Property +
                                (1 | Part_ID) + (1 | Item),
                              family = "binomial",
                              data = DOM_EPT)

summary(DOM_Mood_Correlation)
