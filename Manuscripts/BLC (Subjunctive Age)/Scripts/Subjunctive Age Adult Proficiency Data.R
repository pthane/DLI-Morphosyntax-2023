library(tidyverse)
library(lme4)
library(lmerTest)
library(sjPlot)
library(here)

options(scipen = 999)


# Load data
HSA_EPT <- read_csv(here("./CSV Files/Adult HS/Adult HS Subjunctive EPT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HSA",
         DELE_Std = (DELE - mean(DELE))/sd(DELE))

HSA_FCT <- read_csv(here("./CSV Files/Adult HS/Adult HS Subjunctive FCT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HSA",
         DELE_Std = (DELE - mean(DELE))/sd(DELE))

Aggregate <- rbind(HSA_EPT, HSA_FCT)


# Generate participant averages
## Calculate sum of subjunctive responses by participant
EPT_Part_Avg <- aggregate(HSA_EPT$Mood_Use, list(HSA_EPT$Part_ID), FUN = sum, na.rm = TRUE)
EPT_Part_Avg <- EPT_Part_Avg %>% rename(Part_Avg = x)
EPT_Part_Avg <- left_join(HSA_EPT, EPT_Part_Avg, by = c("Part_ID" = "Group.1")) %>% 
  filter(Item == "EPT-02")

FCT_Part_Avg <- aggregate(HSA_FCT$Mood_Use, list(HSA_FCT$Part_ID), FUN = sum, na.rm = TRUE)
FCT_Part_Avg <- FCT_Part_Avg %>% rename(Part_Avg = x)
FCT_Part_Avg <- left_join(HSA_FCT, FCT_Part_Avg, by = c("Part_ID" = "Group.1")) %>% 
  filter(Item == "FCT-02")

## Rejoin datasets
Total <- rbind(EPT_Part_Avg, FCT_Part_Avg)


# DELE/subjunctive correlation
## Statistical model
DELE_Correlation <- glmer(Mood_Use ~ DELE_Std * Task +
                           (1 | Part_ID) + (1 | Item),
                         data = Aggregate,
                         family = "binomial")

summary(DELE_Correlation)
confint(DELE_Correlation)

plot_model(DELE_Correlation, show.values = TRUE, show.intercept = TRUE, value.offset = .3, transform = NULL, vline.color = "black") +
  scale_y_continuous(breaks = seq(-3, 3, 1.5),
                     limits = c(-3, 3)) +
  labs(title = "Summary of Adult Proficiency GLMM Model", y = "Log odds") +
  scale_x_discrete(labels = c("DELE score : FCT", "FCT", "DELE score", "(Intercept)")) +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"))


## Plot
Total %>% 
  ggplot(aes(x = DELE, y = Part_Avg)) + 
  geom_jitter() +
  geom_smooth(method = glm) +
  facet_grid(rows = vars(Task)) +
  scale_y_continuous(breaks = seq (0, 8, 2),
                     limits = c(-1, 9)) +
  scale_x_continuous(breaks = seq (15, 50, 5),
                     limits = c(15, 50)) +
  labs(x = "DELE score", y = "Number of subjunctive responses per participant", 
       title = "Subjunctive by DELE Score and Task for Adult HS") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"))


# DELE/BESA correlation
## Generate GLM
DELE_BESA <- lm(DELE_Std ~ BESA_Other_Std,
                 data = Total)

summary(DELE_BESA)


## Plot relationship
EPT_Part_Avg %>% 
  ggplot(aes(x = DELE, y = BESA_Other)) +
  geom_jitter() +
  geom_smooth(method = glm) +
  scale_y_continuous(breaks = seq (0, 14, 2),
                     limits = c(-1, 15)) +
  scale_x_continuous(breaks = seq (15, 50, 5),
                     limits = c(15, 50)) +
  labs(x = "DELE score", y = "BESA score", 
       title = "Correlations between DELE and BESA") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"))
