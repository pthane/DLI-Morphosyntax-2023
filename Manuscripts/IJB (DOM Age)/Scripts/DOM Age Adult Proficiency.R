library(tidyverse)
library(lme4)
library(lmerTest)
library(sjPlot)
library

options(scipen = 99)

# Load data
## Original dataframes
HSA_SCT <- read_csv(here("./CSV Files/Adult HS/Adult HS DOM EPT.csv")) %>%
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>% 
  mutate(Group = "HSA",
         Task = "SCT")

HSA_MST <- read_csv(here("./CSV Files/Adult HS/Adult HS DOM FCT.csv")) %>%
  mutate(Group = "HSA",
         Task = "MST")


## Rejoin datasets
Adult_Aggregate <- rbind(HSA_SCT, HSA_MST)
Adult_Aggregate$Task <- factor(Adult_Aggregate$Task, levels = c("SCT", "MST"))


# Run DELE correlation
DELE_GLMM <- glmer(DOM_Use ~ 1 + DELE_Std +
                     (1 | Part_ID) + (1 | Item),
                   data = Adult_Aggregate,
                   family = "binomial")

summary(DELE_GLMM)


# Calculate sum of subjunctive responses by participant
SCT_Part_Avg <- aggregate(HSA_SCT$DOM_Use, list(HSA_SCT$Part_ID), FUN = sum, na.rm = TRUE)
SCT_Part_Avg <- SCT_Part_Avg %>% rename(Part_Avg = x)
SCT_Part_Avg <- left_join(HSA_SCT, SCT_Part_Avg, by = c("Part_ID" = "Group.1")) %>% 
  filter(Item == "EPT-02")

MST_Part_Avg <- aggregate(HSA_MST$DOM_Use, list(HSA_MST$Part_ID), FUN = sum, na.rm = TRUE)
MST_Part_Avg <- MST_Part_Avg %>% rename(Part_Avg = x)
MST_Part_Avg <- left_join(HSA_MST, MST_Part_Avg, by = c("Part_ID" = "Group.1")) %>% 
  filter(Item == "FCT-03")

Proficiency_Graph <- rbind(SCT_Part_Avg, MST_Part_Avg)
Proficiency_Graph$Task <- factor(Proficiency_Graph$Task, levels = c("SCT", "MST"))


# Generate graph
Proficiency_Graph %>% 
  ggplot(aes(x = DELE, y = Part_Avg)) + 
  geom_jitter() +
  geom_smooth(method = glm) +
  facet_grid(rows = vars(Task)) +
  scale_y_continuous(breaks = seq (0, 10, 2),
                     limits = c(-1, 11)) +
  scale_x_continuous(breaks = seq (15, 50, 5),
                     limits = c(15, 50)) +
  labs(x = "DELE score", y = "Number of responses with DOM", 
       title = "DOM by DELE Score and Task for Adult HS") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"))
