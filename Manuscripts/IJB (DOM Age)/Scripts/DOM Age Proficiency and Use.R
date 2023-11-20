library(tidyverse)
library(lme4)
library(lmerTest)

options(scipen = 99)

# Load data
## EPT
HSA_SCT <- read_csv("./CSV Files/Adult HS/Adult HS DOM EPT.csv") %>%
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>%
  mutate(Group = "HSA",
         Task = "SCT")

DLI78_SCT <- read_csv("./CSV Files/DLI-78/DLI-78 DOM EPT.csv") %>%
  mutate(Group = "HS7/8",
         Task = "SCT")

MLS78_SCT <- read_csv("./CSV Files/MLS-78/MLS-78 DOM EPT.csv") %>%
  mutate(Group = "HS7/8",
         Task = "SCT")

DLI5_SCT <- read_csv("./CSV Files/DLI-5/DLI-5 DOM EPT.csv")  %>%
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>%
  mutate(Group = "HS5",
         Task = "SCT")

MLS5_SCT <- read_csv("./CSV Files/MLS-5/MLS-5 DOM EPT.csv") %>%
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>%
  mutate(Group = "HS5",
         Task = "SCT")


## FCT
HSA_MST <- read_csv("./CSV Files/Adult HS/Adult HS DOM FCT.csv") %>%
  mutate(Group = "HSA",
         Task = "MST")

DLI78_MST <- read_csv("./CSV Files/DLI-78/DLI-78 DOM FCT.csv") %>% 
  mutate(Group = "HS7/8",
         Task = "MST")

MLS78_MST <- read_csv("./CSV Files/MLS-78/MLS-78 DOM FCT.csv") %>% 
  mutate(Group = "HS7/8",
         Task = "MST")

DLI5_MST <- read_csv("./CSV Files/DLI-5/DLI-5 DOM FCT.csv")  %>%
  mutate(Group = "HS5",
         Task = "MST")

MLS5_MST <- read_csv("./CSV Files/MLS-5/MLS-5 DOM FCT.csv") %>%
  mutate(Group = "HS5",
         Task = "MST")

## Rejoin datasets
SCT <- rbind(HSA_SCT, DLI78_SCT, MLS78_SCT, DLI5_SCT, MLS5_SCT)
MST <- rbind(HSA_MST, DLI78_MST, MLS78_MST, DLI5_MST, MLS5_MST)


# Calculate sum of subjunctive responses by participant
SCT_Part_Avg <- aggregate(SCT$DOM_Use, list(SCT$Part_ID), FUN = sum, na.rm = TRUE)
SCT_Part_Avg <- SCT_Part_Avg %>% rename(Part_Avg = x)
SCT_Part_Avg <- left_join(SCT, SCT_Part_Avg, by = c("Part_ID" = "Group.1")) %>% 
  filter(Item == "EPT-02")

MST_Part_Avg <- aggregate(MST$DOM_Use, list(MST$Part_ID), FUN = sum, na.rm = TRUE)
MST_Part_Avg <- MST_Part_Avg %>% rename(Part_Avg = x)
MST_Part_Avg <- left_join(MST, MST_Part_Avg, by = c("Part_ID" = "Group.1")) %>% 
  filter(Item == "FCT-03")

Aggregate <- rbind(SCT_Part_Avg, MST_Part_Avg)
Aggregate$Group <- factor(Aggregate$Group, levels = c("HSA", "HS7/8", "HS5"))
Aggregate$Task <- factor(Aggregate$Task, levels = c("SCT", "MST"))


# Generate graphs
## BESA Proficiency
Aggregate %>% 
  ggplot(aes(x = BESA_Total, y = Part_Avg)) + 
  geom_jitter(mapping = aes(color = Group)) +
  geom_smooth(method = glm) +
  facet_grid(rows = vars(Task)) +
  scale_y_continuous(breaks = seq (0, 10, 2),
                     limits = c(-1, 11)) +
  scale_x_continuous(breaks = seq (0, 18, 3),
                     limits = c(-1, 19)) +
  labs(x = "BESA score", y = "Number of responses with DOM", 
       title = "DOM by BESA Proficiency and Task") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"))


## Frequency of use
Aggregate %>% 
  ggplot(aes(x = Use_Joint, y = Part_Avg)) + 
  geom_jitter(mapping = aes(color = Group)) +
  geom_smooth(method = glm) +
  facet_grid(rows = vars(Task)) +
  scale_y_continuous(breaks = seq (0, 10, 2),
                     limits = c(-1, 11)) +
  scale_x_continuous(breaks = seq (0, 30, 5),
                     limits = c(-1, 31)) +
  labs(x = "Frequency of use of Spanish", y = "Number of responses with DOM", 
       title = "DOM by Current Frequency of Use and Task") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"))
