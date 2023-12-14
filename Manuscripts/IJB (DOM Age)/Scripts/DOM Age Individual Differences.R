library(tidyverse)
library(dplyr)


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


# Plot individual differences
## Grouping
SCT_Group <- SCT %>%
  filter(!is.na(DOM_Use)) %>%
  group_by(Part_ID, Group) %>%
  summarize(DOM_Production = sum(DOM_Use))

MST_Group <- MST %>%
  filter(!is.na(DOM_Use)) %>%
  group_by(Part_ID, Group) %>%
  summarize(DOM_Selection = sum(DOM_Use))

Aggregate = left_join(SCT_Group, MST_Group, by = "Part_ID", "Group") %>%
  rename(Group = Group.x) %>% 
  mutate(Total = DOM_Production + DOM_Selection)
Aggregate$Group <- factor(Aggregate$Group, levels = c("HSA", "HS7/8", "HS5"))


# Plot individual differences
## Whole group
Aggregate %>% 
  ggplot(aes(x = DOM_Production, y = DOM_Selection)) +
  geom_jitter(mapping = aes(color = Group)) +
  scale_x_continuous(breaks = seq (0, 10, 2),
                     limits = c(-1, 11)) +
  scale_y_continuous(breaks = seq (0, 8, 2),
                     limits = c(-1, 9)) +
  labs(x = "Sentences with DOM produced in SCT", y = "Sentences with DOM selected in MST", title = "HS' Individual Rates of DOM Selection and Production", color = "Group") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold", size = 10))


# Descriptive statistics
DOM_Production <- Aggregate %>% 
  group_by(Group) %>% 
  summarize(Mean = mean(DOM_Production), SD = sd(DOM_Production))

DOM_Selection <- Aggregate %>% 
  group_by(Group) %>% 
  summarize(Mean = mean(DOM_Selection), SD = sd(DOM_Selection))
