library(tidyverse)
library(here)


# Load data
## Production
SDB_EPT <- read_csv(here("./CSV Files/SDB/SDB DOM EPT.csv")) %>% 
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>% 
  mutate(Group = "SDBA",
         Task = "SCT")

DLI78_EPT <- read_csv(here("./CSV Files/DLI-78/DLI-78 DOM EPT.csv")) %>% 
  mutate(Group = "BES-7/8",
         Task = "SCT")

MLS78_EPT <- read_csv(here("./CSV Files/MLS-78/MLS-78 DOM EPT.csv")) %>%
  filter(!School == "GBCS") %>% 
  mutate(Group = "ME-7/8",
         Task = "SCT")

DLI5_EPT <- read_csv(here("./CSV Files/DLI-5/DLI-5 DOM EPT.csv")) %>% 
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>% 
  mutate(Group = "BES-5",
         Task = "SCT")

MLS5_EPT <- read_csv(here("./CSV Files/MLS-5/MLS-5 DOM EPT.csv")) %>%
  filter(!School == "GBCS") %>% 
  mutate(Group = "ME-5",
         Task = "SCT")

# Selection
SDB_FCT <- read_csv(here("./CSV Files/SDB/SDB DOM FCT.csv")) %>% 
  mutate(Group = "SDBA",
         Task = "MST")

DLI78_FCT <- read_csv(here("./CSV Files/DLI-78/DLI-78 DOM FCT.csv")) %>% 
  mutate(Group = "BES-7/8",
         Task = "MST")

MLS78_FCT <- read_csv(here("./CSV Files/MLS-78/MLS-78 DOM FCT.csv")) %>%
  filter(!School == "GBCS") %>% 
  mutate(Group = "ME-7/8",
         Task = "MST")

DLI5_FCT <- read_csv(here("./CSV Files/DLI-5/DLI-5 DOM FCT.csv")) %>% 
  mutate(Group = "BES-5",
         Task = "MST")

MLS5_FCT <- read_csv(here("./CSV Files/MLS-5/MLS-5 DOM FCT.csv")) %>%
  filter(!School == "GBCS") %>% 
  mutate(Group = "ME-5",
         Task = "MST")


# Rejoin data
Full_EPT <- rbind(SDB_EPT, DLI78_EPT, MLS78_EPT, DLI5_EPT, MLS5_EPT)
Full_FCT <- rbind(SDB_FCT, DLI78_FCT, MLS78_FCT, DLI5_FCT, MLS5_FCT)

Heritage_EPT <- rbind(DLI78_EPT, MLS78_EPT, DLI5_EPT, MLS5_EPT)
Heritage_FCT <- rbind(DLI78_FCT, MLS78_FCT, DLI5_FCT, MLS5_FCT)


# Create groups with individual differences
## Full grouping
Full_EPT_Group <- Full_EPT %>%
  filter(!is.na(DOM_Use)) %>%
  group_by(Part_ID, Group) %>%
  summarize(DOM_Production = sum(DOM_Use))

Full_FCT_Group <- Full_FCT %>%
  filter(!is.na(DOM_Use)) %>%
  group_by(Part_ID, Group) %>%
  summarize(DOM_Selection = sum(DOM_Use))

Full_Aggregate = left_join(Full_EPT_Group, Full_FCT_Group, by = "Part_ID", "NewGroup") %>% 
  mutate(Sum = (DOM_Production + DOM_Selection))
Full_Aggregate$Group.x <- factor(Full_Aggregate$Group.x, levels = c("SDBA", "BES-7/8", "ME-7/8", "BES-5", "ME-5"))


## Heritage only grouping
Heritage_EPT_Group <- Heritage_EPT %>%
  filter(!is.na(DOM_Use)) %>%
  group_by(Part_ID, Group) %>%
  summarize(DOM_Production = sum(DOM_Use))

Heritage_FCT_Group <- Heritage_FCT %>%
  filter(!is.na(DOM_Use)) %>%
  group_by(Part_ID, Group) %>%
  summarize(DOM_Selection = sum(DOM_Use))

Heritage_Aggregate = left_join(Heritage_EPT_Group, Heritage_FCT_Group, by = "Part_ID", "NewGroup") %>% 
  mutate(Sum = (DOM_Production + DOM_Selection))
Heritage_Aggregate$Group.x <- factor(Heritage_Aggregate$Group.x, levels = c("BES-7/8", "ME-7/8", "BES-5", "ME-5"))


## Summary of max/min HS (within SDBA range)
Max_HS <- Heritage_Aggregate %>% 
  filter(Sum > 15)

Min_HS <- Heritage_Aggregate %>% 
  filter(Sum == 1)


# Plot individual differences
## Whole group with SDB
Full_Aggregate %>% 
  ggplot(aes(x = DOM_Production, y = DOM_Selection, color = Group.x)) +
  geom_jitter() +
  scale_x_continuous(breaks = seq (0, 10, 2),
                     limits = c(-0.5, 10.5)) +
  scale_y_continuous(breaks = seq (0, 8, 2),
                     limits = c(-0.5, 8.5)) +
  labs(x = "Sentences with DOM produced in EPT", y = "Sentences with DOM selected in FCT", title = "Individual Rates of DOM Selection and Production", color = "Group") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold", size = 10))


## Whole group without SDB
Heritage_Aggregate %>% 
  ggplot(aes(x = DOM_Production, y = DOM_Selection, color = Group.x)) +
  geom_jitter() +
  scale_x_continuous(breaks = seq (0, 10, 2),
                     limits = c(-0.5, 10.5)) +
  scale_y_continuous(breaks = seq (0, 8, 2),
                     limits = c(-0.5, 8.5)) +
  labs(x = "Sentences with DOM produced in EPT", y = "Sentences with DOM selected in FCT", title = "Individual Rates of DOM Selection and Production", color = "Group") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold", size = 10))


## HS by group with labels
Heritage_Aggregate %>% 
  ggplot(aes(x = DOM_Production, y = DOM_Selection, color = Group.x)) +
  geom_jitter() +
  facet_wrap(facets = vars(Group.x)) +
  geom_smooth(method = glm) +
  scale_x_continuous(breaks = seq (0, 12, 2),
                     limits = c(-0.5, 12.5)) +
  scale_y_continuous(breaks = seq (0, 8, 2),
                     limits = c(-0.5, 8.5)) +
  labs(x = "Sentences with DOM produced in EPT", y = "Sentences with DOM selected in FCT", title = "Individual Rates of DOM Selection and Production", color = "Group") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold", size = 10))
