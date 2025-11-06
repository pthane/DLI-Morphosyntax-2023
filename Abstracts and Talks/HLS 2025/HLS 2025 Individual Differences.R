library(tidyverse)
library(here)
library(patchwork)


# Load data
## Production
DLI78_EPT <- read_csv(here("./CSV Files/DLI-78/DLI-78 DOM EPT.csv")) %>% 
  mutate(Group = "DLI-7/8",
         Task = "Production")

MLS78_EPT <- read_csv(here("./CSV Files/MLS-78/MLS-78 DOM EPT.csv")) %>%
  filter(!School == "GBCS") %>% 
  mutate(Group = "MLS-7/8",
         Task = "Production")

DLI5_EPT <- read_csv(here("./CSV Files/DLI-5/DLI-5 DOM EPT.csv")) %>% 
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>% 
  mutate(Group = "DLI-5",
         Task = "Production")

MLS5_EPT <- read_csv(here("./CSV Files/MLS-5/MLS-5 DOM EPT.csv")) %>%
  filter(!School == "GBCS") %>% 
  mutate(Group = "MLS-5",
         Task = "Production")

HSA_EPT <- read_csv(here("./CSV Files/Adult HS/Adult HS DOM EPT.csv")) %>%
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>%
  mutate(Group = "Adults",
         Task = "Production")


## Selection
DLI78_FCT <- read_csv(here("./CSV Files/DLI-78/DLI-78 DOM FCT.csv")) %>% 
  mutate(Group = "DLI-7/8",
         Task = "Selection")

MLS78_FCT <- read_csv(here("./CSV Files/MLS-78/MLS-78 DOM FCT.csv")) %>%
  filter(!School == "GBCS") %>% 
  mutate(Group = "MLS-7/8",
         Task = "Selection")

DLI5_FCT <- read_csv(here("./CSV Files/DLI-5/DLI-5 DOM FCT.csv")) %>% 
  mutate(Group = "DLI-5",
         Task = "Selection")

MLS5_FCT <- read_csv(here("./CSV Files/MLS-5/MLS-5 DOM FCT.csv")) %>%
  filter(!School == "GBCS") %>% 
  mutate(Group = "MLS-5",
         Task = "Selection")

HSA_FCT <- read_csv(here("./CSV Files/Adult HS/Adult HS DOM FCT.csv")) %>%
  mutate(Group = "Adults",
         Task = "Selection")


# Join and tidy data
## Unify by task
Production <- rbind(DLI78_EPT, MLS78_EPT, DLI5_EPT, MLS5_EPT, HSA_EPT)
Selection <- rbind(DLI78_FCT, MLS78_FCT, DLI5_FCT, MLS5_FCT, HSA_FCT)


## Create dataframe
## Full grouping
Production_Dataframe <- Production %>%
  filter(!is.na(DOM_Use)) %>%
  group_by(Part_ID, Group) %>%
  summarize(DOM_Production = sum(DOM_Use))

Selection_Dataframe <- Selection %>%
  filter(!is.na(DOM_Use)) %>%
  group_by(Part_ID, Group) %>%
  summarize(DOM_Selection = sum(DOM_Use))

Full_Aggregate = left_join(Production_Dataframe, Selection_Dataframe, by = "Part_ID", "NewGroup") %>% 
  select(!Group.x) %>% 
  rename(Group = Group.y) %>% 
  mutate(Sum = (DOM_Production + DOM_Selection))
  
Full_Aggregate$Group <- factor(Full_Aggregate$Group, levels = c("MLS-5", "DLI-5", "MLS-7/8", "DLI-7/8", "Adults"))


# Plot individual differences
## Whole group with SDB
Ind_Diffs <- Full_Aggregate %>% 
  ggplot(aes(x = DOM_Production, y = DOM_Selection, color = Group)) +
  geom_jitter() +
  scale_x_continuous(breaks = seq (0, 10, 2),
                     limits = c(-0.5, 10.5)) +
  scale_y_continuous(breaks = seq (0, 8, 2),
                     limits = c(-0.5, 8.5)) +
  labs(x = "Sentences with DOM produced in production task", y = "Sentences with DOM selected in selection task", title = "Individual Rates of DOM Production and Selection", color = "Group") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold", size = 10))

Ind_Diffs

ggsave(filename = here("Abstracts and Talks", "HLS 2025", "HLS 2025 Individual Differences.pdf"),
       plot = Ind_Diffs,
       device = "pdf",
       width = 10,
       height = 6,
       units = "in")
