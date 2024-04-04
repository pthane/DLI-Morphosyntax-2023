library(tidyverse)

# Load data
## Production
SDB_EPT <- read_csv("./CSV Files/SDB/SDB DOM EPT.csv") %>% 
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>% 
  mutate(Group = "SDBA",
         Task = "SCT")

DLI78_EPT <- read_csv("./CSV Files/DLI-78/DLI-78 DOM EPT.csv") %>% 
  mutate(Group = "BES-7/8",
         Task = "SCT",
         School = "Immersion",
         Age = "7th/8th")

MLS78_EPT <- read_csv("./CSV Files/MLS-78/MLS-78 DOM EPT.csv") %>%
  filter(!School == "GBCS") %>% 
  mutate(Group = "ME-7/8",
         Task = "SCT",
         School = "Monolingual",
         Age = "7th/8th")

DLI5_EPT <- read_csv("./CSV Files/DLI-5/DLI-5 DOM EPT.csv") %>% 
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>% 
  mutate(Group = "BES-5",
         Task = "SCT",
         School = "Immersion",
         Age = "5th")

MLS5_EPT <- read_csv("./CSV Files/MLS-5/MLS-5 DOM EPT.csv") %>%
  filter(!School == "GBCS") %>% 
  mutate(Group = "ME-5",
         Task = "SCT",
         School = "Monolingual",
         Age = "5th")


# Selection
SDB_FCT <- read_csv("./CSV Files/SDB/SDB DOM FCT.csv") %>% 
  mutate(Group = "SDBA",
         Task = "MST")

DLI78_FCT <- read_csv("./CSV Files/DLI-78/DLI-78 DOM FCT.csv") %>% 
  mutate(Group = "BES-7/8",
         Task = "MST",
         School = "Immersion",
         Age = "7th/8th")

MLS78_FCT <- read_csv("./CSV Files/MLS-78/MLS-78 DOM FCT.csv") %>%
  filter(!School == "GBCS") %>% 
  mutate(Group = "ME-7/8",
         Task = "MST",
         School = "Monolingual",
         Age = "7th/8th")

DLI5_FCT <- read_csv("./CSV Files/DLI-5/DLI-5 DOM FCT.csv") %>% 
  mutate(Group = "BES-5",
         Task = "MST",
         School = "Immersion",
         Age = "5th")

MLS5_FCT <- read_csv("./CSV Files/MLS-5/MLS-5 DOM FCT.csv") %>%
  filter(!School == "GBCS") %>% 
  mutate(Group = "ME-5",
         Task = "MST",
         School = "Monolingual",
         Age = "5th")


## Rejoin datasets for all participants
EPT <- rbind(SDB_EPT, DLI78_EPT, MLS78_EPT, DLI5_EPT, MLS5_EPT)
FCT <- rbind(SDB_FCT, DLI78_FCT, MLS78_FCT, DLI5_FCT, MLS5_FCT)


# Generate averages
EPT_Average <- aggregate(EPT$DOM_Use, list(EPT$Part_ID), FUN = sum, na.rm = TRUE)
EPT_Average <- EPT_Average %>% rename(DOM_Sum = x)
EPT_Average <- left_join(EPT, EPT_Average, by = c("Part_ID" = "Group.1")) %>% 
  mutate(DOM_Percentage = (DOM_Sum/10)*100)

FCT_Average <- aggregate(FCT$DOM_Use, list(FCT$Part_ID), FUN = sum, na.rm = TRUE)
FCT_Average <- FCT_Average %>% rename(DOM_Sum = x)
FCT_Average <- left_join(FCT, FCT_Average, by = c("Part_ID" = "Group.1")) %>% 
  mutate(DOM_Percentage = (DOM_Sum/8)*100)


# Join dataset
Aggregate <- rbind(EPT_Average, FCT_Average)
Aggregate$Group <- factor(Aggregate$Group, levels = c("SDBA", "BES-7/8", "ME-7/8", "BES-5", "ME-5"))
Aggregate$Task <- factor(Aggregate$Task, levels = c("SCT", "MST"))


# Generate graph
Aggregate %>% 
  ggplot(aes(x = Group, y = DOM_Percentage, fill = Task)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 100)) +
  scale_fill_manual(values = c("#56B4E9", "#F0E442")) +
  labs(x = "Group", y = "Percentage of DOM responses", title = "DOM by Group and Task") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"))

