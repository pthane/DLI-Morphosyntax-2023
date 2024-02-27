library(tidyverse)

# Load data
## EPT
SDB_EPT <- read_csv("./CSV Files/SDB/SDB Subjunctive EPT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "SDBA",
         Task = "Production")

DLI78_EPT <- read_csv("./CSV Files/DLI-78/DLI-78 Subjunctive EPT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "DLI-7/8",
         Task = "Production")

MLS78_EPT <- read_csv("./CSV Files/MLS-78/MLS-78 Subjunctive EPT.csv") %>%
  filter(!School == "GBCS") %>% 
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "MLE-7/8",
         Task = "Production")

DLI5_EPT <- read_csv("./CSV Files/DLI-5/DLI-5 Subjunctive EPT.csv")  %>%
  filter(Property == "Intensional subjunctive")%>% 
  mutate(Group = "DLI-5",
         Task = "Production")

MLS5_EPT <- read_csv("./CSV Files/MLS-5/MLS-5 Subjunctive EPT.csv") %>%
  filter(!School == "GBCS") %>% 
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "MLE-5",
         Task = "Production")


## FCT
SDB_FCT <- read_csv("./CSV Files/SDB/SDB Subjunctive FCT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "SDBA",
         Task = "Selection")

DLI78_FCT <- read_csv("./CSV Files/DLI-78/DLI-78 Subjunctive FCT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "DLI-7/8",
         Task = "Selection")

MLS78_FCT <- read_csv("./CSV Files/MLS-78/MLS-78 Subjunctive FCT.csv") %>%
  filter(!School == "GBCS") %>% 
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "MLE-7/8",
         Task = "Selection")

DLI5_FCT <- read_csv("./CSV Files/DLI-5/DLI-5 Subjunctive FCT.csv")  %>%
  filter(Property == "Intensional subjunctive")%>% 
  mutate(Group = "DLI-5",
         Task = "Selection")

MLS5_FCT <- read_csv("./CSV Files/MLS-5/MLS-5 Subjunctive FCT.csv") %>%
  filter(!School == "GBCS") %>% 
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "MLE-5",
         Task = "Selection")


## Rejoin datasets for all participants
EPT <- rbind(SDB_EPT, DLI78_EPT, MLS78_EPT, DLI5_EPT, MLS5_EPT)
FCT <- rbind(SDB_FCT, DLI78_FCT, MLS78_FCT, DLI5_FCT, MLS5_FCT)


# Generate averages
EPT_Average <- aggregate(EPT$Mood_Use, list(EPT$Part_ID), FUN = sum, na.rm = TRUE)
EPT_Average <- EPT_Average %>% rename(Subj_Sum = x)
EPT_Average <- left_join(EPT, EPT_Average, by = c("Part_ID" = "Group.1")) %>% 
  mutate(Subj_Percentage = (Subj_Sum/8)*100)

FCT_Average <- aggregate(FCT$Mood_Use, list(FCT$Part_ID), FUN = sum, na.rm = TRUE)
FCT_Average <- FCT_Average %>% rename(Subj_Sum = x)
FCT_Average <- left_join(FCT, FCT_Average, by = c("Part_ID" = "Group.1")) %>% 
  mutate(Subj_Percentage = (Subj_Sum/8)*100)


# Join dataset
Aggregate <- rbind(EPT_Average, FCT_Average)
Aggregate$Group <- factor(Aggregate$Group, levels = c("DLI-5", "MLE-5", "DLI-7/8", "MLE-7/8", "SDBA"))
Aggregate$Task <- factor(Aggregate$Task, levels = c("Production", "Selection"))


# Generate graph
Aggregate %>% 
  ggplot(aes(x = Group, y = Subj_Sum, fill = Task)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq (0, 8, 2),
                     limits = c(0, 8)) +
  scale_fill_manual(values = c("#56B4E9", "#F0E442")) +
  labs(x = "Group", y = "Number of subjunctive responses", title = "Subjunctive by Group and Task") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"))

