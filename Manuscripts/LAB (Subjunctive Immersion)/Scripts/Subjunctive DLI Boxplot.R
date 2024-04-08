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
EPT <- rbind(SDB_EPT, DLI78_EPT, MLS78_EPT, DLI5_EPT, MLS5_EPT) %>% 
  group_by(Part_ID, Group) %>%
  summarize(Total_Subj = sum(Mood_Use, na.rm = TRUE),
            Total_Responses = sum(!is.na(Mood_Use)),
            Ratio = Total_Subj/Total_Responses) %>% 
  mutate(Task = "Production",
         Ratio = (Ratio*100)/12.5)

FCT <- rbind(SDB_FCT, DLI78_FCT, MLS78_FCT, DLI5_FCT, MLS5_FCT) %>% 
  group_by(Part_ID, Group) %>%
  summarize(Total_Subj = sum(Mood_Use, na.rm = TRUE),
            Total_Responses = sum(!is.na(Mood_Use)),
            Ratio = Total_Subj/Total_Responses) %>% 
  mutate(Task = "Selection",
         Ratio = (Ratio*100)/12.5)

Master <- rbind(EPT, FCT)


# Plot graph
# Generate graph
Master %>% 
  ggplot(aes(x = Group, y = Ratio, fill = Task)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq (0, 8, 1),
                     limits = c(0, 8)) +
  scale_fill_manual(values = c("#56B4E9", "#F0E442")) +
  labs(x = "Group", y = "Number of subjunctive responses", title = "Subjunctive by Group and Task") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"))
