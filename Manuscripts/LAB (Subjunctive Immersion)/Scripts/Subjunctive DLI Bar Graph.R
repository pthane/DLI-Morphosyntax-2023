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


# Generate aggregate levels
## Create individual values
MLS5_EPT_Structure <- aggregate(MLS5_EPT$Mood_Use, list(MLS5_EPT$Structure), FUN = mean, na.rm = TRUE)
MLS5_EPT_Structure <- MLS5_EPT_Structure %>% rename(Structure_Avg = x)
MLS5_EPT_Structure <- left_join(MLS5_EPT, MLS5_EPT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

MLS5_FCT_Structure <- aggregate(MLS5_FCT$Mood_Use, list(MLS5_FCT$Structure), FUN = mean, na.rm = TRUE)
MLS5_FCT_Structure <- MLS5_FCT_Structure %>% rename(Structure_Avg = x)
MLS5_FCT_Structure <- left_join(MLS5_FCT, MLS5_FCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

DLI5_EPT_Structure <- aggregate(DLI5_EPT$Mood_Use, list(DLI5_EPT$Structure), FUN = mean, na.rm = TRUE)
DLI5_EPT_Structure <- DLI5_EPT_Structure %>% rename(Structure_Avg = x)
DLI5_EPT_Structure <- left_join(DLI5_EPT, DLI5_EPT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

DLI5_FCT_Structure <- aggregate(DLI5_FCT$Mood_Use, list(DLI5_FCT$Structure), FUN = mean, na.rm = TRUE)
DLI5_FCT_Structure <- DLI5_FCT_Structure %>% rename(Structure_Avg = x)
DLI5_FCT_Structure <- left_join(DLI5_FCT, DLI5_FCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

DLI78_EPT_Structure <- aggregate(DLI78_EPT$Mood_Use, list(DLI78_EPT$Structure), FUN = mean, na.rm = TRUE)
DLI78_EPT_Structure <- DLI78_EPT_Structure %>% rename(Structure_Avg = x)
DLI78_EPT_Structure <- left_join(DLI78_EPT, DLI78_EPT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

DLI78_FCT_Structure <- aggregate(DLI78_FCT$Mood_Use, list(DLI78_FCT$Structure), FUN = mean, na.rm = TRUE)
DLI78_FCT_Structure <- DLI78_FCT_Structure %>% rename(Structure_Avg = x)
DLI78_FCT_Structure <- left_join(DLI78_FCT, DLI78_FCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

MLS78_EPT_Structure <- aggregate(MLS78_EPT$Mood_Use, list(MLS78_EPT$Structure), FUN = mean, na.rm = TRUE)
MLS78_EPT_Structure <- MLS78_EPT_Structure %>% rename(Structure_Avg = x)
MLS78_EPT_Structure <- left_join(MLS78_EPT, MLS78_EPT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

MLS78_FCT_Structure <- aggregate(MLS78_FCT$Mood_Use, list(MLS78_FCT$Structure), FUN = mean, na.rm = TRUE)
MLS78_FCT_Structure <- MLS78_FCT_Structure %>% rename(Structure_Avg = x)
MLS78_FCT_Structure <- left_join(MLS78_FCT, MLS78_FCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

SDB_EPT_Structure <- aggregate(SDB_EPT$Mood_Use, list(SDB_EPT$Structure), FUN = mean, na.rm = TRUE)
SDB_EPT_Structure <- SDB_EPT_Structure %>% rename(Structure_Avg = x)
SDB_EPT_Structure <- left_join(SDB_EPT, SDB_EPT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

SDB_FCT_Structure <- aggregate(SDB_FCT$Mood_Use, list(SDB_FCT$Structure), FUN = mean, na.rm = TRUE)
SDB_FCT_Structure <- SDB_FCT_Structure %>% rename(Structure_Avg = x)
SDB_FCT_Structure <- left_join(SDB_FCT, SDB_FCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

EPT_Structure <- rbind(DLI5_EPT_Structure, MLS5_EPT_Structure, DLI78_EPT_Structure, MLS78_EPT_Structure, SDB_EPT_Structure)
FCT_Structure <- rbind(DLI5_FCT_Structure, MLS5_FCT_Structure, DLI78_FCT_Structure, MLS78_FCT_Structure, SDB_FCT_Structure)

Subj_Percentage <- rbind(EPT_Structure, FCT_Structure)
Subj_Percentage$Group <- factor(Subj_Percentage$Group, levels = c("DLI-5", "MLE-5", "DLI-7/8", "MLE-7/8", "SDBA"))
Subj_Percentage$Task <- factor(Subj_Percentage$Task, levels = c("Production", "Selection"))

# Generate bar graph
Subj_Percentage %>% 
  ggplot(aes(x = Group, y = Structure_Percentage, fill = Task)) + 
  geom_bar(position = "dodge", color = "black", stat = "identity") +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 105)) +
  geom_text(aes(label = round(Structure_Percentage)),
            position = position_dodge(width = .9),
            vjust = -0.5,
            size = 3) +
  scale_fill_manual(values = c("#56B4E9", "#F0E442")) +
  labs(x = "Group", y = "Percentage of subjunctive responses", fill = "Task", title = "Subjunctive by Group and Task") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"))

