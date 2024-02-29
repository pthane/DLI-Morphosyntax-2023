library(tidyverse)


## Production
SDB_EPT <- read_csv("./CSV Files/SDB/SDB Subjunctive EPT.csv") %>% 
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "SDBA",
         Task = "Production")

DLI78_EPT <- read_csv("./CSV Files/DLI-78/DLI-78 Subjunctive EPT.csv") %>% 
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "DLI-7/8",
         Task = "Production",
         School = "Immersion",
         Age = "7th/8th")

MLS78_EPT <- read_csv("./CSV Files/MLS-78/MLS-78 Subjunctive EPT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  filter(!School == "GBCS") %>% 
  mutate(Group = "MES-7/8",
         Task = "Production",
         School = "Monolingual",
         Age = "7th/8th")

DLI5_EPT <- read_csv("./CSV Files/DLI-5/DLI-5 Subjunctive EPT.csv") %>% 
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "DLI-5",
         Task = "Production",
         School = "Immersion",
         Age = "5th")

MLS5_EPT <- read_csv("./CSV Files/MLS-5/MLS-5 Subjunctive EPT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  filter(!School == "GBCS") %>% 
  mutate(Group = "MES-5",
         Task = "Production",
         School = "Monolingual",
         Age = "5th")


# Selection
SDB_FCT <- read_csv("./CSV Files/SDB/SDB Subjunctive FCT.csv") %>% 
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "SDBA",
         Task = "Selection")

DLI78_FCT <- read_csv("./CSV Files/DLI-78/DLI-78 Subjunctive FCT.csv") %>% 
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "DLI-7/8",
         Task = "Selection",
         School = "Immersion",
         Age = "7th/8th")

MLS78_FCT <- read_csv("./CSV Files/MLS-78/MLS-78 Subjunctive FCT.csv") %>%
  filter(!School == "GBCS") %>% 
  mutate(Group = "MES-7/8",
         Task = "Selection",
         School = "Monolingual",
         Age = "7th/8th")

DLI5_FCT <- read_csv("./CSV Files/DLI-5/DLI-5 Subjunctive FCT.csv") %>% 
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "DLI-5",
         Task = "Selection",
         School = "Immersion",
         Age = "5th")

MLS5_FCT <- read_csv("./CSV Files/MLS-5/MLS-5 Subjunctive FCT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  filter(!School == "GBCS") %>% 
  mutate(Group = "MES-5",
         Task = "Selection",
         School = "Monolingual",
         Age = "5th")


# Generate aggregate levels
## Create individual values
SDB_EPT_Structure <- aggregate(SDB_EPT$Mood_Use, list(SDB_EPT$Structure), FUN = mean, na.rm = TRUE)
SDB_EPT_Structure <- SDB_EPT_Structure %>% rename(Structure_Avg = x)
SDB_EPT_Structure <- left_join(SDB_EPT, SDB_EPT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

SDB_FCT_Structure <- aggregate(SDB_FCT$Mood_Use, list(SDB_FCT$Structure), FUN = mean, na.rm = TRUE)
SDB_FCT_Structure <- SDB_FCT_Structure %>% rename(Structure_Avg = x)
SDB_FCT_Structure <- left_join(SDB_FCT, SDB_FCT_Structure, by = c("Structure" = "Group.1")) %>%  
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

DLI5_EPT_Structure <- aggregate(DLI5_EPT$Mood_Use, list(DLI5_EPT$Structure), FUN = mean, na.rm = TRUE)
DLI5_EPT_Structure <- DLI5_EPT_Structure %>% rename(Structure_Avg = x)
DLI5_EPT_Structure <- left_join(DLI5_EPT, DLI5_EPT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

DLI5_FCT_Structure <- aggregate(DLI5_FCT$Mood_Use, list(DLI5_FCT$Structure), FUN = mean, na.rm = TRUE)
DLI5_FCT_Structure <- DLI5_FCT_Structure %>% rename(Structure_Avg = x)
DLI5_FCT_Structure <- left_join(DLI5_FCT, DLI5_FCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

MLS5_EPT_Structure <- aggregate(MLS5_EPT$Mood_Use, list(MLS5_EPT$Structure), FUN = mean, na.rm = TRUE)
MLS5_EPT_Structure <- MLS5_EPT_Structure %>% rename(Structure_Avg = x)
MLS5_EPT_Structure <- left_join(MLS5_EPT, MLS5_EPT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

MLS5_FCT_Structure <- aggregate(MLS5_FCT$Mood_Use, list(MLS5_FCT$Structure), FUN = mean, na.rm = TRUE)
MLS5_FCT_Structure <- MLS5_FCT_Structure %>% rename(Structure_Avg = x)
MLS5_FCT_Structure <- left_join(MLS5_FCT, MLS5_FCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## Rejoin datasets
EPT <- rbind(SDB_EPT_Structure, DLI78_EPT_Structure, MLS78_EPT_Structure, DLI5_EPT_Structure, MLS5_EPT_Structure)
FCT <- rbind(SDB_FCT_Structure, DLI78_FCT_Structure, MLS78_FCT_Structure, DLI5_FCT_Structure, MLS5_FCT_Structure)

Aggregate <- rbind(EPT, FCT)
Aggregate$Group <- factor(Aggregate$Group, levels = c("SDBA", "DLI-7/8", "MES-7/8", "DLI-5", "MES-5"))
Aggregate$Task <- factor(Aggregate$Task, levels = c("Production", "Selection"))


## Create heritage-only dataset
Aggregate_Heritage <- Aggregate %>% 
  filter(!Group == "SDBA")
Aggregate_Heritage$Group <- factor(Aggregate_Heritage$Group, levels = c("DLI-7/8", "MES-7/8", "DLI-5", "MES-5"))
Aggregate_Heritage$Task <- factor(Aggregate_Heritage$Task, levels = c("Production", "Selection"))


# Generate bar graphs
## With comparison group
Aggregate %>% 
  ggplot(aes(x = Group, y = Structure_Percentage, fill = Task)) + 
  geom_bar(position = "dodge", color = "black", stat = "identity") +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 102)) +
  geom_text(aes(label = round(Structure_Percentage)),
            position = position_dodge(width = .9),
            vjust = -0.5,
            size = 3) +
  scale_fill_manual(values = c("#56B4E9", "#F0E442")) +
  labs(x = "Group", y = "Percentage of responses with subjunctive", fill = "Task", title = "Subjunctive by Group and Task") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text.x = element_text(face = "bold"))


## Without comparison group
Aggregate_Heritage %>% 
  ggplot(aes(x = Group, y = Structure_Percentage, fill = Task)) + 
  geom_bar(position = "dodge", color = "black", stat = "identity") +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 102)) +
  geom_text(aes(label = round(Structure_Percentage)),
            position = position_dodge(width = .9),
            vjust = -0.5,
            size = 3) +
  scale_fill_manual(values = c("#56B4E9", "#F0E442")) +
  labs(x = "Group", y = "Percentage of responses with subjunctive", fill = "Task", title = "Subjunctive by Group and Task") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text.x = element_text(face = "bold"))
