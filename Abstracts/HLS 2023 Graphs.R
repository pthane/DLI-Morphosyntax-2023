library(tidyverse)


# Load data
## EPT DOM
DLI78_DOM_EPT <- read_csv("./CSV Files/DLI-78/DLI-78 DOM EPT.csv") %>% 
  mutate(Group = "DLE-7/8")

MLS78_DOM_EPT <- read_csv("./CSV Files/MLS-78/MLS-78 DOM EPT.csv")

DLI5_DOM_EPT <- read_csv("./CSV Files/DLI-5/DLI-5 DOM EPT.csv") %>% 
  mutate(Group = "DLE-5")

MLS5_DOM_EPT <- read_csv("./CSV Files/MLS-5/MLS-5 DOM EPT.csv")


## FCT DOM
DLI78_DOM_FCT <- read_csv("./CSV Files/DLI-78/DLI-78 DOM FCT.csv")  %>%
  mutate(Group = "DLE-7/8")

MLS78_DOM_FCT <- read_csv("./CSV Files/MLS-78/MLS-78 DOM FCT.csv")

DLI5_DOM_FCT <- read_csv("./CSV Files/DLI-5/DLI-5 DOM FCT.csv") %>% 
  mutate(Group = "DLE-5")

MLS5_DOM_FCT <- read_csv("./CSV Files/MLS-5/MLS-5 DOM FCT.csv")


## EPT Subjunctive
DLI78_Subj_EPT <- read_csv("./CSV Files/DLI-78/DLI-78 Subjunctive EPT.csv") %>% 
  mutate(Group = "DLE-7/8") %>% 
  filter(Property == "Intensional subjunctive")

MLS78_Subj_EPT <- read_csv("./CSV Files/MLS-78/MLS-78 Subjunctive EPT.csv") %>% 
  filter(Property == "Intensional subjunctive")

DLI5_Subj_EPT <- read_csv("./CSV Files/DLI-5/DLI-5 Subjunctive EPT.csv") %>% 
  mutate(Group = "DLE-5") %>% 
  filter(Property == "Intensional subjunctive")

MLS5_Subj_EPT <- read_csv("./CSV Files/MLS-5/MLS-5 Subjunctive EPT.csv") %>% 
  filter(Property == "Intensional subjunctive")


## FCT Subjunctive
DLI78_Subj_FCT <- read_csv("./CSV Files/DLI-78/DLI-78 Subjunctive FCT.csv")  %>%
  mutate(Group = "DLE-7/8") %>% 
  filter(Property == "Intensional subjunctive")

MLS78_Subj_FCT <- read_csv("./CSV Files/MLS-78/MLS-78 Subjunctive FCT.csv") %>% 
  filter(Property == "Intensional subjunctive")

DLI5_Subj_FCT <- read_csv("./CSV Files/DLI-5/DLI-5 Subjunctive FCT.csv") %>% 
  mutate(Group = "DLE-5")

MLS5_Subj_FCT <- read_csv("./CSV Files/MLS-5/MLS-5 Subjunctive FCT.csv") %>% 
  filter(Property == "Intensional subjunctive")


# Generate aggregate levels for DOM
## Create individual values
DLI78_DOM_EPT_Structure <- aggregate(DLI78_DOM_EPT$DOM_Use, list(DLI78_DOM_EPT$Structure), FUN = mean, na.rm = TRUE)
DLI78_DOM_EPT_Structure <- DLI78_DOM_EPT_Structure %>% rename(Structure_Avg = x)
DLI78_DOM_EPT_Structure <- left_join(DLI78_DOM_EPT, DLI78_DOM_EPT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

DLI78_DOM_FCT_Structure <- aggregate(DLI78_DOM_FCT$DOM_Use, list(DLI78_DOM_FCT$Structure), FUN = mean, na.rm = TRUE)
DLI78_DOM_FCT_Structure <- DLI78_DOM_FCT_Structure %>% rename(Structure_Avg = x)
DLI78_DOM_FCT_Structure <- left_join(DLI78_DOM_FCT, DLI78_DOM_FCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

MLS78_DOM_EPT_Structure <- aggregate(MLS78_DOM_EPT$DOM_Use, list(MLS78_DOM_EPT$Structure), FUN = mean, na.rm = TRUE)
MLS78_DOM_EPT_Structure <- MLS78_DOM_EPT_Structure %>% rename(Structure_Avg = x)
MLS78_DOM_EPT_Structure <- left_join(MLS78_DOM_EPT, MLS78_DOM_EPT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

MLS78_DOM_FCT_Structure <- aggregate(MLS78_DOM_FCT$DOM_Use, list(MLS78_DOM_FCT$Structure), FUN = mean, na.rm = TRUE)
MLS78_DOM_FCT_Structure <- MLS78_DOM_FCT_Structure %>% rename(Structure_Avg = x)
MLS78_DOM_FCT_Structure <- left_join(MLS78_DOM_FCT, MLS78_DOM_FCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

DLI5_DOM_EPT_Structure <- aggregate(DLI5_DOM_EPT$DOM_Use, list(DLI5_DOM_EPT$Structure), FUN = mean, na.rm = TRUE)
DLI5_DOM_EPT_Structure <- DLI5_DOM_EPT_Structure %>% rename(Structure_Avg = x)
DLI5_DOM_EPT_Structure <- left_join(DLI5_DOM_EPT, DLI5_DOM_EPT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

DLI5_DOM_FCT_Structure <- aggregate(DLI5_DOM_FCT$DOM_Use, list(DLI5_DOM_FCT$Structure), FUN = mean, na.rm = TRUE)
DLI5_DOM_FCT_Structure <- DLI5_DOM_FCT_Structure %>% rename(Structure_Avg = x)
DLI5_DOM_FCT_Structure <- left_join(DLI5_DOM_FCT, DLI5_DOM_FCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

MLS5_DOM_EPT_Structure <- aggregate(MLS5_DOM_EPT$DOM_Use, list(MLS5_DOM_EPT$Structure), FUN = mean, na.rm = TRUE)
MLS5_DOM_EPT_Structure <- MLS5_DOM_EPT_Structure %>% rename(Structure_Avg = x)
MLS5_DOM_EPT_Structure <- left_join(MLS5_DOM_EPT, MLS5_DOM_EPT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

MLS5_DOM_FCT_Structure <- aggregate(MLS5_DOM_FCT$DOM_Use, list(MLS5_DOM_FCT$Structure), FUN = mean, na.rm = TRUE)
MLS5_DOM_FCT_Structure <- MLS5_DOM_FCT_Structure %>% rename(Structure_Avg = x)
MLS5_DOM_FCT_Structure <- left_join(MLS5_DOM_FCT, MLS5_DOM_FCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## Rejoin datasets
EPT_DOM <- rbind(DLI78_DOM_EPT_Structure, MLS78_DOM_EPT_Structure, DLI5_DOM_EPT_Structure, MLS5_DOM_EPT_Structure) %>% 
  mutate(Task = "Production")
FCT_DOM <- rbind(DLI78_DOM_FCT_Structure, MLS78_DOM_FCT_Structure, DLI5_DOM_FCT_Structure, MLS5_DOM_FCT_Structure) %>% 
  mutate(Task = "Multiple choice")

Aggregate_DOM <- rbind(EPT_DOM, FCT_DOM)
Aggregate_DOM$Group <- factor(Aggregate_DOM$Group, levels = c("DLE-7/8", "MLS-7/8", "DLE-5", "MLS-5"))
Aggregate_DOM$Task <- factor(Aggregate_DOM$Task, levels = c("Production", "Multiple choice"))


# Generate aggregate levels for subjunctive
## Create individual values
DLI78_Subj_EPT_Structure <- aggregate(DLI78_Subj_EPT$Mood_Use, list(DLI78_Subj_EPT$Structure), FUN = mean, na.rm = TRUE)
DLI78_Subj_EPT_Structure <- DLI78_Subj_EPT_Structure %>% rename(Structure_Avg = x)
DLI78_Subj_EPT_Structure <- left_join(DLI78_Subj_EPT, DLI78_Subj_EPT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

DLI78_Subj_FCT_Structure <- aggregate(DLI78_Subj_FCT$Mood_Use, list(DLI78_Subj_FCT$Structure), FUN = mean, na.rm = TRUE)
DLI78_Subj_FCT_Structure <- DLI78_Subj_FCT_Structure %>% rename(Structure_Avg = x)
DLI78_Subj_FCT_Structure <- left_join(DLI78_Subj_FCT, DLI78_Subj_FCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

MLS78_Subj_EPT_Structure <- aggregate(MLS78_Subj_EPT$Mood_Use, list(MLS78_Subj_EPT$Structure), FUN = mean, na.rm = TRUE)
MLS78_Subj_EPT_Structure <- MLS78_Subj_EPT_Structure %>% rename(Structure_Avg = x)
MLS78_Subj_EPT_Structure <- left_join(MLS78_Subj_EPT, MLS78_Subj_EPT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

MLS78_Subj_FCT_Structure <- aggregate(MLS78_Subj_FCT$Mood_Use, list(MLS78_Subj_FCT$Structure), FUN = mean, na.rm = TRUE)
MLS78_Subj_FCT_Structure <- MLS78_Subj_FCT_Structure %>% rename(Structure_Avg = x)
MLS78_Subj_FCT_Structure <- left_join(MLS78_Subj_FCT, MLS78_Subj_FCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

DLI5_Subj_EPT_Structure <- aggregate(DLI5_Subj_EPT$Mood_Use, list(DLI5_Subj_EPT$Structure), FUN = mean, na.rm = TRUE)
DLI5_Subj_EPT_Structure <- DLI5_Subj_EPT_Structure %>% rename(Structure_Avg = x)
DLI5_Subj_EPT_Structure <- left_join(DLI5_Subj_EPT, DLI5_Subj_EPT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

DLI5_Subj_FCT_Structure <- aggregate(DLI5_Subj_FCT$Mood_Use, list(DLI5_Subj_FCT$Structure), FUN = mean, na.rm = TRUE)
DLI5_Subj_FCT_Structure <- DLI5_Subj_FCT_Structure %>% rename(Structure_Avg = x)
DLI5_Subj_FCT_Structure <- left_join(DLI5_Subj_FCT, DLI5_Subj_FCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

MLS5_Subj_EPT_Structure <- aggregate(MLS5_Subj_EPT$Mood_Use, list(MLS5_Subj_EPT$Structure), FUN = mean, na.rm = TRUE)
MLS5_Subj_EPT_Structure <- MLS5_Subj_EPT_Structure %>% rename(Structure_Avg = x)
MLS5_Subj_EPT_Structure <- left_join(MLS5_Subj_EPT, MLS5_Subj_EPT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

MLS5_Subj_FCT_Structure <- aggregate(MLS5_Subj_FCT$Mood_Use, list(MLS5_Subj_FCT$Structure), FUN = mean, na.rm = TRUE)
MLS5_Subj_FCT_Structure <- MLS5_Subj_FCT_Structure %>% rename(Structure_Avg = x)
MLS5_Subj_FCT_Structure <- left_join(MLS5_Subj_FCT, MLS5_Subj_FCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## Rejoin datasets
EPT_Subj <- rbind(DLI78_Subj_EPT_Structure, MLS78_Subj_EPT_Structure, DLI5_Subj_EPT_Structure, MLS5_Subj_EPT_Structure) %>% 
  mutate(Task = "Production")
FCT_Subj <- rbind(DLI78_Subj_FCT_Structure, MLS78_Subj_FCT_Structure, DLI5_Subj_FCT_Structure, MLS5_Subj_FCT_Structure) %>% 
  mutate(Task = "Multiple choice")

Aggregate_Subj <- rbind(EPT_Subj, FCT_Subj)
Aggregate_Subj$Group <- factor(Aggregate_Subj$Group, levels = c("DLE-7/8", "MLS-7/8", "DLE-5", "MLS-5"))
Aggregate_Subj$Task <- factor(Aggregate_Subj$Task, levels = c("Production", "Multiple choice"))


# Generate bar graphs
## DOM
Aggregate_DOM %>% 
  ggplot(aes(x = Group, y = Structure_Percentage, fill = Task)) + 
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 100)) +
  labs(x = "Subjunctive type", y = "Percentage of DOM responses", fill = "Task", title = "DOM by Group and Task") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text.x = element_text(face = "bold"))


## Subjunctive
Aggregate_Subj %>% 
  ggplot(aes(x = Group, y = Structure_Percentage, fill = Task)) + 
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 100)) +
  labs(x = "Subjunctive type", y = "Percentage of Subjunctive responses", fill = "Task", title = "Subjunctive by Group and Task") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text.x = element_text(face = "bold"))
