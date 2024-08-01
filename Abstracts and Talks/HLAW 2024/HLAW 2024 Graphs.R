library(tidyverse)


# Load DOM data
## EPT
SDB_DOM_EPT <- read_csv("./CSV Files/SDB/SDB DOM EPT.csv") %>% 
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>% 
  mutate(Group = "Adults",
         Task = "Production",
         School = "Adults",
         Age = "Adults")

DLI78_DOM_EPT <- read.csv("./CSV Files/DLI-78/DLI-78 DOM EPT.csv") %>% 
  mutate(Group = "Immersion-7/8",
         Task = "Production",
         School = "DLI",
         Age = "7th/8th")

MLS78_DOM_EPT <- read.csv("./CSV Files/MLS-78/MLS-78 DOM EPT.csv") %>% 
  filter(!School == "GBCS") %>%
  mutate(Group = "Monolingual-7/8",
         Task = "Production",
         School = "MLS",
         Age = "7th/8th")

DLI5_DOM_EPT <- read.csv("./CSV Files/MLS-5/MLS-5 DOM EPT.csv") %>% 
  mutate(Group = "Immersion-5",
         Task = "Production",
         School = "DLI",
         Age = "5th")

MLS5_DOM_EPT <- read.csv("./CSV Files/MLS-5/MLS-5 DOM EPT.csv") %>% 
  filter(!School == "GBCS") %>% 
  mutate(Group = "Monolingual-5",
         Task = "Production",
         School = "MLS",
         Age = "5th")


## FCT
SDB_DOM_FCT <- read_csv("./CSV Files/SDB/SDB DOM FCT.csv") %>% 
  mutate(Group = "Adults",
         Task = "Selection",
         School = "Adults",
         Age = "Adults")

DLI78_DOM_FCT <- read.csv("./CSV Files/DLI-78/DLI-78 DOM FCT.csv") %>% 
  mutate(Group = "Immersion-7/8",
         Task = "Selection",
         School = "DLI",
         Age = "7th/8th")

MLS78_DOM_FCT <- read.csv("./CSV Files/MLS-78/MLS-78 DOM FCT.csv") %>% 
  filter(!School == "GBCS") %>%
  mutate(Group = "Monolingual-7/8",
         Task = "Selection",
         School = "MLS",
         Age = "7th/8th")

DLI5_DOM_FCT <- read.csv("./CSV Files/MLS-5/MLS-5 DOM FCT.csv") %>% 
  mutate(Group = "Immersion-5",
         Task = "Selection",
         School = "DLI",
         Age = "5th")

MLS5_DOM_FCT <- read.csv("./CSV Files/MLS-5/MLS-5 DOM FCT.csv") %>% 
  filter(!School == "GBCS") %>% 
  mutate(Group = "Monolingual-5",
         Task = "Selection",
         School = "MLS",
         Age = "5th")


# Load subjunctive data
SDB_Subj_EPT <- read_csv("./CSV Files/SDB/SDB Subjunctive EPT.csv") %>%
  filter(Short_Prop == "Intensional") %>% 
  mutate(Group = "Adults",
         Task = "Production",
         Mood = "Subjunctive",
         School = "Adults",
         Age = "Adults")

DLI78_Subj_EPT <- read.csv("./CSV Files/DLI-78/DLI-78 Subjunctive EPT.csv") %>% 
  filter(Short_Prop == "Intensional") %>% 
  mutate(Group = "Immersion-7/8",
         Task = "Production",
         Mood = "Subjunctive",
         School = "DLI",
         Age = "7th/8th")

MLS78_Subj_EPT <- read.csv("./CSV Files/MLS-78/MLS-78 Subjunctive EPT.csv") %>% 
  filter(!School == "GBCS") %>% 
  filter(Short_Prop == "Intensional") %>% 
  mutate(Group = "Monolingual-7/8",
         Task = "Production",
         Mood = "Subjunctive",
         School = "MLS",
         Age = "7th/8th")

DLI5_Subj_EPT <- read.csv("./CSV Files/MLS-5/MLS-5 Subjunctive EPT.csv") %>% 
  filter(Short_Prop == "Intensional") %>% 
  mutate(Group = "Immersion-5",
         Task = "Production",
         Mood = "Subjunctive",
         School = "DLI",
         Age = "5th")

MLS5_Subj_EPT <- read.csv("./CSV Files/MLS-5/MLS-5 Subjunctive EPT.csv") %>% 
  filter(!School == "GBCS") %>% 
  filter(Short_Prop == "Intensional") %>% 
  mutate(Group = "Monolingual-5",
         Task = "Production",
         Mood = "Subjunctive",
         School = "MLS",
         Age = "5th")


# FCT
SDB_Subj_FCT <- read_csv("./CSV Files/SDB/SDB Subjunctive FCT.csv") %>%
  filter(Short_Prop == "Intensional") %>% 
  mutate(Group = "Adults",
         Task = "Selection",
         Mood = "Subjunctive",
         School = "Adults",
         Age = "Adults")

DLI78_Subj_FCT <- read.csv("./CSV Files/DLI-78/DLI-78 Subjunctive FCT.csv") %>% 
  filter(Short_Prop == "Intensional") %>% 
  mutate(Group = "Immersion-7/8",
         Task = "Selection",
         Mood = "Subjunctive",
         School = "DLI",
         Age = "7th/8th")

MLS78_Subj_FCT <- read.csv("./CSV Files/MLS-78/MLS-78 Subjunctive FCT.csv") %>% 
  filter(!School == "GBCS") %>% 
  filter(Short_Prop == "Intensional") %>% 
  mutate(Group = "Monolingual-7/8",
         Task = "Selection",
         Mood = "Subjunctive",
         School = "MLS",
         Age = "7th/8th")

DLI5_Subj_FCT <- read.csv("./CSV Files/MLS-5/MLS-5 Subjunctive FCT.csv") %>% 
  filter(Short_Prop == "Intensional") %>% 
  mutate(Group = "Immersion-5",
         Task = "Selection",
         Mood = "Subjunctive",
         School = "DLI",
         Age = "5th")

MLS5_Subj_FCT <- read.csv("./CSV Files/MLS-5/MLS-5 Subjunctive FCT.csv") %>% 
  filter(!School == "GBCS") %>% 
  filter(Short_Prop == "Intensional") %>% 
  mutate(Group = "Monolingual-5",
         Task = "Selection",
         Mood = "Subjunctive",
         School = "MLS",
         Age = "5th")


# Generate DOM participant averages for graph
## SDB, Production
SDB_DOM_EPT_Structure <- aggregate(SDB_DOM_EPT$DOM_Use, list(SDB_DOM_EPT$Structure), FUN = mean, na.rm = TRUE)
SDB_DOM_EPT_Structure <- SDB_DOM_EPT_Structure %>% rename(Structure_Avg = x)
SDB_DOM_EPT_Structure <- left_join(SDB_DOM_EPT, SDB_DOM_EPT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## SDB, Selection
SDB_DOM_FCT_Structure <- aggregate(SDB_DOM_FCT$DOM_Use, list(SDB_DOM_FCT$Structure), FUN = mean, na.rm = TRUE)
SDB_DOM_FCT_Structure <- SDB_DOM_FCT_Structure %>% rename(Structure_Avg = x)
SDB_DOM_FCT_Structure <- left_join(SDB_DOM_FCT, SDB_DOM_FCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## DLI78, Production
DLI78_DOM_EPT_Structure <- aggregate(DLI78_DOM_EPT$DOM_Use, list(DLI78_DOM_EPT$Structure), FUN = mean, na.rm = TRUE)
DLI78_DOM_EPT_Structure <- DLI78_DOM_EPT_Structure %>% rename(Structure_Avg = x)
DLI78_DOM_EPT_Structure <- left_join(DLI78_DOM_EPT, DLI78_DOM_EPT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## DLI78, Selection
DLI78_DOM_FCT_Structure <- aggregate(DLI78_DOM_FCT$DOM_Use, list(DLI78_DOM_FCT$Structure), FUN = mean, na.rm = TRUE)
DLI78_DOM_FCT_Structure <- DLI78_DOM_FCT_Structure %>% rename(Structure_Avg = x)
DLI78_DOM_FCT_Structure <- left_join(DLI78_DOM_FCT, DLI78_DOM_FCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## MLS78, Production
MLS78_DOM_EPT_Structure <- aggregate(MLS78_DOM_EPT$DOM_Use, list(MLS78_DOM_EPT$Structure), FUN = mean, na.rm = TRUE)
MLS78_DOM_EPT_Structure <- MLS78_DOM_EPT_Structure %>% rename(Structure_Avg = x)
MLS78_DOM_EPT_Structure <- left_join(MLS78_DOM_EPT, MLS78_DOM_EPT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## MLS78, Selection
MLS78_DOM_FCT_Structure <- aggregate(MLS78_DOM_FCT$DOM_Use, list(MLS78_DOM_FCT$Structure), FUN = mean, na.rm = TRUE)
MLS78_DOM_FCT_Structure <- MLS78_DOM_FCT_Structure %>% rename(Structure_Avg = x)
MLS78_DOM_FCT_Structure <- left_join(MLS78_DOM_FCT, MLS78_DOM_FCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## DLI5, Production
DLI5_DOM_EPT_Structure <- aggregate(DLI5_DOM_EPT$DOM_Use, list(DLI5_DOM_EPT$Structure), FUN = mean, na.rm = TRUE)
DLI5_DOM_EPT_Structure <- DLI5_DOM_EPT_Structure %>% rename(Structure_Avg = x)
DLI5_DOM_EPT_Structure <- left_join(DLI5_DOM_EPT, DLI5_DOM_EPT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## DLI5, Selection
DLI5_DOM_FCT_Structure <- aggregate(DLI5_DOM_FCT$DOM_Use, list(DLI5_DOM_FCT$Structure), FUN = mean, na.rm = TRUE)
DLI5_DOM_FCT_Structure <- DLI5_DOM_FCT_Structure %>% rename(Structure_Avg = x)
DLI5_DOM_FCT_Structure <- left_join(DLI5_DOM_FCT, DLI5_DOM_FCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## MLS5, Production
MLS5_DOM_EPT_Structure <- aggregate(MLS5_DOM_EPT$DOM_Use, list(MLS5_DOM_EPT$Structure), FUN = mean, na.rm = TRUE)
MLS5_DOM_EPT_Structure <- MLS5_DOM_EPT_Structure %>% rename(Structure_Avg = x)
MLS5_DOM_EPT_Structure <- left_join(MLS5_DOM_EPT, MLS5_DOM_EPT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## MLS5, Selection
MLS5_DOM_FCT_Structure <- aggregate(MLS5_DOM_FCT$DOM_Use, list(MLS5_DOM_FCT$Structure), FUN = mean, na.rm = TRUE)
MLS5_DOM_FCT_Structure <- MLS5_DOM_FCT_Structure %>% rename(Structure_Avg = x)
MLS5_DOM_FCT_Structure <- left_join(MLS5_DOM_FCT, MLS5_DOM_FCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## Join datasets
DOM_EPT_Final <- rbind(SDB_DOM_EPT_Structure, DLI78_DOM_EPT_Structure, MLS78_DOM_EPT_Structure, DLI5_DOM_EPT_Structure, MLS5_DOM_EPT_Structure) %>% 
  mutate(Structure = "DOM")

DOM_FCT_Final <- rbind(SDB_DOM_FCT_Structure, DLI78_DOM_FCT_Structure, MLS78_DOM_FCT_Structure, DLI5_DOM_FCT_Structure, MLS5_DOM_FCT_Structure) %>% 
  mutate(Structure = "DOM")

DOM_Master <- rbind(DOM_EPT_Final, DOM_FCT_Final)
DOM_Master$Group <- factor(DOM_Master$Group, levels = c("Adults", "Immersion-7/8", "Monolingual-7/8", "Immersion-5", "Monolingual-5"))


# Generate subjunctive participant averages for graph
## SDB, Production
SDB_Subj_EPT_Structure <- aggregate(SDB_Subj_EPT$Mood_Use, list(SDB_Subj_EPT$Structure), FUN = mean, na.rm = TRUE)
SDB_Subj_EPT_Structure <- SDB_Subj_EPT_Structure %>% rename(Structure_Avg = x)
SDB_Subj_EPT_Structure <- left_join(SDB_Subj_EPT, SDB_Subj_EPT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## SDB, Selection
SDB_Subj_FCT_Structure <- aggregate(SDB_Subj_FCT$Mood_Use, list(SDB_Subj_FCT$Structure), FUN = mean, na.rm = TRUE)
SDB_Subj_FCT_Structure <- SDB_Subj_FCT_Structure %>% rename(Structure_Avg = x)
SDB_Subj_FCT_Structure <- left_join(SDB_Subj_FCT, SDB_Subj_FCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## DLI78, Production
DLI78_Subj_EPT_Structure <- aggregate(DLI78_Subj_EPT$Mood_Use, list(DLI78_Subj_EPT$Structure), FUN = mean, na.rm = TRUE)
DLI78_Subj_EPT_Structure <- DLI78_Subj_EPT_Structure %>% rename(Structure_Avg = x)
DLI78_Subj_EPT_Structure <- left_join(DLI78_Subj_EPT, DLI78_Subj_EPT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## DLI78, Selection
DLI78_Subj_FCT_Structure <- aggregate(DLI78_Subj_FCT$Mood_Use, list(DLI78_Subj_FCT$Structure), FUN = mean, na.rm = TRUE)
DLI78_Subj_FCT_Structure <- DLI78_Subj_FCT_Structure %>% rename(Structure_Avg = x)
DLI78_Subj_FCT_Structure <- left_join(DLI78_Subj_FCT, DLI78_Subj_FCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## MLS78, Production
MLS78_Subj_EPT_Structure <- aggregate(MLS78_Subj_EPT$Mood_Use, list(MLS78_Subj_EPT$Structure), FUN = mean, na.rm = TRUE)
MLS78_Subj_EPT_Structure <- MLS78_Subj_EPT_Structure %>% rename(Structure_Avg = x)
MLS78_Subj_EPT_Structure <- left_join(MLS78_Subj_EPT, MLS78_Subj_EPT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## MLS78, Selection
MLS78_Subj_FCT_Structure <- aggregate(MLS78_Subj_FCT$Mood_Use, list(MLS78_Subj_FCT$Structure), FUN = mean, na.rm = TRUE)
MLS78_Subj_FCT_Structure <- MLS78_Subj_FCT_Structure %>% rename(Structure_Avg = x)
MLS78_Subj_FCT_Structure <- left_join(MLS78_Subj_FCT, MLS78_Subj_FCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## DLI5, Production
DLI5_Subj_EPT_Structure <- aggregate(DLI5_Subj_EPT$Mood_Use, list(DLI5_Subj_EPT$Structure), FUN = mean, na.rm = TRUE)
DLI5_Subj_EPT_Structure <- DLI5_Subj_EPT_Structure %>% rename(Structure_Avg = x)
DLI5_Subj_EPT_Structure <- left_join(DLI5_Subj_EPT, DLI5_Subj_EPT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## DLI5, Selection
DLI5_Subj_FCT_Structure <- aggregate(DLI5_Subj_FCT$Mood_Use, list(DLI5_Subj_FCT$Structure), FUN = mean, na.rm = TRUE)
DLI5_Subj_FCT_Structure <- DLI5_Subj_FCT_Structure %>% rename(Structure_Avg = x)
DLI5_Subj_FCT_Structure <- left_join(DLI5_Subj_FCT, DLI5_Subj_FCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## MLS5, Production
MLS5_Subj_EPT_Structure <- aggregate(MLS5_Subj_EPT$Mood_Use, list(MLS5_Subj_EPT$Structure), FUN = mean, na.rm = TRUE)
MLS5_Subj_EPT_Structure <- MLS5_Subj_EPT_Structure %>% rename(Structure_Avg = x)
MLS5_Subj_EPT_Structure <- left_join(MLS5_Subj_EPT, MLS5_Subj_EPT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## MLS5, Selection
MLS5_Subj_FCT_Structure <- aggregate(MLS5_Subj_FCT$Mood_Use, list(MLS5_Subj_FCT$Structure), FUN = mean, na.rm = TRUE)
MLS5_Subj_FCT_Structure <- MLS5_Subj_FCT_Structure %>% rename(Structure_Avg = x)
MLS5_Subj_FCT_Structure <- left_join(MLS5_Subj_FCT, MLS5_Subj_FCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## Join datasets
Subj_EPT_Final <- rbind(SDB_Subj_EPT_Structure, DLI78_Subj_EPT_Structure, MLS78_Subj_EPT_Structure, DLI5_Subj_EPT_Structure, MLS5_Subj_EPT_Structure) %>% 
  mutate(Structure = "Subj")

Subj_FCT_Final <- rbind(SDB_Subj_FCT_Structure, DLI78_Subj_FCT_Structure, MLS78_Subj_FCT_Structure, DLI5_Subj_FCT_Structure, MLS5_Subj_FCT_Structure) %>% 
  mutate(Structure = "Subj")

Subj_Master <- rbind(Subj_EPT_Final, Subj_FCT_Final)
Subj_Master$Group <- factor(Subj_Master$Group, levels = c("Adults", "Immersion-7/8", "Monolingual-7/8", "Immersion-5", "Monolingual-5"))


# Generate plots
## DOM graph
DOM_Master %>% 
  ggplot(aes(x = Group, y = Structure_Percentage, fill = Task)) +
  geom_bar(position = "dodge", color = "black", stat = "identity") +
  facet_wrap(facets = vars(Structure)) +
  scale_y_continuous(breaks = seq(0, 100, 20),
                     limits = c(0, 105)) +
  geom_text(aes(label = round(Structure_Percentage)),
            position = position_dodge(width = .9),
            vjust = -0.5,
            size = 3.5) +
  labs(x = "Group", y = "Percentage of DOM responses", fill = "Task", title = "DOM by Group and Task") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text.x = element_text(face = "bold"))


## Subjunctive graph
Subj_Master %>% 
  ggplot(aes(x = Group, y = Structure_Percentage, fill = Task)) +
  geom_bar(position = "dodge", color = "black", stat = "identity") +
  facet_wrap(facets = vars(Structure)) +
  scale_y_continuous(breaks = seq(0, 100, 20),
                     limits = c(0, 105)) +
  geom_text(aes(label = round(Structure_Percentage)),
            position = position_dodge(width = .9),
            vjust = -0.5,
            size = 3.5) +
  labs(x = "Group", y = "Percentage of subjunctive responses", fill = "Task", title = "Subjunctive by Group and Task") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text.x = element_text(face = "bold"))  
