library(tidyverse)


# Load data
## EPT DOM
HSA_DOM_EPT <- read_csv("./CSV Files/Adult HS/Adult HS DOM EPT.csv") %>% 
  mutate(Group = "HSA") %>% 
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42"))

DLI78_DOM_EPT <- read_csv("./CSV Files/DLI-78/DLI-78 DOM EPT.csv") %>% 
  mutate(Group = "HS7/8")

MLS78_DOM_EPT <- read_csv("./CSV Files/MLS-78/MLS-78 DOM EPT.csv") %>% 
  mutate(Group = "HS7/8")

DLI5_DOM_EPT <- read_csv("./CSV Files/DLI-5/DLI-5 DOM EPT.csv") %>% 
  mutate(Group = "HS5")

MLS5_DOM_EPT <- read_csv("./CSV Files/MLS-5/MLS-5 DOM EPT.csv") %>% 
  mutate(Group = "HS5")


## FCT DOM
HSA_DOM_FCT <- read_csv("./CSV Files/Adult HS/Adult HS DOM FCT.csv") %>% 
  mutate(Group = "HSA")

DLI78_DOM_FCT <- read_csv("./CSV Files/DLI-78/DLI-78 DOM FCT.csv") %>% 
  mutate(Group = "HS7/8")

MLS78_DOM_FCT <- read_csv("./CSV Files/MLS-78/MLS-78 DOM FCT.csv") %>% 
  mutate(Group = "HS7/8")

DLI5_DOM_FCT <- read_csv("./CSV Files/DLI-5/DLI-5 DOM FCT.csv") %>% 
  mutate(Group = "HS5")

MLS5_DOM_FCT <- read_csv("./CSV Files/MLS-5/MLS-5 DOM FCT.csv") %>% 
  mutate(Group = "HS5")


## EPT Subjunctive
HSA_Subj_EPT <- read_csv("./CSV Files/Adult HS/Adult HS Subjunctive FCT.csv") %>% 
  mutate(Group = "HSA") %>% 
  filter(Property == "Intensional subjunctive")

DLI78_Subj_EPT <- read_csv("./CSV Files/DLI-78/DLI-78 Subjunctive EPT.csv") %>% 
  mutate(Group = "HS7/8") %>% 
  filter(Property == "Intensional subjunctive")

MLS78_Subj_EPT <- read_csv("./CSV Files/MLS-78/MLS-78 Subjunctive EPT.csv") %>% 
  mutate(Group = "HS7/8") %>% 
  filter(Property == "Intensional subjunctive")

DLI5_Subj_EPT <- read_csv("./CSV Files/DLI-5/DLI-5 Subjunctive EPT.csv") %>% 
  mutate(Group = "HS5") %>% 
  filter(Property == "Intensional subjunctive")

MLS5_Subj_EPT <- read_csv("./CSV Files/MLS-5/MLS-5 Subjunctive EPT.csv") %>% 
  mutate(Group = "HS5") %>%
  filter(Property == "Intensional subjunctive")


## FCT Subjunctive
HSA_Subj_FCT <- read_csv("./CSV Files/Adult HS/Adult HS Subjunctive FCT.csv") %>% 
  mutate(Group = "HSA") %>% 
  filter(Property == "Intensional subjunctive")

DLI78_Subj_FCT <- read_csv("./CSV Files/DLI-78/DLI-78 Subjunctive FCT.csv") %>% 
  mutate(Group = "HS7/8") %>% 
  filter(Property == "Intensional subjunctive")

MLS78_Subj_FCT <- read_csv("./CSV Files/MLS-78/MLS-78 Subjunctive FCT.csv") %>% 
  mutate(Group = "HS7/8") %>% 
  filter(Property == "Intensional subjunctive")

DLI5_Subj_FCT <- read_csv("./CSV Files/DLI-5/DLI-5 Subjunctive FCT.csv") %>% 
  mutate(Group = "HS5") %>% 
  filter(Property == "Intensional subjunctive")

MLS5_Subj_FCT <- read_csv("./CSV Files/MLS-5/MLS-5 Subjunctive FCT.csv") %>% 
  mutate(Group = "HS5") %>%
  filter(Property == "Intensional subjunctive")


## Rejoin relevant datasets
HS78_DOM_EPT <- rbind(DLI78_DOM_EPT, MLS78_DOM_EPT)
HS78_Subj_EPT <- rbind(DLI78_Subj_EPT, DLI78_Subj_EPT)
HS78_DOM_FCT <- rbind(DLI78_DOM_FCT, MLS78_DOM_FCT)
HS78_Subj_FCT <- rbind(DLI78_Subj_FCT, DLI78_Subj_FCT)

HS5_DOM_EPT <- rbind(DLI5_DOM_EPT, MLS5_DOM_EPT)
HS5_Subj_EPT <- rbind(DLI5_Subj_EPT, MLS5_Subj_EPT)
HS5_DOM_FCT <- rbind(DLI5_DOM_FCT, MLS5_DOM_FCT)
HS5_Subj_FCT <- rbind(DLI5_Subj_FCT, MLS5_Subj_FCT)


# Generate aggregate levels for DOM
## Create individual values
HSA_DOM_EPT_Structure <- aggregate(HSA_DOM_EPT$DOM_Use, list(HSA_DOM_EPT$Structure), FUN = mean, na.rm = TRUE)
HSA_DOM_EPT_Structure <- HSA_DOM_EPT_Structure %>% rename(Structure_Avg = x)
HSA_DOM_EPT_Structure <- left_join(HSA_DOM_EPT, HSA_DOM_EPT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HSA_DOM_FCT_Structure <- aggregate(HSA_DOM_FCT$DOM_Use, list(HSA_DOM_FCT$Structure), FUN = mean, na.rm = TRUE)
HSA_DOM_FCT_Structure <- HSA_DOM_FCT_Structure %>% rename(Structure_Avg = x)
HSA_DOM_FCT_Structure <- left_join(HSA_DOM_FCT, HSA_DOM_FCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS78_DOM_EPT_Structure <- aggregate(HS78_DOM_EPT$DOM_Use, list(HS78_DOM_EPT$Structure), FUN = mean, na.rm = TRUE)
HS78_DOM_EPT_Structure <- HS78_DOM_EPT_Structure %>% rename(Structure_Avg = x)
HS78_DOM_EPT_Structure <- left_join(HS78_DOM_EPT, HS78_DOM_EPT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS78_DOM_FCT_Structure <- aggregate(HS78_DOM_FCT$DOM_Use, list(HS78_DOM_FCT$Structure), FUN = mean, na.rm = TRUE)
HS78_DOM_FCT_Structure <- HS78_DOM_FCT_Structure %>% rename(Structure_Avg = x)
HS78_DOM_FCT_Structure <- left_join(HS78_DOM_FCT, HS78_DOM_FCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS5_DOM_EPT_Structure <- aggregate(HS5_DOM_EPT$DOM_Use, list(HS5_DOM_EPT$Structure), FUN = mean, na.rm = TRUE)
HS5_DOM_EPT_Structure <- HS5_DOM_EPT_Structure %>% rename(Structure_Avg = x)
HS5_DOM_EPT_Structure <- left_join(HS5_DOM_EPT, HS5_DOM_EPT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS5_DOM_FCT_Structure <- aggregate(HS5_DOM_FCT$DOM_Use, list(HS5_DOM_FCT$Structure), FUN = mean, na.rm = TRUE)
HS5_DOM_FCT_Structure <- HS5_DOM_FCT_Structure %>% rename(Structure_Avg = x)
HS5_DOM_FCT_Structure <- left_join(HS5_DOM_FCT, HS5_DOM_FCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## Rejoin datasets
EPT_DOM <- rbind(HSA_DOM_EPT_Structure, HS78_DOM_EPT_Structure, HS5_DOM_EPT_Structure) %>% 
  mutate(Task = "Production")
FCT_DOM <- rbind(HSA_DOM_FCT_Structure, HS78_DOM_FCT_Structure, HS5_DOM_FCT_Structure) %>% 
  mutate(Task = "Multiple choice")

Aggregate_DOM <- rbind(EPT_DOM, FCT_DOM)
Aggregate_DOM$Group <- factor(Aggregate_DOM$Group, levels = c("HSA", "HS7/8", "HS5"))
Aggregate_DOM$Task <- factor(Aggregate_DOM$Task, levels = c("Production", "Multiple choice"))


# Generate aggregate levels for subjunctive
## Create individual values
HSA_Subj_EPT_Structure <- aggregate(HSA_Subj_EPT$Mood_Use, list(HSA_Subj_EPT$Structure), FUN = mean, na.rm = TRUE)
HSA_Subj_EPT_Structure <- HSA_Subj_EPT_Structure %>% rename(Structure_Avg = x)
HSA_Subj_EPT_Structure <- left_join(HSA_Subj_EPT, HSA_Subj_EPT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HSA_Subj_FCT_Structure <- aggregate(HSA_Subj_FCT$Mood_Use, list(HSA_Subj_FCT$Structure), FUN = mean, na.rm = TRUE)
HSA_Subj_FCT_Structure <- HSA_Subj_FCT_Structure %>% rename(Structure_Avg = x)
HSA_Subj_FCT_Structure <- left_join(HSA_Subj_FCT, HSA_Subj_FCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS78_Subj_EPT_Structure <- aggregate(HS78_Subj_EPT$Mood_Use, list(HS78_Subj_EPT$Structure), FUN = mean, na.rm = TRUE)
HS78_Subj_EPT_Structure <- HS78_Subj_EPT_Structure %>% rename(Structure_Avg = x)
HS78_Subj_EPT_Structure <- left_join(HS78_Subj_EPT, HS78_Subj_EPT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS78_Subj_FCT_Structure <- aggregate(HS78_Subj_FCT$Mood_Use, list(HS78_Subj_FCT$Structure), FUN = mean, na.rm = TRUE)
HS78_Subj_FCT_Structure <- HS78_Subj_FCT_Structure %>% rename(Structure_Avg = x)
HS78_Subj_FCT_Structure <- left_join(HS78_Subj_FCT, HS78_Subj_FCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS5_Subj_EPT_Structure <- aggregate(HS5_Subj_EPT$Mood_Use, list(HS5_Subj_EPT$Structure), FUN = mean, na.rm = TRUE)
HS5_Subj_EPT_Structure <- HS5_Subj_EPT_Structure %>% rename(Structure_Avg = x)
HS5_Subj_EPT_Structure <- left_join(HS5_Subj_EPT, HS5_Subj_EPT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS5_Subj_FCT_Structure <- aggregate(HS5_Subj_FCT$Mood_Use, list(HS5_Subj_FCT$Structure), FUN = mean, na.rm = TRUE)
HS5_Subj_FCT_Structure <- HS5_Subj_FCT_Structure %>% rename(Structure_Avg = x)
HS5_Subj_FCT_Structure <- left_join(HS5_Subj_FCT, HS5_Subj_FCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## Rejoin datasets
EPT_Subj <- rbind(HSA_Subj_EPT_Structure, HS78_Subj_EPT_Structure, HS5_Subj_EPT_Structure) %>% 
  mutate(Task = "Production")
FCT_Subj <- rbind(HSA_Subj_FCT_Structure, HS78_Subj_FCT_Structure, HS5_Subj_FCT_Structure) %>% 
  mutate(Task = "Multiple choice")

Aggregate_Subj <- rbind(EPT_Subj, FCT_Subj)
Aggregate_Subj$Group <- factor(Aggregate_Subj$Group, levels = c("HSA", "HS7/8", "HS5"))
Aggregate_Subj$Task <- factor(Aggregate_Subj$Task, levels = c("Production", "Multiple choice"))


# Generate bar graphs
## DOM
Aggregate_DOM %>% 
  ggplot(aes(x = Group, y = Structure_Percentage, fill = Task)) + 
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 100)) +
  labs(x = "Group", y = "Percentage of DOM responses", fill = "Task", title = "DOM by Group and Task") +
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
  labs(x = "Group", y = "Percentage of subjunctive responses", fill = "Task", title = "Subjunctive by Group and Task") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text.x = element_text(face = "bold"))
