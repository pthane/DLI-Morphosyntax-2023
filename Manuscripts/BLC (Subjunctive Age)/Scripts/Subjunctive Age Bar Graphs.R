library(tidyverse)


# Load data
## EPT
SDB_EPT <- read_csv("./CSV Files/SDB/SDB Subjunctive EPT.csv") %>%
  filter(Property == "Intensional subjunctive")

HSA_EPT <- read_csv("./CSV Files/Adult HS/Adult HS Subjunctive EPT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HSA")

DLI78_EPT <- read_csv("./CSV Files/DLI-78/DLI-78 Subjunctive EPT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS7/8")

MLS78_EPT <- read_csv("./CSV Files/MLS-78/MLS-78 Subjunctive EPT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS7/8")

DLI5_EPT <- read_csv("./CSV Files/DLI-5/DLI-5 Subjunctive EPT.csv")  %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS5")

MLS5_EPT <- read_csv("./CSV Files/MLS-5/MLS-5 Subjunctive EPT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS5") 


## FCT
SDB_FCT <- read_csv("./CSV Files/SDB/SDB Subjunctive FCT.csv") %>%
  filter(Property == "Intensional subjunctive")

HSA_FCT <- read_csv("./CSV Files/Adult HS/Adult HS Subjunctive FCT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HSA")

DLI78_FCT <- read_csv("./CSV Files/DLI-78/DLI-78 Subjunctive FCT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS7/8")

MLS78_FCT <- read_csv("./CSV Files/MLS-78/MLS-78 Subjunctive FCT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS7/8")

DLI5_FCT <- read_csv("./CSV Files/DLI-5/DLI-5 Subjunctive FCT.csv")  %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS5")

MLS5_FCT <- read_csv("./CSV Files/MLS-5/MLS-5 Subjunctive FCT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS5") 


## Join age group datasets
HS78_EPT <- rbind(DLI78_EPT, MLS78_EPT)
HS78_FCT <- rbind(DLI78_FCT, MLS78_FCT)

HS5_EPT <- rbind(DLI5_EPT, MLS5_EPT)
HS5_FCT <- rbind(DLI5_FCT, MLS5_FCT)


# Generate averages
SDB_EPT_Structure <- aggregate(SDB_EPT$Mood_Use, list(SDB_EPT$Structure), FUN = mean, na.rm = TRUE)
SDB_EPT_Structure <- SDB_EPT_Structure %>% rename(Structure_Avg = x)
SDB_EPT_Structure <- left_join(SDB_EPT, SDB_EPT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

SDB_FCT_Structure <- aggregate(SDB_FCT$Mood_Use, list(SDB_FCT$Structure), FUN = mean, na.rm = TRUE)
SDB_FCT_Structure <- SDB_FCT_Structure %>% rename(Structure_Avg = x)
SDB_FCT_Structure <- left_join(SDB_FCT, SDB_FCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HSA_EPT_Structure <- aggregate(HSA_EPT$Mood_Use, list(HSA_EPT$Structure), FUN = mean, na.rm = TRUE)
HSA_EPT_Structure <- HSA_EPT_Structure %>% rename(Structure_Avg = x)
HSA_EPT_Structure <- left_join(HSA_EPT, HSA_EPT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HSA_FCT_Structure <- aggregate(HSA_FCT$Mood_Use, list(HSA_FCT$Structure), FUN = mean, na.rm = TRUE)
HSA_FCT_Structure <- HSA_FCT_Structure %>% rename(Structure_Avg = x)
HSA_FCT_Structure <- left_join(HSA_FCT, HSA_FCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS78_EPT_Structure <- aggregate(HS78_EPT$Mood_Use, list(HS78_EPT$Structure), FUN = mean, na.rm = TRUE)
HS78_EPT_Structure <- HS78_EPT_Structure %>% rename(Structure_Avg = x)
HS78_EPT_Structure <- left_join(HS78_EPT, HS78_EPT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS78_FCT_Structure <- aggregate(HS78_FCT$Mood_Use, list(HS78_FCT$Structure), FUN = mean, na.rm = TRUE)
HS78_FCT_Structure <- HS78_FCT_Structure %>% rename(Structure_Avg = x)
HS78_FCT_Structure <- left_join(HS78_FCT, HS78_FCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS5_EPT_Structure <- aggregate(HS5_EPT$Mood_Use, list(HS5_EPT$Structure), FUN = mean, na.rm = TRUE)
HS5_EPT_Structure <- HS5_EPT_Structure %>% rename(Structure_Avg = x)
HS5_EPT_Structure <- left_join(HS5_EPT, HS5_EPT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS5_FCT_Structure <- aggregate(HS5_FCT$Mood_Use, list(HS5_FCT$Structure), FUN = mean, na.rm = TRUE)
HS5_FCT_Structure <- HS5_FCT_Structure %>% rename(Structure_Avg = x)
HS5_FCT_Structure <- left_join(HS5_FCT, HS5_FCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


# Join aggregate dataset
EPT <- rbind(SDB_EPT_Structure, HSA_EPT_Structure, HS78_EPT_Structure, HS5_EPT_Structure)
FCT <- rbind(SDB_FCT_Structure, HSA_FCT_Structure, HS78_FCT_Structure, HS5_FCT_Structure)

Aggregate <- rbind(EPT, FCT)
Aggregate$Group <- factor(Aggregate$Group, levels = c("SDB", "HSA", "HS7/8", "HS5"))


# Generate bar graph
Aggregate %>% 
  ggplot(aes(x = Group, y = Structure_Percentage, fill = Task)) + 
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 105)) +
  geom_text(aes(label = round(Structure_Percentage)),
            position = position_dodge(width = .9),
            vjust = -0.5,
            size = 3) +
  labs(x = "Group", y = "Percentage of subjunctive responses", fill = "Task", title = "Subjunctive by Group and Task") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"))
