library(tidyverse)


# Load and prepare data
## EPT
SDB_EPT <- read_csv("./CSV Files/SDB/SDB Subjunctive EPT.csv") %>% 
  filter(Short_Prop == "Intensional") %>% 
  mutate(Group = "SDBA",
         Task = "SCT")

HS_EPT <- read_csv("./CSV Files/Adult HS/Adult HS Subjunctive EPT.csv") %>% 
  filter(Short_Prop == "Intensional") %>% 
  mutate(Group = "HS",
         Task = "SCT")

L2L_EPT <- read_csv("./CSV Files/Adult L2L/Adult L2L Subjunctive EPT.csv") %>% 
  filter(Short_Prop == "Intensional") %>% 
  mutate(Group = "L2L",
         Task = "SCT")


## FCT
SDB_FCT <- read_csv("./CSV Files/SDB/SDB Subjunctive FCT.csv") %>% 
  filter(Short_Prop == "Intensional") %>% 
  mutate(Group = "SDBA",
         Task = "MRT")

HS_FCT <- read_csv("./CSV Files/Adult HS/Adult HS Subjunctive FCT.csv") %>% 
  filter(Short_Prop == "Intensional") %>% 
  mutate(Group = "HS",
         Task = "MRT")

L2L_FCT <- read_csv("./CSV Files/Adult L2L/Adult L2L Subjunctive FCT.csv") %>% 
  filter(Short_Prop == "Intensional") %>% 
  mutate(Group = "L2L",
         Task = "MRT")


# Generate averages
## SDBA
SDB_EPT_Structure <- aggregate(SDB_EPT$Mood_Use, list(SDB_EPT$Structure), FUN = mean, na.rm = TRUE)
SDB_EPT_Structure <- SDB_EPT_Structure %>% rename(Structure_Avg = x)
SDB_EPT_Structure <- left_join(SDB_EPT, SDB_EPT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

SDB_FCT_Structure <- aggregate(SDB_FCT$Mood_Use, list(SDB_FCT$Structure), FUN = mean, na.rm = TRUE)
SDB_FCT_Structure <- SDB_FCT_Structure %>% rename(Structure_Avg = x)
SDB_FCT_Structure <- left_join(SDB_FCT, SDB_FCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## HS
HS_EPT_Structure <- aggregate(HS_EPT$Mood_Use, list(HS_EPT$Structure), FUN = mean, na.rm = TRUE)
HS_EPT_Structure <- HS_EPT_Structure %>% rename(Structure_Avg = x)
HS_EPT_Structure <- left_join(HS_EPT, HS_EPT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS_FCT_Structure <- aggregate(HS_FCT$Mood_Use, list(HS_FCT$Structure), FUN = mean, na.rm = TRUE)
HS_FCT_Structure <- HS_FCT_Structure %>% rename(Structure_Avg = x)
HS_FCT_Structure <- left_join(HS_FCT, HS_FCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## L2L
L2L_EPT_Structure <- aggregate(L2L_EPT$Mood_Use, list(L2L_EPT$Structure), FUN = mean, na.rm = TRUE)
L2L_EPT_Structure <- L2L_EPT_Structure %>% rename(Structure_Avg = x)
L2L_EPT_Structure <- left_join(L2L_EPT, L2L_EPT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

L2L_FCT_Structure <- aggregate(L2L_FCT$Mood_Use, list(L2L_FCT$Structure), FUN = mean, na.rm = TRUE)
L2L_FCT_Structure <- L2L_FCT_Structure %>% rename(Structure_Avg = x)
L2L_FCT_Structure <- left_join(L2L_FCT, L2L_FCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


# Join aggregate dataset
EPT <- rbind(SDB_EPT_Structure, HS_EPT_Structure, L2L_EPT_Structure)
FCT <- rbind(SDB_FCT_Structure, HS_FCT_Structure, L2L_FCT_Structure)

Aggregate <- rbind(EPT, FCT)
Aggregate$Group <- factor(Aggregate$Group, levels = c("SDBA", "HS", "L2L"))
Aggregate$Task <- factor(Aggregate$Task, levels = c("SCT", "MRT"))


# Create graph
Aggregate %>% 
  ggplot(aes(x = Group, y = Structure_Percentage, fill = Task)) + 
  geom_bar(position = "dodge", color = "black", stat = "identity") +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 105)) +
  geom_text(aes(label = round(Structure_Percentage)),
            position = position_dodge(width = .9),
            vjust = -0.5,
            size = 4) +
  scale_fill_manual(values = c("#56B4E9", "#F0E442")) +
  labs(x = "Group", y = "Percentage of responses with subjunctive", fill = "Task", title = "Subjunctive Use by Group and Task") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"))
