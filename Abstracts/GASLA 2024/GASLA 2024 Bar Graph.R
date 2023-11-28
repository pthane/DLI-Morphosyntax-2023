library(tidyverse)

# Load DOM data
## EPT
SDB_EPT <- read.csv("./CSV Files/SDB/SDB DOM EPT.csv") %>% 
  mutate(Group = "SDBA",
         Task = "Production")

HSA_EPT <- read.csv("./CSV Files/Adult HS/Adult HS DOM EPT.csv") %>% 
  mutate(Group = "HSA",
         Task = "Production")

DLI78_EPT <- read.csv("./CSV Files/DLI-78/DLI-78 DOM EPT.csv") %>% 
  mutate(Group = "HS7/8",
         Task = "Production")

MLS78_EPT <- read.csv("./CSV Files/MLS-78/MLS-78 DOM EPT.csv") %>% 
  filter(!School == "GBCS") %>%
  mutate(Group = "HS7/8",
         Task = "Production")

DLI5_EPT <- read.csv("./CSV Files/MLS-5/MLS-5 DOM EPT.csv") %>% 
  mutate(Group = "HS5",
         Task = "Production")

MLS5_EPT <- read.csv("./CSV Files/MLS-5/MLS-5 DOM EPT.csv") %>% 
  filter(!School == "GBCS") %>% 
  mutate(Group = "HS5",
         Task = "Production")


## FCT
SDB_FCT <- read.csv("./CSV Files/SDB/SDB DOM FCT.csv") %>% 
  mutate(Group = "SDBA",
         Task = "Forced Choice")

HSA_FCT <- read.csv("./CSV Files/Adult HS/Adult HS DOM FCT.csv") %>% 
  mutate(Group = "HSA",
         Task = "Forced Choice")

DLI78_FCT <- read.csv("./CSV Files/DLI-78/DLI-78 DOM FCT.csv") %>% 
  mutate(Group = "HS7/8",
         Task = "Forced Choice")

MLS78_FCT <- read.csv("./CSV Files/MLS-78/MLS-78 DOM FCT.csv") %>% 
  filter(!School == "GBCS") %>%
  mutate(Group = "HS7/8",
         Task = "Forced Choice")

DLI5_FCT <- read.csv("./CSV Files/MLS-5/MLS-5 DOM FCT.csv") %>% 
  mutate(Group = "HS5",
         Task = "Forced Choice")

MLS5_FCT <- read.csv("./CSV Files/MLS-5/MLS-5 DOM FCT.csv") %>% 
  filter(!School == "GBCS") %>% 
  mutate(Group = "HS5",
         Task = "Forced Choice")

## Join HS datasets
HS78_EPT <- rbind(DLI78_EPT, MLS78_EPT)
HS78_FCT <- rbind(DLI78_FCT, MLS78_FCT)

HS5_EPT <- rbind(DLI5_EPT, MLS5_EPT)
HS5_FCT <- rbind(DLI5_FCT, MLS5_FCT)


# Generate DOM participant averages for graph
## SDBA
SDB_EPT_Structure <- aggregate(SDB_EPT$DOM_Use, list(SDB_EPT$Structure), FUN = mean, na.rm = TRUE)
SDB_EPT_Structure <- SDB_EPT_Structure %>% rename(Structure_Avg = x)
SDB_EPT_Structure <- left_join(SDB_EPT, SDB_EPT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

SDB_FCT_Structure <- aggregate(SDB_FCT$DOM_Use, list(SDB_FCT$Structure), FUN = mean, na.rm = TRUE)
SDB_FCT_Structure <- SDB_FCT_Structure %>% rename(Structure_Avg = x)
SDB_FCT_Structure <- left_join(SDB_FCT, SDB_FCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## HSA
HSA_EPT_Structure <- aggregate(HSA_EPT$DOM_Use, list(HSA_EPT$Structure), FUN = mean, na.rm = TRUE)
HSA_EPT_Structure <- HSA_EPT_Structure %>% rename(Structure_Avg = x)
HSA_EPT_Structure <- left_join(HSA_EPT, HSA_EPT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HSA_FCT_Structure <- aggregate(HSA_FCT$DOM_Use, list(HSA_FCT$Structure), FUN = mean, na.rm = TRUE)
HSA_FCT_Structure <- HSA_FCT_Structure %>% rename(Structure_Avg = x)
HSA_FCT_Structure <- left_join(HSA_FCT, HSA_FCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## HS7/8
HS78_EPT_Structure <- aggregate(HS78_EPT$DOM_Use, list(HS78_EPT$Structure), FUN = mean, na.rm = TRUE)
HS78_EPT_Structure <- HS78_EPT_Structure %>% rename(Structure_Avg = x)
HS78_EPT_Structure <- left_join(HS78_EPT, HS78_EPT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS78_FCT_Structure <- aggregate(HS78_FCT$DOM_Use, list(HS78_FCT$Structure), FUN = mean, na.rm = TRUE)
HS78_FCT_Structure <- HS78_FCT_Structure %>% rename(Structure_Avg = x)
HS78_FCT_Structure <- left_join(HS78_FCT, HS78_FCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


## HS5
HS5_EPT_Structure <- aggregate(HS5_EPT$DOM_Use, list(HS5_EPT$Structure), FUN = mean, na.rm = TRUE)
HS5_EPT_Structure <- HS5_EPT_Structure %>% rename(Structure_Avg = x)
HS5_EPT_Structure <- left_join(HS5_EPT, HS5_EPT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS5_FCT_Structure <- aggregate(HS5_FCT$DOM_Use, list(HS5_FCT$Structure), FUN = mean, na.rm = TRUE)
HS5_FCT_Structure <- HS5_FCT_Structure %>% rename(Structure_Avg = x)
HS5_FCT_Structure <- left_join(HS5_FCT, HS5_FCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


# Rejoin datasets after averages
EPT <- rbind(SDB_EPT_Structure, HSA_EPT_Structure, HS78_EPT_Structure, HS5_EPT_Structure)
FCT <- rbind(SDB_FCT_Structure, HSA_FCT_Structure, HS78_FCT_Structure, HS5_FCT_Structure)

Master <- rbind(EPT, FCT)
Master$Group <- factor(Master$Group, levels = c("SDBA", "HSA", "HS7/8", "HS5"))
Master$Task <- factor(Master$Task, levels = c("Production", "Forced Choice"))


# Generate bar graph
Master %>% 
  ggplot(aes(x = Group, y = Structure_Percentage, fill = Task)) +
  geom_bar(position = "dodge", color = "black", stat = "identity") +
  scale_y_continuous(breaks = seq(0, 100, 20),
                     limits = c(0, 105)) +
  geom_text(aes(label = round(Structure_Percentage)),
            position = position_dodge(width = .9),
            vjust = -0.5,
            size = 3.5) +
  scale_fill_manual(values = c("#56B4E9", "#F0E442")) +
  labs(x = "Group", y = "Percentage of DOM responses", fill = "Task", title = "DOM by Group and Task") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text.x = element_text(face = "bold"))  
