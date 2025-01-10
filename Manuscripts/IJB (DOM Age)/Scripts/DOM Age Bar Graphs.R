library(tidyverse)
library(here)


# Load data
## EPT
SDB_SCT <- read_csv(here("./CSV Files/SDB/SDB DOM EPT.csv")) %>%
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>% 
  mutate(Group = "SDBA",
         Task = "SCT")

HSA_SCT <- read_csv(here("./CSV Files/Adult HS/Adult HS DOM EPT.csv")) %>%
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>%
  mutate(Group = "HSA",
         Task = "SCT")

DLI78_SCT <- read_csv(here("./CSV Files/DLI-78/DLI-78 DOM EPT.csv")) %>%
  mutate(Group = "HS7/8",
         Task = "SCT")

MLS78_SCT <- read_csv(here("./CSV Files/MLS-78/MLS-78 DOM EPT.csv")) %>%
  mutate(Group = "HS7/8",
         Task = "SCT")

DLI5_SCT <- read_csv(here("./CSV Files/DLI-5/DLI-5 DOM EPT.csv"))  %>%
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>%
  mutate(Group = "HS5",
         Task = "SCT")

MLS5_SCT <- read_csv(here("./CSV Files/MLS-5/MLS-5 DOM EPT.csv")) %>%
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>%
  mutate(Group = "HS5",
         Task = "SCT")


## FCT
SDB_MST <- read_csv(here("./CSV Files/SDB/SDB DOM FCT.csv")) %>% 
  mutate(Group = "SDBA",
         Task = "MST")

HSA_MST <- read_csv(here("./CSV Files/Adult HS/Adult HS DOM FCT.csv")) %>%
  mutate(Group = "HSA",
         Task = "MST")

DLI78_MST <- read_csv(here("./CSV Files/DLI-78/DLI-78 DOM FCT.csv")) %>% 
  mutate(Group = "HS7/8",
         Task = "MST")

MLS78_MST <- read_csv(here("./CSV Files/MLS-78/MLS-78 DOM FCT.csv")) %>% 
  mutate(Group = "HS7/8",
         Task = "MST")

DLI5_MST <- read_csv(here("./CSV Files/DLI-5/DLI-5 DOM FCT.csv"))  %>%
  mutate(Group = "HS5",
         Task = "MST")

MLS5_MST <- read_csv(here("./CSV Files/MLS-5/MLS-5 DOM FCT.csv")) %>%
  mutate(Group = "HS5",
         Task = "MST") 


## Join age group datasets
HS78_SCT <- rbind(DLI78_SCT, MLS78_SCT)
HS78_MST <- rbind(DLI78_MST, MLS78_MST)

HS5_SCT <- rbind(DLI5_SCT, MLS5_SCT)
HS5_MST <- rbind(DLI5_MST, MLS5_MST)


# Generate averages
SDB_SCT_Structure <- aggregate(SDB_SCT$DOM_Use, list(SDB_SCT$Structure), FUN = mean, na.rm = TRUE)
SDB_SCT_Structure <- SDB_SCT_Structure %>% rename(Structure_Avg = x)
SDB_SCT_Structure <- left_join(SDB_SCT, SDB_SCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

SDB_MST_Structure <- aggregate(SDB_MST$DOM_Use, list(SDB_MST$Structure), FUN = mean, na.rm = TRUE)
SDB_MST_Structure <- SDB_MST_Structure %>% rename(Structure_Avg = x)
SDB_MST_Structure <- left_join(SDB_MST, SDB_MST_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HSA_SCT_Structure <- aggregate(HSA_SCT$DOM_Use, list(HSA_SCT$Structure), FUN = mean, na.rm = TRUE)
HSA_SCT_Structure <- HSA_SCT_Structure %>% rename(Structure_Avg = x)
HSA_SCT_Structure <- left_join(HSA_SCT, HSA_SCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HSA_MST_Structure <- aggregate(HSA_MST$DOM_Use, list(HSA_MST$Structure), FUN = mean, na.rm = TRUE)
HSA_MST_Structure <- HSA_MST_Structure %>% rename(Structure_Avg = x)
HSA_MST_Structure <- left_join(HSA_MST, HSA_MST_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS78_SCT_Structure <- aggregate(HS78_SCT$DOM_Use, list(HS78_SCT$Structure), FUN = mean, na.rm = TRUE)
HS78_SCT_Structure <- HS78_SCT_Structure %>% rename(Structure_Avg = x)
HS78_SCT_Structure <- left_join(HS78_SCT, HS78_SCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS78_MST_Structure <- aggregate(HS78_MST$DOM_Use, list(HS78_MST$Structure), FUN = mean, na.rm = TRUE)
HS78_MST_Structure <- HS78_MST_Structure %>% rename(Structure_Avg = x)
HS78_MST_Structure <- left_join(HS78_MST, HS78_MST_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS5_SCT_Structure <- aggregate(HS5_SCT$DOM_Use, list(HS5_SCT$Structure), FUN = mean, na.rm = TRUE)
HS5_SCT_Structure <- HS5_SCT_Structure %>% rename(Structure_Avg = x)
HS5_SCT_Structure <- left_join(HS5_SCT, HS5_SCT_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HS5_MST_Structure <- aggregate(HS5_MST$DOM_Use, list(HS5_MST$Structure), FUN = mean, na.rm = TRUE)
HS5_MST_Structure <- HS5_MST_Structure %>% rename(Structure_Avg = x)
HS5_MST_Structure <- left_join(HS5_MST, HS5_MST_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)


# Join aggregate dataset
EPT <- rbind(SDB_SCT_Structure, HSA_SCT_Structure, HS78_SCT_Structure, HS5_SCT_Structure)
FCT <- rbind(SDB_MST_Structure, HSA_MST_Structure, HS78_MST_Structure, HS5_MST_Structure)

Aggregate <- rbind(EPT, FCT)
Aggregate$Group <- factor(Aggregate$Group, levels = c("SDBA", "HSA", "HS7/8", "HS5"))
Aggregate$Task <- factor(Aggregate$Task, levels = c("SCT", "MST"))


# Generate bar graph
Aggregate %>% 
  ggplot(aes(x = Group, y = Structure_Percentage, fill = Task)) + 
  geom_bar(position = "dodge", color = "black", stat = "identity") +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 100)) +
  labs(x = "Group", y = "Percentage of DOM responses", fill = "Task", title = "DOM by Group and Task") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"))

