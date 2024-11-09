library(tidyverse)


# Prepare and load data
## EPT
SDB_EPT <- read_csv("./CSV Files/SDB/SDB DOM EPT.csv") %>%
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>% 
  mutate(Group = "SDBA")

HSA_EPT <- read_csv("./CSV Files/Adult HS/Adult HS DOM EPT.csv") %>%
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>% 
  mutate(Group = "HSA")

DLI78_EPT <- read_csv("./CSV Files/DLI-78/DLI-78 DOM EPT.csv") %>%
  mutate(Group = "HS7/8")

MLS78_EPT <- read_csv("./CSV Files/MLS-78/MLS-78 DOM EPT.csv") %>%
  mutate(Group = "HS7/8")

DLI5_EPT <- read_csv("./CSV Files/DLI-5/DLI-5 DOM EPT.csv")  %>%
  mutate(Group = "HS5")

MLS5_EPT <- read_csv("./CSV Files/MLS-5/MLS-5 DOM EPT.csv") %>%
  mutate(Group = "HS5")


## FCT
SDB_FCT <- read_csv("./CSV Files/SDB/SDB DOM FCT.csv") %>%
  mutate(Group = "SDBA")

HSA_FCT <- read_csv("./CSV Files/Adult HS/Adult HS DOM FCT.csv") %>%
  mutate(Group = "HSA")

DLI78_FCT <- read_csv("./CSV Files/DLI-78/DLI-78 DOM FCT.csv") %>%
  mutate(Group = "HS7/8")

MLS78_FCT <- read_csv("./CSV Files/MLS-78/MLS-78 DOM FCT.csv") %>%
  mutate(Group = "HS7/8")

DLI5_FCT <- read_csv("./CSV Files/DLI-5/DLI-5 DOM FCT.csv")  %>%
  mutate(Group = "HS5")

MLS5_FCT <- read_csv("./CSV Files/MLS-5/MLS-5 DOM FCT.csv") %>%
  mutate(Group = "HS5")


## Join and summarize data
EPT <- rbind(SDB_EPT, HSA_EPT, DLI78_EPT, MLS78_EPT, DLI5_EPT, MLS5_EPT) %>% 
  group_by(Part_ID, Group) %>%
  summarize(Total_Subj = sum(DOM_Use, na.rm = TRUE),
            Total_Responses = sum(!is.na(DOM_Use)),
            Ratio = Total_Subj/Total_Responses) %>% 
  mutate(Task = "Production",
         Ratio = (Ratio*100))


FCT <- rbind(SDB_FCT, HSA_FCT, DLI78_FCT, MLS78_FCT, DLI5_FCT, MLS5_FCT) %>% 
  group_by(Part_ID, Group) %>%
  summarize(Total_Subj = sum(DOM_Use, na.rm = TRUE),
            Total_Responses = sum(!is.na(DOM_Use)),
            Ratio = Total_Subj/Total_Responses) %>% 
  mutate(Task = "Selection",
         Ratio = (Ratio*100))

Aggregate <- rbind(EPT, FCT)
Aggregate$Group <- factor(Aggregate$Group, levels = c("HS5", "HS7/8", "HSA", "SDBA"))


## Create heritage dataset
Aggregate_Heritage <- Aggregate %>% 
  filter(!Group == "SDBA")
Aggregate_Heritage$Group <- factor(Aggregate_Heritage$Group, c("HS5", "HS7/8", "HSA"))


# Create graphs in English
## With SDBAs
Aggregate %>% 
  ggplot(aes(x = Group, y = Ratio, fill = Task)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 100)) +
  scale_fill_manual(values = c("#56B4E9", "#F0E442")) +
  labs(x = "Group", y = "Percentage of DOM responses", title = "DOM by Age Group and Task") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"))


## Without SDBAs
Aggregate_Heritage %>% 
  ggplot(aes(x = Group, y = Ratio, fill = Task)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 100)) +
  scale_fill_manual(values = c("#56B4E9", "#F0E442")) +
  labs(x = "Group", y = "Percentage of DOM responses", title = "DOM by Age Group and Task") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"))


# Create graphs in Spanish
## With SDBAs
Aggregate %>% 
  ggplot(aes(x = Group, y = Ratio, fill = Task)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 100)) +
  scale_x_discrete(labels = c("HH 5° grado", "HH 7°/8° grado", "HH adultos", "ABDE")) +
  scale_fill_manual(values = c("#56B4E9", "#F0E442"), labels = c("Producción", "Selección")) +
  labs(x = "Grupo", y = "Porcentaje de respuestas con MDO", title = "MDO por grupo de edad y tarea", fill = "Tarea") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"))


## Without SDBAs
Aggregate_Heritage %>% 
  ggplot(aes(x = Group, y = Ratio, fill = Task)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 100)) +
  scale_x_discrete(labels = c("HH 5° grado", "HH 7°/8° grado",  "HH adultos")) +
  scale_fill_manual(values = c("#56B4E9", "#F0E442"), labels = c("Producción", "Selección")) +
  labs(x = "Grupo", y = "Porcentaje de respuestas con MDO", title = "MDO por grupo de edad y tarea", fill = "Tarea") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"))
