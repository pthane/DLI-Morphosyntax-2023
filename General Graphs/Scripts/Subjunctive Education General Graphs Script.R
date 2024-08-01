library(tidyverse)


# Prepare and load data
## EPT
SDB_EPT <- read_csv("./CSV Files/SDB/SDB Subjunctive EPT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "SDBA")

DLI78_EPT <- read_csv("./CSV Files/DLI-78/DLI-78 Subjunctive EPT.csv") %>% 
  filter(Property == "Intensional subjunctive")

MLS78_EPT <- read_csv("./CSV Files/MLS-78/MLS-78 Subjunctive EPT.csv") %>% 
  filter(Property == "Intensional subjunctive")

DLI5_EPT <- read_csv("./CSV Files/DLI-5/DLI-5 Subjunctive EPT.csv") %>% 
  filter(Property == "Intensional subjunctive")

MLS5_EPT <- read_csv("./CSV Files/MLS-5/MLS-5 Subjunctive EPT.csv") %>% 
  filter(Property == "Intensional subjunctive")


## FCT
SDB_FCT <- read_csv("./CSV Files/SDB/SDB Subjunctive FCT.csv") %>% 
  filter(Property == "Intensional subjunctive") %>%
  mutate(Group = "SDBA")

DLI78_FCT <- read_csv("./CSV Files/DLI-78/DLI-78 Subjunctive FCT.csv") %>% 
  filter(Property == "Intensional subjunctive")

MLS78_FCT <- read_csv("./CSV Files/MLS-78/MLS-78 Subjunctive FCT.csv") %>% 
  filter(Property == "Intensional subjunctive")

DLI5_FCT <- read_csv("./CSV Files/DLI-5/DLI-5 Subjunctive FCT.csv") %>% 
  filter(Property == "Intensional subjunctive")

MLS5_FCT <- read_csv("./CSV Files/MLS-5/MLS-5 Subjunctive FCT.csv") %>% 
  filter(Property == "Intensional subjunctive")


## Join and summarize data
EPT <- rbind(SDB_EPT, DLI78_EPT, MLS78_EPT, DLI5_EPT, MLS5_EPT) %>% 
  group_by(Part_ID, Group) %>%
  summarize(Total_Subj = sum(Mood_Use, na.rm = TRUE),
            Total_Responses = sum(!is.na(Mood_Use)),
            Ratio = Total_Subj/Total_Responses) %>% 
  mutate(Task = "Production",
         Ratio = (Ratio*100))


FCT <- rbind(SDB_FCT, DLI78_FCT, MLS78_FCT, DLI5_FCT, MLS5_FCT) %>% 
  group_by(Part_ID, Group) %>%
  summarize(Total_Subj = sum(Mood_Use, na.rm = TRUE),
            Total_Responses = sum(!is.na(Mood_Use)),
            Ratio = Total_Subj/Total_Responses) %>% 
  mutate(Task = "Selection",
         Ratio = (Ratio*100))

Aggregate <- rbind(EPT, FCT)
Aggregate$Group <- factor(Aggregate$Group, levels = c("SDBA", "DLI-7/8", "MLS-7/8", "DLI-5", "MLS-5"))


## Create heritage dataset
Aggregate_Heritage <- Aggregate %>% 
  filter(!Group == "SDBA")
Aggregate_Heritage$Group <- factor(Aggregate_Heritage$Group, c("DLI-7/8", "MLS-7/8", "DLI-5", "MLS-5"))


# Create graphs in English
## With SDBAs
Aggregate %>% 
  ggplot(aes(x = Group, y = Ratio, fill = Task)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 100)) +
  scale_fill_manual(values = c("#56B4E9", "#F0E442")) +
  labs(x = "Group", y = "Percentage of Subjunctive responses", title = "Subjunctive by Group and Task") +
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
  labs(x = "Group", y = "Percentage of Subjunctive responses", title = "Subjunctive by Group and Task") +
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
  scale_x_discrete(labels = c("ABDE", "7°/8° inmersión", "7°/8° monolingüe", "5° inmersión", "5° monolingüe")) +
  scale_fill_manual(values = c("#56B4E9", "#F0E442"), labels = c("Producción", "Selección")) +
  labs(x = "Grupo", y = "Porcentaje de respuestas con subjuntivo", title = "Subjuntivo por grupo y tarea", fill = "Tarea") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"))


## Without SDBAs
Aggregate_Heritage %>% 
  ggplot(aes(x = Group, y = Ratio, fill = Task)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 100)) +
  scale_x_discrete(labels = c("7°/8° inmersión", "7°/8° monolingüe", "5° inmersión", "5° monolingüe")) +
  scale_fill_manual(values = c("#56B4E9", "#F0E442"), labels = c("Producción", "Selección")) +
  labs(x = "Grupo", y = "Porcentaje de respuestas con subjuntivo", title = "Subjuntivo por grupo y tarea", fill = "Tarea") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"))
