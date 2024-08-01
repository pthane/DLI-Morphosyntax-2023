library(tidyverse)


# Load subjunctive data
## EPT
HSA_EPT_Subj <- read_csv("./CSV Files/Adult HS/Adult HS Subjunctive EPT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HSA")

DLI78_EPT_Subj <- read_csv("./CSV Files/DLI-78/DLI-78 Subjunctive EPT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS7/8")

MLS78_EPT_Subj <- read_csv("./CSV Files/MLS-78/MLS-78 Subjunctive EPT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS7/8")

DLI5_EPT_Subj <- read_csv("./CSV Files/DLI-5/DLI-5 Subjunctive EPT.csv")  %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS5")

MLS5_EPT_Subj <- read_csv("./CSV Files/MLS-5/MLS-5 Subjunctive EPT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS5")


## FCT
HSA_FCT_Subj <- read_csv("./CSV Files/Adult HS/Adult HS Subjunctive FCT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HSA")

DLI78_FCT_Subj <- read_csv("./CSV Files/DLI-78/DLI-78 Subjunctive FCT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS7/8")

MLS78_FCT_Subj <- read_csv("./CSV Files/MLS-78/MLS-78 Subjunctive FCT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS7/8")

DLI5_FCT_Subj <- read_csv("./CSV Files/DLI-5/DLI-5 Subjunctive FCT.csv")  %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS5")

MLS5_FCT_Subj <- read_csv("./CSV Files/MLS-5/MLS-5 Subjunctive FCT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS5")


# Load DOM data
## EPT
HSA_EPT_DOM <- read_csv("./CSV Files/Adult HS/Adult HS DOM EPT.csv") %>%
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>% 
  mutate(Group = "HSA")

DLI78_EPT_DOM <- read_csv("./CSV Files/DLI-78/DLI-78 DOM EPT.csv") %>%
  mutate(Group = "HS7/8")

MLS78_EPT_DOM <- read_csv("./CSV Files/MLS-78/MLS-78 DOM EPT.csv") %>%
  mutate(Group = "HS7/8")

DLI5_EPT_DOM <- read_csv("./CSV Files/DLI-5/DLI-5 DOM EPT.csv")  %>%
  mutate(Group = "HS5")

MLS5_EPT_DOM <- read_csv("./CSV Files/MLS-5/MLS-5 DOM EPT.csv") %>%
  mutate(Group = "HS5")


## FCT
HSA_FCT_DOM <- read_csv("./CSV Files/Adult HS/Adult HS DOM FCT.csv") %>%
  mutate(Group = "HSA")

DLI78_FCT_DOM <- read_csv("./CSV Files/DLI-78/DLI-78 DOM FCT.csv") %>%
  mutate(Group = "HS7/8")

MLS78_FCT_DOM <- read_csv("./CSV Files/MLS-78/MLS-78 DOM FCT.csv") %>%
  mutate(Group = "HS7/8")

DLI5_FCT_DOM <- read_csv("./CSV Files/DLI-5/DLI-5 DOM FCT.csv")  %>%
  mutate(Group = "HS5")

MLS5_FCT_DOM <- read_csv("./CSV Files/MLS-5/MLS-5 DOM FCT.csv") %>%
  mutate(Group = "HS5")


# Calculate ratios
## Subj
EPT_Subj <- rbind(HSA_EPT_Subj, DLI78_EPT_Subj, MLS78_EPT_Subj, DLI5_EPT_Subj, MLS5_EPT_Subj) %>% 
  group_by(Part_ID, Group, Use_Joint) %>%
  summarize(Total_Subj = sum(Mood_Use, na.rm = TRUE),
            Total_Responses = sum(!is.na(Mood_Use)),
            Ratio = Total_Subj/Total_Responses) %>% 
  mutate(Task = "Production",
         Task_SPA = "Producción",
         Structure = "Subjunctive",
         Structure_SPA = "Subjuntivo",
         Ratio = (Ratio*100))


FCT_Subj <- rbind(HSA_FCT_Subj, DLI78_FCT_Subj, MLS78_FCT_Subj, DLI5_FCT_Subj, MLS5_FCT_Subj) %>% 
  group_by(Part_ID, Group, Use_Joint) %>%
  summarize(Total_Subj = sum(Mood_Use, na.rm = TRUE),
            Total_Responses = sum(!is.na(Mood_Use)),
            Ratio = Total_Subj/Total_Responses) %>% 
  mutate(Task = "Selection",
         Task_SPA = "Selección",
         Structure = "Subjunctive",
         Structure_SPA = "Subjuntivo",
         Ratio = (Ratio*100))

Aggregate_Subj <- rbind(EPT_Subj, FCT_Subj)
Aggregate_Subj$Group <- factor(Aggregate_Subj$Group, levels = c("HSA", "HS7/8", "HS5"))


## DOM
EPT_DOM <- rbind(HSA_EPT_DOM, DLI78_EPT_DOM, MLS78_EPT_DOM, DLI5_EPT_DOM, MLS5_EPT_DOM) %>% 
  group_by(Part_ID, Group, Use_Joint) %>%
  summarize(Total_Subj = sum(DOM_Use, na.rm = TRUE),
            Total_Responses = sum(!is.na(DOM_Use)),
            Ratio = Total_Subj/Total_Responses) %>% 
  mutate(Task = "Production",
         Task_SPA = "Producción",
         Structure = "DOM",
         Structure_SPA = "MDO",
         Ratio = (Ratio*100))


FCT_DOM <- rbind(HSA_FCT_DOM, DLI78_FCT_DOM, MLS78_FCT_DOM, DLI5_FCT_DOM, MLS5_FCT_DOM) %>% 
  group_by(Part_ID, Group, Use_Joint) %>%
  summarize(Total_Subj = sum(DOM_Use, na.rm = TRUE),
            Total_Responses = sum(!is.na(DOM_Use)),
            Ratio = Total_Subj/Total_Responses) %>% 
  mutate(Task = "Selection",
         Task_SPA = "Selección",
         Structure = "DOM",
         Structure_SPA = "MDO",
         Ratio = (Ratio*100))

Aggregate_DOM <- rbind(EPT_DOM, FCT_DOM)


# Create final dataset
Aggregate <- rbind(Aggregate_Subj, Aggregate_DOM)
Aggregate$Structure <- factor(Aggregate$Structure, levels = c("Subjunctive", "DOM"))
Aggregate$Structure_SPA <- factor(Aggregate$Structure_SPA, levels = c("Subjuntivo", "MDO"))
Aggregate$Group <- factor(Aggregate$Group, levels = c("HSA", "HS7/8", "HS5"))


# Create graphs
## Spanish
Aggregate %>% 
  ggplot(aes(x = Use_Joint, y = Ratio)) + 
  geom_jitter(mapping = aes(color = Group)) +
  facet_grid(rows = vars(Structure),
             cols = vars(Task)) +
  geom_smooth(method = glm) +
  scale_x_continuous(breaks = seq (0, 30, 5),
                     limits = c(-1, 31)) +
  labs(x = "Frequency of use of Spanish", y = "Percentage of responses with expected morphology", 
       title = "Subjunctive/DOM by Frequency of Use, Group, and Task") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"))


## Spanish
Aggregate %>% 
  ggplot(aes(x = Use_Joint, y = Ratio)) + 
  geom_jitter(mapping = aes(color = Group)) +
  facet_grid(rows = vars(Structure_SPA),
             cols = vars(Task_SPA)) +
  geom_smooth(method = glm) +
  scale_x_continuous(breaks = seq (0, 30, 5),
                     limits = c(-1, 31)) +
  scale_color_discrete(labels = c("HH adultos", "HH 7°/8° grado", "HH 5° grado")) +
  labs(x = "Frecuencia de uso del español", y = "Porcentaje de respuestas con la morfología anticipada", 
       title = "Subjuntivo/MDO por frecuencia de uso, grupo, y tarea", color= "Grupo") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"))

