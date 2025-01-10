library(tidyverse)
library(here)


# Load subjunctive data
## EPT
HSA_EPT_Subj <- read_csv(here("./CSV Files/Adult HS/Adult HS Subjunctive EPT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HSA")

DLI78_EPT_Subj <- read_csv(here("./CSV Files/DLI-78/DLI-78 Subjunctive EPT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS7/8")

MLS78_EPT_Subj <- read_csv(here("./CSV Files/MLS-78/MLS-78 Subjunctive EPT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS7/8")

DLI5_EPT_Subj <- read_csv(here("./CSV Files/DLI-5/DLI-5 Subjunctive EPT.csv"))  %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS5")

MLS5_EPT_Subj <- read_csv(here("./CSV Files/MLS-5/MLS-5 Subjunctive EPT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS5")


## FCT
HSA_FCT_Subj <- read_csv(here("./CSV Files/Adult HS/Adult HS Subjunctive FCT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HSA")

DLI78_FCT_Subj <- read_csv(here("./CSV Files/DLI-78/DLI-78 Subjunctive FCT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS7/8")

MLS78_FCT_Subj <- read_csv(here("./CSV Files/MLS-78/MLS-78 Subjunctive FCT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS7/8")

DLI5_FCT_Subj <- read_csv(here("./CSV Files/DLI-5/DLI-5 Subjunctive FCT.csv"))  %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS5")

MLS5_FCT_Subj <- read_csv(here("./CSV Files/MLS-5/MLS-5 Subjunctive FCT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS5")


## Join datasets
EPT_Subj <- rbind(HSA_EPT_Subj, DLI78_EPT_Subj, MLS78_EPT_Subj, DLI5_EPT_Subj, MLS5_EPT_Subj)
FCT_Subj <- rbind(HSA_FCT_Subj, DLI78_FCT_Subj, MLS78_FCT_Subj, DLI5_FCT_Subj, MLS5_FCT_Subj)


# Load DOM data
## EPT
HSA_EPT_DOM <- read_csv(here("./CSV Files/Adult HS/Adult HS DOM EPT.csv")) %>%
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>% 
  mutate(Group = "HSA")

DLI78_EPT_DOM <- read_csv(here("./CSV Files/DLI-78/DLI-78 DOM EPT.csv")) %>%
  mutate(Group = "HS7/8")

MLS78_EPT_DOM <- read_csv(here("./CSV Files/MLS-78/MLS-78 DOM EPT.csv")) %>%
  mutate(Group = "HS7/8")

DLI5_EPT_DOM <- read_csv(here("./CSV Files/DLI-5/DLI-5 DOM EPT.csv"))  %>%
  mutate(Group = "HS5")

MLS5_EPT_DOM <- read_csv(here("./CSV Files/MLS-5/MLS-5 DOM EPT.csv")) %>%
  mutate(Group = "HS5")


## FCT
HSA_FCT_DOM <- read_csv(here("./CSV Files/Adult HS/Adult HS DOM FCT.csv")) %>%
  mutate(Group = "HSA")

DLI78_FCT_DOM <- read_csv(here("./CSV Files/DLI-78/DLI-78 DOM FCT.csv")) %>%
  mutate(Group = "HS7/8")

MLS78_FCT_DOM <- read_csv(here("./CSV Files/MLS-78/MLS-78 DOM FCT.csv")) %>%
  mutate(Group = "HS7/8")

DLI5_FCT_DOM <- read_csv(here("./CSV Files/DLI-5/DLI-5 DOM FCT.csv"))  %>%
  mutate(Group = "HS5")

MLS5_FCT_DOM <- read_csv(here("./CSV Files/MLS-5/MLS-5 DOM FCT.csv")) %>%
  mutate(Group = "HS5")


## Join datasets
EPT_DOM <- rbind(HSA_EPT_DOM, DLI78_EPT_DOM, MLS78_EPT_DOM, DLI5_EPT_DOM, MLS5_EPT_DOM)
FCT_DOM <- rbind(HSA_FCT_DOM, DLI78_FCT_DOM, MLS78_FCT_DOM, DLI5_FCT_DOM, MLS5_FCT_DOM)


# Create datasets for plot
## Subjunctive
EPT_Group_Subj <- EPT_Subj %>%
  filter(!is.na(Mood_Use)) %>%
  group_by(Part_ID, Group) %>%
  summarize(Production = sum(Mood_Use))

FCT_Group_Subj <- FCT_Subj %>%
  filter(!is.na(Mood_Use)) %>%
  group_by(Part_ID, Group) %>%
  summarize(Selection = sum(Mood_Use))

Aggregate_Subj = left_join(EPT_Group_Subj, FCT_Group_Subj, by = "Part_ID", "Group") %>%
  rename(Group = Group.x) %>% 
  mutate(Total = Production + Selection,
         Structure = "Subjunctive",
         Structure_SPA = "Subjuntivo")


## DOM
EPT_Group_DOM <- EPT_DOM %>%
  filter(!is.na(DOM_Use)) %>%
  group_by(Part_ID, Group) %>%
  summarize(Production = sum(DOM_Use))

FCT_Group_DOM <- FCT_DOM %>%
  filter(!is.na(DOM_Use)) %>%
  group_by(Part_ID, Group) %>%
  summarize(Selection = sum(DOM_Use))

Aggregate_DOM = left_join(EPT_Group_DOM, FCT_Group_DOM, by = "Part_ID", "Group") %>%
  rename(Group = Group.x) %>% 
  mutate(Total = Production + Selection,
         Structure = "DOM",
         Structure_SPA = "MDO")


## Join datasets
Aggregate <- rbind(Aggregate_Subj, Aggregate_DOM)
Aggregate$Group <- factor(Aggregate$Group, levels = c("HSA", "HS7/8", "HS5"))
Aggregate$Structure <- factor(Aggregate$Structure, levels = c("Subjunctive", "DOM"))
Aggregate$Structure_SPA <- factor(Aggregate$Structure_SPA, levels = c("Subjuntivo", "MDO"))


# Graphs
## English, acronyms
Aggregate %>% 
  ggplot(aes(x = Production, y = Selection)) +
  geom_jitter(mapping = aes(color = Group)) +
  facet_grid(rows = vars(Structure)) +
  scale_x_continuous(breaks = seq (0, 10, 2),
                     limits = c(-1, 11)) +
  scale_y_continuous(breaks = seq (0, 10, 2),
                     limits = c(-1, 9)) +
  labs(x = "No. of sentences with expected morphology produced", y = "Sentences with expected morphology selected", title = "Individual Rates of Subjunctive/DOM Production and Selection", color = "Group") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold", size = 10))


## English, full group names
Aggregate %>% 
  ggplot(aes(x = Production, y = Selection)) +
  geom_jitter(mapping = aes(color = Group)) +
  facet_grid(rows = vars(Structure)) +
  scale_x_continuous(breaks = seq (0, 10, 2),
                     limits = c(-1, 11)) +
  scale_y_continuous(breaks = seq (0, 10, 2),
                     limits = c(-1, 9)) +
  scale_color_discrete(labels = c("Adults", "7th/8th", "5th")) +
  labs(x = "No. of sentences with expected morphology produced", y = "Sentences with expected morphology selected", title = "Individual Rates of Subjunctive/DOM Production and Selection", color = "Group") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold", size = 10))

## Spanish
Aggregate %>% 
  ggplot(aes(x = Production, y = Selection)) +
  geom_jitter(mapping = aes(color = Group)) +
  facet_grid(rows = vars(Structure_SPA)) +
  scale_x_continuous(breaks = seq (0, 10, 2),
                     limits = c(-1, 11)) +
  scale_y_continuous(breaks = seq (0, 10, 2),
                     limits = c(-1, 9)) +
  scale_color_discrete(labels = c("HH adultos", "HH 7°/8° grado", "HH 5° grado")) +
  labs(x = "Oraciones producidas con morfología anticipada", y = "Oraciones seleccionadas con morfología anticipada", title = "Análisis individual de uso de subjuntivo y MDO", color = "Grupo") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold", size = 10))