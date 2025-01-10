library(tidyverse)
library(here)


# Prepare subjunctive data
## Load production
SDB_Subjunctive_EPT <- read_csv(here("./CSV Files/SDB/SDB Subjunctive EPT.csv")) %>% 
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Child_Group = "SDBA",
         Group = "SDBA",
         Task = "Production",
         School_Group = "SDBA",
         Age = "Adults")

HSA_Subjunctive_EPT <- read_csv(here("./CSV Files/Adult HS/Adult HS Subjunctive EPT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Child_Group = "HSA",
         Group = "HSA",
         Task = "Production",
         School_Group = "HSA",
         Age = "Adults")


DLI78_Subjunctive_EPT <- read_csv(here("./CSV Files/DLI-78/DLI-78 Subjunctive EPT.csv")) %>% 
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Child_Group = "DLI-7/8",
         Group = "HS7/8",
         Task = "Production",
         School_Group = "Immersion",
         Age = "7th/8th")

MLS78_Subjunctive_EPT <- read_csv(here("./CSV Files/MLS-78/MLS-78 Subjunctive EPT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Child_Group = "MLS-7/8",
         Group = "HS7/8",
         Task = "Production",
         School_Group = "Monolingual",
         Age = "7th/8th")

DLI5_Subjunctive_EPT <- read_csv(here("./CSV Files/DLI-5/DLI-5 Subjunctive EPT.csv")) %>% 
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Child_Group = "DLI-5",
         Group = "HS5",
         Task = "Production",
         School_Group = "Immersion",
         Age = "5th")

MLS5_Subjunctive_EPT <- read_csv(here("./CSV Files/MLS-5/MLS-5 Subjunctive EPT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Child_Group = "MLS-5",
         Group = "HS5",
         Task = "Production",
         School_Group = "Monolingual",
         Age = "5th")


## Load selection
SDB_Subjunctive_FCT <- read_csv(here("./CSV Files/SDB/SDB Subjunctive FCT.csv")) %>% 
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Child_Group = "SDBA",
         Group = "SDBA",
         Task = "Selection",
         School_Group = "SDBA",
         Age = "Adults")

HSA_Subjunctive_FCT <- read_csv(here("./CSV Files/Adult HS/Adult HS Subjunctive FCT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Child_Group = "HSA",
         Group = "HSA",
         Task = "Selection",
         School_Group = "HSA",
         Age = "Adults")


DLI78_Subjunctive_FCT <- read_csv(here("./CSV Files/DLI-78/DLI-78 Subjunctive FCT.csv")) %>% 
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Child_Group = "DLI-7/8",
         Group = "HS7/8",
         Task = "Selection",
         School_Group = "Immersion",
         Age = "7th/8th")

MLS78_Subjunctive_FCT <- read_csv(here("./CSV Files/MLS-78/MLS-78 Subjunctive FCT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Child_Group = "MLS-7/8",
         Group = "HS7/8",
         Task = "Selection",
         School_Group = "Monolingual",
         Age = "7th/8th")

DLI5_Subjunctive_FCT <- read_csv(here("./CSV Files/DLI-5/DLI-5 Subjunctive FCT.csv")) %>% 
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Child_Group = "DLI-5",
         Group = "HS5",
         Task = "Selection",
         School_Group = "Immersion",
         Age = "5th")

MLS5_Subjunctive_FCT <- read_csv(here("./CSV Files/MLS-5/MLS-5 Subjunctive FCT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Child_Group = "MLS-5",
         Group = "HS5",
         Task = "Selection",
         School_Group = "Monolingual",
         Age = "5th")


## Join datasets
Subjunctive_EPT <- rbind(SDB_Subjunctive_EPT, HSA_Subjunctive_EPT, DLI78_Subjunctive_EPT, MLS78_Subjunctive_EPT, DLI5_Subjunctive_EPT, MLS5_Subjunctive_EPT) %>% 
  group_by(Part_ID, Group) %>%
  summarize(Total = sum(Mood_Use, na.rm = TRUE),
            Total_Responses = sum(!is.na(Mood_Use)),
            Ratio = Total/Total_Responses) %>% 
  mutate(Task = "Production",
         Ratio = (Ratio*100))


Subjunctive_FCT <- rbind(SDB_Subjunctive_FCT, HSA_Subjunctive_FCT, DLI78_Subjunctive_FCT, MLS78_Subjunctive_FCT, DLI5_Subjunctive_FCT, MLS5_Subjunctive_FCT) %>% 
  group_by(Part_ID, Group) %>%
  summarize(Total = sum(Mood_Use, na.rm = TRUE),
            Total_Responses = sum(!is.na(Mood_Use)),
            Ratio = Total/Total_Responses) %>% 
  mutate(Task = "Selection",
         Ratio = (Ratio*100))

Subjunctive_Aggregate <- rbind(Subjunctive_EPT, Subjunctive_FCT) %>% 
  mutate(Structure = "Subjunctive")


# DOM data
## Load production
SDB_DOM_EPT <- read_csv(here("./CSV Files/SDB/SDB DOM EPT.csv")) %>%
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>% 
  mutate(Child_Group = "SDBA",
         Group = "SDBA",
         Task = "Production",
         School_Group = "SDBA",
         Age = "Adults")

HSA_DOM_EPT <- read_csv(here("./CSV Files/Adult HS/Adult HS DOM EPT.csv")) %>%
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>% 
  mutate(Child_Group = "HSA",
         Group = "HSA",
         Task = "Production",
         School_Group = "HSA",
         Age = "Adults")

DLI78_DOM_EPT <- read_csv(here("./CSV Files/DLI-78/DLI-78 DOM EPT.csv")) %>% 
  mutate(Child_Group = "DLI-7/8",
         Group = "HS7/8",
         Task = "Production",
         School_Group = "Immersion",
         Age = "7th/8th")

MLS78_DOM_EPT <- read_csv(here("./CSV Files/MLS-78/MLS-78 DOM EPT.csv")) %>%
  mutate(Child_Group = "MLS-7/8",
         Group = "HS7/8",
         Task = "Production",
         School_Group = "Monolingual",
         Age = "7th/8th")

DLI5_DOM_EPT <- read_csv(here("./CSV Files/DLI-5/DLI-5 DOM EPT.csv")) %>% 
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>% 
  mutate(Child_Group = "DLI-5",
         Group = "HS5",
         Task = "Production",
         School_Group = "Immersion",
         Age = "5th")

MLS5_DOM_EPT <- read_csv(here("./CSV Files/MLS-5/MLS-5 DOM EPT.csv")) %>%
  mutate(Child_Group = "MLS-5",
         Group = "HS5",
         Task = "Production",
         School_Group = "Monolingual",
         Age = "5th")


## Load selection
SDB_DOM_FCT <- read_csv(here("./CSV Files/SDB/SDB DOM FCT.csv")) %>%
  mutate(Child_Group = "SDBA",
         Group = "SDBA",
         Task = "Selection",
         School_Group = "SDBA",
         Age = "Adults")

HSA_DOM_FCT <- read_csv(here("./CSV Files/Adult HS/Adult HS DOM FCT.csv")) %>%
  mutate(Child_Group = "HSA",
         Group = "HSA",
         Task = "Selection",
         School_Group = "HSA",
         Age = "Adults")

DLI78_DOM_FCT <- read_csv(here("./CSV Files/DLI-78/DLI-78 DOM FCT.csv")) %>% 
  mutate(Child_Group = "DLI-7/8",
         Group = "HS7/8",
         Task = "Selection",
         School_Group = "Immersion",
         Age = "7th/8th")

MLS78_DOM_FCT <- read_csv(here("./CSV Files/MLS-78/MLS-78 DOM FCT.csv")) %>%
  mutate(Child_Group = "MLS-7/8",
         Group = "HS7/8",
         Task = "Selection",
         School_Group = "Monolingual",
         Age = "7th/8th")

DLI5_DOM_FCT <- read_csv(here("./CSV Files/DLI-5/DLI-5 DOM FCT.csv")) %>% 
  filter(Item %in% c("FCT-02", "FCT-08", "FCT-12", "FCT-14", "FCT-20", "FCT-24", "FCT-26", "FCT-32", "FCT-38", "FCT-42")) %>% 
  mutate(Child_Group = "DLI-5",
         Group = "HS5",
         Task = "Selection",
         School_Group = "Immersion",
         Age = "5th")

MLS5_DOM_FCT <- read_csv(here("./CSV Files/MLS-5/MLS-5 DOM FCT.csv")) %>%
  mutate(Child_Group = "MLS-5",
         Group = "HS5",
         Task = "Selection",
         School_Group = "Monolingual",
         Age = "5th")


## Join datasets
DOM_EPT <- rbind(SDB_DOM_EPT, HSA_DOM_EPT, DLI78_DOM_EPT, MLS78_DOM_EPT, DLI5_DOM_EPT, MLS5_DOM_EPT) %>% 
  group_by(Part_ID, Group, Task) %>%
  summarize(Total = sum(DOM_Use, na.rm = TRUE),
            Total_Responses = sum(!is.na(DOM_Use)),
            Ratio = Total/Total_Responses) %>% 
  mutate(Task = "Production",
         Ratio = (Ratio*100))

DOM_FCT <- rbind(SDB_DOM_FCT, HSA_DOM_FCT, DLI78_DOM_FCT, MLS78_DOM_FCT, DLI5_DOM_FCT, MLS5_DOM_FCT) %>% 
  group_by(Part_ID, Group, Task) %>%
  summarize(Total = sum(DOM_Use, na.rm = TRUE),
            Total_Responses = sum(!is.na(DOM_Use)),
            Ratio = Total/Total_Responses) %>% 
  mutate(Task = "Selection",
         Ratio = (Ratio*100))

DOM_Aggregate <- rbind(DOM_EPT, DOM_FCT) %>% 
  mutate(Structure = "DOM")


# Join all datasets
Master <- rbind(Subjunctive_Aggregate, DOM_Aggregate)
Master$Structure <- factor(Master$Structure, levels = c("Subjunctive", "DOM"))
Master$Group <- factor(Master$Group, levels = c("HS5", "HS7/8", "HSA", "SDBA"))


# Create graph
Age_Boxplot <- Master %>% 
  ggplot(aes(x = Group, y = Ratio, fill = Task)) +
  geom_boxplot() +
  facet_grid(rows = vars(Structure)) +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 100)) +
  scale_fill_manual(values = c("black", "white")) +
  labs(x = "Group", y = "Percentage of subjunctive/DOM responses", title = "Subjunctive/DOM by Age Group and Task") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"))

Age_Boxplot

ggsave(filename = here("./Manuscripts/GASLA17 Proceedings/Graphs/GASLA17 Age Boxplot.pdf"),
       plot = Age_Boxplot,
       device = "pdf",
       width = 6.5,
       height = 3.5,
       units = "in")
