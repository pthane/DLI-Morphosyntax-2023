library(tidyverse)


# Load DOM data
## EPT
HSA_DOM_EPT <- read_csv("./CSV Files/Adult HS/Adult HS DOM EPT.csv") %>%
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>%
  mutate(Group = "HSA")

DLI78_DOM_EPT <- read_csv("./CSV Files/DLI-78/DLI-78 DOM EPT.csv") %>%
  mutate(Group = "HS7/8")

MLS78_DOM_EPT <- read_csv("./CSV Files/MLS-78/MLS-78 DOM EPT.csv") %>%
  mutate(Group = "HS7/8")

DLI5_DOM_EPT <- read_csv("./CSV Files/DLI-5/DLI-5 DOM EPT.csv")  %>%
  mutate(Group = "HS5")

MLS5_DOM_EPT <- read_csv("./CSV Files/MLS-5/MLS-5 DOM EPT.csv") %>%
  mutate(Group = "HS5") 


## FCT
HSA_DOM_FCT <- read_csv("./CSV Files/Adult HS/Adult HS DOM FCT.csv") %>%
  mutate(Group = "HSA")

DLI78_DOM_FCT <- read_csv("./CSV Files/DLI-78/DLI-78 DOM FCT.csv") %>%
  mutate(Group = "HS7/8")

MLS78_DOM_FCT <- read_csv("./CSV Files/MLS-78/MLS-78 DOM FCT.csv") %>%
  mutate(Group = "HS7/8")

DLI5_DOM_FCT <- read_csv("./CSV Files/DLI-5/DLI-5 DOM FCT.csv")  %>%
  mutate(Group = "HS5")

MLS5_DOM_FCT <- read_csv("./CSV Files/MLS-5/MLS-5 DOM FCT.csv") %>%
  mutate(Group = "HS5") 


## Rejoin dataframes
EPT_DOM <- rbind(HSA_DOM_EPT, DLI78_DOM_EPT, MLS78_DOM_EPT, DLI5_DOM_EPT, MLS5_DOM_EPT) %>% 
  mutate(Task = "Production")
FCT_DOM <- rbind(HSA_DOM_FCT, DLI78_DOM_FCT, MLS78_DOM_FCT, DLI5_DOM_FCT, MLS5_DOM_FCT) %>% 
  mutate(Task = "Multiple choice")


# Load subjunctive data
## EPT
HSA_Subj_EPT <- read_csv("./CSV Files/Adult HS/Adult HS Subjunctive EPT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HSA")

DLI78_Subj_EPT <- read_csv("./CSV Files/DLI-78/DLI-78 Subjunctive EPT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS7/8")

MLS78_Subj_EPT <- read_csv("./CSV Files/MLS-78/MLS-78 Subjunctive EPT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS7/8")

DLI5_Subj_EPT <- read_csv("./CSV Files/DLI-5/DLI-5 Subjunctive EPT.csv")  %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS5")

MLS5_Subj_EPT <- read_csv("./CSV Files/MLS-5/MLS-5 Subjunctive EPT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS5") 


## FCT
HSA_Subj_FCT <- read_csv("./CSV Files/Adult HS/Adult HS Subjunctive FCT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HSA")

DLI78_Subj_FCT <- read_csv("./CSV Files/DLI-78/DLI-78 Subjunctive FCT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS7/8")

MLS78_Subj_FCT <- read_csv("./CSV Files/MLS-78/MLS-78 Subjunctive FCT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS7/8")

DLI5_Subj_FCT <- read_csv("./CSV Files/DLI-5/DLI-5 Subjunctive FCT.csv")  %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS5")

MLS5_Subj_FCT <- read_csv("./CSV Files/MLS-5/MLS-5 Subjunctive FCT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS5") 


## Rejoin dataframes
EPT_Subj <- rbind(HSA_Subj_EPT, DLI78_Subj_EPT, MLS78_Subj_EPT, DLI5_Subj_EPT, MLS5_Subj_EPT) %>% 
  mutate(Task = "Production")
FCT_Subj <- rbind(HSA_Subj_FCT, DLI78_Subj_FCT, MLS78_Subj_FCT, DLI5_Subj_FCT, MLS5_Subj_FCT) %>% 
  mutate(Task = "Multiple choice")


# Generate aggregate dataset for DOM
## Participant averages
EPT_DOM_Part_Avg <- aggregate(EPT_DOM$DOM_Use, list(EPT_DOM$Part_ID), FUN = sum, na.rm = TRUE)
EPT_DOM_Part_Avg <- EPT_DOM_Part_Avg %>% rename(Part_Avg = x)
EPT_DOM_Part_Avg <- left_join(EPT_DOM, EPT_DOM_Part_Avg, by = c("Part_ID" = "Group.1"))

FCT_DOM_Part_Avg <- aggregate(FCT_DOM$DOM_Use, list(FCT_DOM$Part_ID), FUN = sum, na.rm = TRUE)
FCT_DOM_Part_Avg <- FCT_DOM_Part_Avg %>% rename(Part_Avg = x)
FCT_DOM_Part_Avg <- left_join(FCT_DOM, FCT_DOM_Part_Avg, by = c("Part_ID" = "Group.1"))


## Select individual item for ease of viewing
EPT_DOM_Modified <- EPT_DOM_Part_Avg %>% 
  filter(Item == "EPT-02")

FCT_DOM_Modified <- FCT_DOM_Part_Avg %>% 
  filter(Item == "FCT-11")


## Rejoin dataframes
Aggregate_DOM <- rbind(EPT_DOM_Modified, FCT_DOM_Modified)
Aggregate_DOM$Group <- factor(Aggregate_DOM$Group, levels = c("HSA", "HS7/8", "HS5"))
Aggregate_DOM$Task <- factor(Aggregate_DOM$Task, levels = c("Production", "Multiple choice"))


# Generate aggregate dataset for subjunctive
## Participant averages
EPT_Subj_Part_Avg <- aggregate(EPT_Subj$Mood_Use, list(EPT_Subj$Part_ID), FUN = sum, na.rm = TRUE)
EPT_Subj_Part_Avg <- EPT_Subj_Part_Avg %>% rename(Part_Avg = x)
EPT_Subj_Part_Avg <- left_join(EPT_Subj, EPT_Subj_Part_Avg, by = c("Part_ID" = "Group.1"))

FCT_Subj_Part_Avg <- aggregate(FCT_Subj$Mood_Use, list(FCT_Subj$Part_ID), FUN = sum, na.rm = TRUE)
FCT_Subj_Part_Avg <- FCT_Subj_Part_Avg %>% rename(Part_Avg = x)
FCT_Subj_Part_Avg <- left_join(FCT_Subj, FCT_Subj_Part_Avg, by = c("Part_ID" = "Group.1"))


## Select individual item for ease of viewing
EPT_Subj_Modified <- EPT_Subj_Part_Avg %>% 
  filter(Item == "EPT-02")

FCT_Subj_Modified <- FCT_Subj_Part_Avg %>% 
  filter(Item == "FCT-02")


## Rejoin dataframes
Aggregate_Subj <- rbind(EPT_Subj_Modified, FCT_Subj_Modified)
Aggregate_Subj$Group <- factor(Aggregate_Subj$Group, levels = c("HSA", "HS7/8", "HS5"))
Aggregate_Subj$Task <- factor(Aggregate_Subj$Task, levels = c("Production", "Multiple choice"))


# Generate graphs
## DOM
Aggregate_DOM %>% 
  ggplot(aes(x = BESA_Other, y = Part_Avg)) + 
  geom_jitter(mapping = aes(color = Group)) +
  geom_smooth(method = glm) +
  facet_grid(rows = vars(Task)) +
  scale_y_continuous(breaks = seq (0, 10, 2),
                     limits = c(-1, 11)) +
  scale_x_continuous(breaks = seq (0, 18, 3),
                     limits = c(-1, 15)) +
  labs(x = "Number of anticipated responses on BESA", y = "DOM responses per participant", 
       title = "DOM by Proficiency and Task") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"))


## Subjunctive
Aggregate_Subj %>% 
  ggplot(aes(x = BESA_Other, y = Part_Avg)) + 
  geom_jitter(mapping = aes(color = Group)) +
  geom_smooth(method = glm) +
  facet_grid(rows = vars(Task)) +
  scale_y_continuous(breaks = seq (0, 8, 2),
                     limits = c(-1, 9)) +
  scale_x_continuous(breaks = seq (0, 14, 2),
                     limits = c(-1, 15)) +
  labs(x = "Number of anticipated responses on BESA", y = "Subjunctive responses per participant", 
       title = "Subjunctive by Proficiency and Task") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"))
