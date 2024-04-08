library(tidyverse)


# Load data
## EPT
DLI78_EPT <- read_csv("./CSV Files/DLI-78/DLI-78 Subjunctive EPT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "DLI-7/8",
         Task = "Production")

MLS78_EPT <- read_csv("./CSV Files/MLS-78/MLS-78 Subjunctive EPT.csv") %>%
  filter(!School == "GBCS") %>% 
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "MLE-7/8",
         Task = "Production")

DLI5_EPT <- read_csv("./CSV Files/DLI-5/DLI-5 Subjunctive EPT.csv")  %>%
  filter(Property == "Intensional subjunctive")%>% 
  mutate(Group = "DLI-5",
         Task = "Production")

MLS5_EPT <- read_csv("./CSV Files/MLS-5/MLS-5 Subjunctive EPT.csv") %>%
  filter(!School == "GBCS") %>% 
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "MLE-5",
         Task = "Production")


## Load FCT
DLI78_FCT <- read_csv("./CSV Files/DLI-78/DLI-78 Subjunctive FCT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "DLI-7/8",
         Task = "Selection")

MLS78_FCT <- read_csv("./CSV Files/MLS-78/MLS-78 Subjunctive FCT.csv") %>%
  filter(!School == "GBCS") %>% 
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "MLE-7/8",
         Task = "Selection")

DLI5_FCT <- read_csv("./CSV Files/DLI-5/DLI-5 Subjunctive FCT.csv")  %>%
  filter(Property == "Intensional subjunctive")%>% 
  mutate(Group = "DLI-5",
         Task = "Selection")

MLS5_FCT <- read_csv("./CSV Files/MLS-5/MLS-5 Subjunctive FCT.csv") %>%
  filter(!School == "GBCS") %>% 
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "MLE-5",
         Task = "Selection")


# Separate by verb
## EPT by verb
EPT <- rbind(DLI78_EPT, MLS78_EPT, DLI5_EPT, MLS5_EPT)
FCT <- rbind(DLI78_FCT, MLS78_FCT, DLI5_FCT, MLS5_FCT)


# Generate averages and combined dataset
## EPT
EPT_Verb_Avg <- aggregate(EPT$Mood_Use, list(EPT$Verb), FUN = mean, na.rm = TRUE)
EPT_Verb_Avg <- EPT_Verb_Avg %>% rename(Verb_Avg = x)
EPT_Verb_Avg <- left_join(EPT, EPT_Verb_Avg, by = c("Verb" = "Group.1")) %>% 
  mutate(Verb_Avg = (Verb_Avg * 100))


## FCT
FCT_Verb_Avg <- aggregate(FCT$Mood_Use, list(FCT$Verb), FUN = mean, na.rm = TRUE)
FCT_Verb_Avg <- FCT_Verb_Avg %>% rename(Verb_Avg = x)
FCT_Verb_Avg <- left_join(FCT, FCT_Verb_Avg, by = c("Verb" = "Group.1")) %>% 
  mutate(Verb_Avg = (Verb_Avg * 100))


## Join datasets
Aggregate <- rbind(EPT_Verb_Avg, FCT_Verb_Avg)
Aggregate$Group <- factor(Aggregate$Group, levels = c("DLI-/8", "MLS-7/8", "DLI-5", "MLS-5"))
Aggregate$Task <- factor(Aggregate$Task, levels = c("Production", "Selection"))
Aggregate$Verb <- factor(Aggregate$Verb, levels = c("llevar", "tratar", "llamar", "cuidar", "mirar", "amar", "pintar", "peinar"))


# Create graph
Aggregate %>% 
  ggplot(aes(x = Verb, y = Verb_Avg, fill = Task)) + 
  geom_bar(position = "dodge", color = "black", stat = "identity") +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 105)) +
  geom_text(aes(label = round(Verb_Avg)),
            position = position_dodge(width = .9),
            vjust = -0.5,
            size = 3) +
  scale_fill_manual(values = c("#56B4E9", "#F0E442")) +
  labs(x = "Verb (most to least frequent)", y = "Percentage of subjunctive responses", fill = "Task", title = "Subjunctive by Verb and Task") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"))
