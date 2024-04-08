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


## FCT
DLI78_FCT <- read_csv("./CSV Files/DLI-78/DLI-78 Subjunctive FCT.csv")  %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "DLI-7/8",
         Task = "Selection")

MLS78_FCT <- read_csv("./CSV Files/MLS-78/MLS-78 Subjunctive FCT.csv")  %>%
  filter(!School == "GBCS") %>% 
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "MLE-7/8",
         Task = "Selection")

DLI5_FCT <- read_csv("./CSV Files/DLI-5/DLI-5 Subjunctive FCT.csv")  %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "DLI-5",
         Task = "Selection")

MLS5_FCT <- read_csv("./CSV Files/MLS-5/MLS-5 Subjunctive FCT.csv") %>%
  filter(!School == "GBCS") %>% 
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "MLE-5",
         Task = "Selection")


# Join datasets
## Bind by task
EPT <- rbind(DLI78_EPT, MLS78_EPT, DLI5_EPT, MLS5_EPT)
FCT <- rbind(DLI78_FCT, MLS78_FCT, DLI5_FCT, MLS5_FCT)


## Create datasets for graphs
EPT_Group <- EPT %>%
  filter(!is.na(Mood_Use)) %>%
  group_by(Part_ID, Group) %>%
  summarize(Production = sum(Mood_Use))

FCT_Group <- FCT %>%
  filter(!is.na(Mood_Use)) %>%
  group_by(Part_ID, Group) %>%
  summarize(Production = sum(Mood_Use))

Aggregate = left_join(EPT_Group, FCT_Group, by = "Part_ID", "Group") %>%
  rename(Group = Group.x,
         Production = Production.x,
         Preference = Production.y) %>% 
  mutate(Total = Preference + Production)


# Plot individual differences
## Without labels
Aggregate %>% 
  ggplot(aes(x = Production, y = Preference, fill = Group)) +
  geom_jitter(mapping = aes(color = Group)) +
  scale_x_continuous(breaks = seq (0, 8, 2),
                     limits = c(-1, 9)) +
  scale_y_continuous(breaks = seq (0, 8, 2),
                     limits = c(-1, 9)) +
  labs(x = "Number of sentences with subjunctive produced", y = "Number of sentences with subjunctive selected", title = "Individual Rates of Subjunctive Production and Selection", color = "Group") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold", size = 10))


## With labels
Aggregate %>% 
  ggplot(aes(x = Production, y = Preference, fill = Group, label = Part_ID)) +
  geom_jitter(mapping = aes(color = Group)) +
  geom_text() +
  scale_x_continuous(breaks = seq (0, 8, 2),
                     limits = c(-1, 9)) +
  scale_y_continuous(breaks = seq (0, 8, 2),
                     limits = c(-1, 9)) +
  labs(x = "Number of sentences with subjunctive produced", y = "Number of sentences with subjunctive selected", title = "Individual Rates of Subjunctive Selection and Production", color = "Group") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold", size = 10))