library(tidyverse)


# Prepare data
## Load EPT
DLI78_EPT <- read_csv(here("./CSV Files/DLI-78/DLI-78 Subjunctive EPT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "DLI-7/8",
         Task = "Production")

MLS78_EPT <- read_csv(here("./CSV Files/MLS-78/MLS-78 Subjunctive EPT.csv")) %>%
  filter(!School == "GBCS") %>% 
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "MLE-7/8",
         Task = "Production")

DLI5_EPT <- read_csv(here("./CSV Files/DLI-5/DLI-5 Subjunctive EPT.csv"))  %>%
  filter(Property == "Intensional subjunctive")%>% 
  mutate(Group = "DLI-5",
         Task = "Production")

MLS5_EPT <- read_csv(here("./CSV Files/MLS-5/MLS-5 Subjunctive EPT.csv")) %>%
  filter(!School == "GBCS") %>% 
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "MLE-5",
         Task = "Production")


## Load FCT
DLI78_FCT <- read_csv(here("./CSV Files/DLI-78/DLI-78 Subjunctive FCT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "DLI-7/8",
         Task = "Selection")

MLS78_FCT <- read_csv(here("./CSV Files/MLS-78/MLS-78 Subjunctive FCT.csv")) %>%
  filter(!School == "GBCS") %>% 
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "MLE-7/8",
         Task = "Selection")

DLI5_FCT <- read_csv(here("./CSV Files/DLI-5/DLI-5 Subjunctive FCT.csv"))  %>%
  filter(Property == "Intensional subjunctive")%>% 
  mutate(Group = "DLI-5",
         Task = "Selection")

MLS5_FCT <- read_csv(here("./CSV Files/MLS-5/MLS-5 Subjunctive FCT.csv")) %>%
  filter(!School == "GBCS") %>% 
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "MLE-5",
         Task = "Selection")


## Merge tasks
EPT <- rbind(DLI78_EPT, MLS78_EPT, DLI5_EPT, MLS5_EPT)
FCT <- rbind(DLI78_FCT, MLS78_FCT, DLI5_FCT, MLS5_FCT)


# Generate averages and combined dataset
## 
EPT_Part_Avg <- aggregate(EPT$Mood_Use, list(EPT$Part_ID), FUN = sum, na.rm = TRUE)
EPT_Part_Avg <- EPT_Part_Avg %>% rename(Part_Avg = x)
EPT_Part_Avg <- left_join(EPT, EPT_Part_Avg, by = c("Part_ID" = "Group.1")) %>% 
  filter(Item == "EPT-02")

FCT_Part_Avg <- aggregate(FCT$Mood_Use, list(FCT$Part_ID), FUN = sum, na.rm = TRUE)
FCT_Part_Avg <- FCT_Part_Avg %>% rename(Part_Avg = x)
FCT_Part_Avg <- left_join(FCT, FCT_Part_Avg, by = c("Part_ID" = "Group.1")) %>% 
  filter(Item == "FCT-02")

Aggregate <- rbind(EPT_Part_Avg, FCT_Part_Avg)
Aggregate$Task <- factor(Aggregate$Task, levels = c("Production", "Selection"))
Aggregate$Group <- factor(Aggregate$Group, levels = c("DLI-7/8", "MLE-7/8", "DLI-5", "MLE-5"))


# Generate graphs
## BESA Proficiency
Aggregate %>% 
  ggplot(aes(x = BESA_Total, y = Part_Avg)) + 
  geom_jitter(mapping = aes(color = Group)) +
  geom_smooth(method = glm) +
  facet_grid(rows = vars(Task)) +
  scale_y_continuous(breaks = seq (0, 8, 2),
                     limits = c(-1, 9)) +
  scale_x_continuous(breaks = seq (0, 14, 3),
                     limits = c(-1, 15)) +
  labs(x = "BESA score", y = "Number of responses with subjunctive", 
       title = "Subjunctive by BESA Proficiency and Task") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"))


## Frequency of use
Aggregate %>% 
  ggplot(aes(x = Use_Joint, y = Part_Avg)) + 
  geom_jitter(mapping = aes(color = Group)) +
  geom_smooth(method = glm) +
  facet_grid(rows = vars(Task)) +
  scale_y_continuous(breaks = seq (0, 8, 2),
                     limits = c(-1, 9)) +
  scale_x_continuous(breaks = seq (5, 30, 5),
                     limits = c(5, 30)) +
  labs(x = "BESA score", y = "Number of responses with subjunctive", 
       title = "Subjunctive by Frequency of Use and Task") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"))

