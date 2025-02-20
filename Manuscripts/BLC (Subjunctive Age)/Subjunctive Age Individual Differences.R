library(tidyverse)
library(dplyr)
library(here)


# Load data
## EPT
HSA_EPT <- read_csv(here("./CSV Files/Adult HS/Adult HS Subjunctive EPT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HSA")

DLI78_EPT <- read_csv(here("./CSV Files/DLI-78/DLI-78 Subjunctive EPT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS7/8")

MLS78_EPT <- read_csv(here("./CSV Files/MLS-78/MLS-78 Subjunctive EPT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS7/8")

DLI5_EPT <- read_csv(here("./CSV Files/DLI-5/DLI-5 Subjunctive EPT.csv"))  %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS5")

MLS5_EPT <- read_csv(here("./CSV Files/MLS-5/MLS-5 Subjunctive EPT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS5") 


## FCT
HSA_FCT <- read_csv(here("./CSV Files/Adult HS/Adult HS Subjunctive FCT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HSA")

DLI78_FCT <- read_csv(here("./CSV Files/DLI-78/DLI-78 Subjunctive FCT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS7/8")

MLS78_FCT <- read_csv(here("./CSV Files/MLS-78/MLS-78 Subjunctive FCT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS7/8")

DLI5_FCT <- read_csv(here("./CSV Files/DLI-5/DLI-5 Subjunctive FCT.csv"))  %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS5")

MLS5_FCT <- read_csv(here("./CSV Files/MLS-5/MLS-5 Subjunctive FCT.csv")) %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS5") 


## Rejoin dataframes
EPT <- rbind(HSA_EPT, DLI78_EPT, MLS78_EPT, DLI5_EPT, MLS5_EPT)
FCT <- rbind(HSA_FCT, DLI78_FCT, MLS78_FCT, DLI5_FCT, MLS5_FCT)


# Plot individual differences
## Grouping
EPT_Group <- EPT %>%
  filter(!is.na(Mood_Use)) %>%
  group_by(Part_ID, Group) %>%
  summarize(Mood_Production = sum(Mood_Use))

FCT_Group <- FCT %>%
  filter(!is.na(Mood_Use)) %>%
  group_by(Part_ID, Group) %>%
  summarize(Mood_Selection = sum(Mood_Use))

Aggregate = left_join(EPT_Group, FCT_Group, by = "Part_ID", "Group") %>%
  rename(Group = Group.x) %>% 
  mutate(Total = Mood_Production + Mood_Selection)
Aggregate$Group <- factor(Aggregate$Group, levels = c("HSA", "HS7/8", "HS5"))


# Plot individual differences
## Whole group
Aggregate %>% 
  ggplot(aes(x = Mood_Production, y = Mood_Selection)) +
  geom_jitter(mapping = aes(color = Group)) +
  scale_x_continuous(breaks = seq (0, 10, 2),
                     limits = c(-1, 9)) +
  scale_y_continuous(breaks = seq (0, 8, 2),
                     limits = c(-1, 9)) +
  labs(x = "Sentences with subjunctive produced on EPT", y = "Sentences with subjunctive selected on FCT", title = "Individual Rates of Subjunctive Selection and Production", color = "Group") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold", size = 10))


# Descriptive statistics
Mood_Production <- Aggregate %>% 
  group_by(Group) %>% 
  summarize(Mean = mean(Mood_Production), SD = sd(Mood_Production))

Mood_Selection <- Aggregate %>% 
  group_by(Group) %>% 
  summarize(Mean = mean(Mood_Selection), SD = sd(Mood_Selection))
