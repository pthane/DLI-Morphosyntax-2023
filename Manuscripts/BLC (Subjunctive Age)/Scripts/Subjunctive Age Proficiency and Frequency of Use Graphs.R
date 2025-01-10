library(tidyverse)
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

# Generate aggregate dataset
## Participant averages
EPT_Part_Avg <- aggregate(EPT$Mood_Use, list(EPT$Part_ID), FUN = sum, na.rm = TRUE)
EPT_Part_Avg <- EPT_Part_Avg %>% rename(Part_Avg = x)
EPT_Part_Avg <- left_join(EPT, EPT_Part_Avg, by = c("Part_ID" = "Group.1"))

FCT_Part_Avg <- aggregate(FCT$Mood_Use, list(FCT$Part_ID), FUN = sum, na.rm = TRUE)
FCT_Part_Avg <- FCT_Part_Avg %>% rename(Part_Avg = x)
FCT_Part_Avg <- left_join(FCT, FCT_Part_Avg, by = c("Part_ID" = "Group.1"))


## Select individual item for ease of viewing
EPT_Modified <- EPT_Part_Avg %>% 
  filter(Item == "EPT-02")

FCT_Modified <- FCT_Part_Avg %>% 
  filter(Item == "FCT-02")


## Rejoin dataframes
Aggregate <- rbind(EPT_Modified, FCT_Modified)
Aggregate$Group <- factor(Aggregate$Group, levels = c("HSA", "HS7/8", "HS5"))


# Generate graphs
## Proficiency
Aggregate %>% 
  ggplot(aes(x = BESA_Other, y = Part_Avg)) + 
  geom_jitter(mapping = aes(color = Group)) +
  geom_smooth(method = glm) +
  facet_grid(rows = vars(Task)) +
  scale_y_continuous(breaks = seq (0, 8, 2),
                     limits = c(-1, 9)) +
  scale_x_continuous(breaks = seq (0, 14, 2),
                     limits = c(-1, 15)) +
  labs(x = "Number of anticipated responses on BESA", y = "Number of subjunctive responses per participant", 
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
  scale_x_continuous(breaks = seq (0, 30, 5),
                     limits = c(-1, 31)) +
  labs(x = "Self-reported frequency of use of Spanish", y = "Number of subjunctive responses per participant", 
       title = "Subjunctive by Frequency of Use and Task") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"))
