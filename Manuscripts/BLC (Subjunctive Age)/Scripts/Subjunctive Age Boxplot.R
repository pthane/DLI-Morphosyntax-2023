library(tidyverse)


# Prepare and load data
## EPT
SDB_EPT <- read_csv("./CSV Files/SDB/SDB Subjunctive EPT.csv") %>%
  filter(Property == "Intensional subjunctive") %>%
  mutate(Group = "SDBA")

HSA_EPT <- read_csv("./CSV Files/Adult HS/Adult HS Subjunctive EPT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HSA")

DLI78_EPT <- read_csv("./CSV Files/DLI-78/DLI-78 Subjunctive EPT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS7/8")

MLS78_EPT <- read_csv("./CSV Files/MLS-78/MLS-78 Subjunctive EPT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS7/8")

DLI5_EPT <- read_csv("./CSV Files/DLI-5/DLI-5 Subjunctive EPT.csv")  %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS5")

MLS5_EPT <- read_csv("./CSV Files/MLS-5/MLS-5 Subjunctive EPT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS5") 


## FCT
SDB_FCT <- read_csv("./CSV Files/SDB/SDB Subjunctive FCT.csv") %>%
  filter(Property == "Intensional subjunctive") %>%
  mutate(Group = "SDBA")

HSA_FCT <- read_csv("./CSV Files/Adult HS/Adult HS Subjunctive FCT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HSA")

DLI78_FCT <- read_csv("./CSV Files/DLI-78/DLI-78 Subjunctive FCT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS7/8")

MLS78_FCT <- read_csv("./CSV Files/MLS-78/MLS-78 Subjunctive FCT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS7/8")

DLI5_FCT <- read_csv("./CSV Files/DLI-5/DLI-5 Subjunctive FCT.csv")  %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS5")

MLS5_FCT <- read_csv("./CSV Files/MLS-5/MLS-5 Subjunctive FCT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "HS5")


## Join data
EPT <- rbind(SDB_EPT, HSA_EPT, DLI78_EPT, MLS78_EPT, DLI5_EPT, MLS5_EPT)
FCT <- rbind(SDB_FCT, HSA_FCT, DLI78_FCT, MLS78_FCT, DLI5_FCT, MLS5_FCT)


# Calculate averages
## Generate averages
EPT_Average <- aggregate(EPT$Mood_Use, list(EPT$Part_ID), FUN = sum, na.rm = TRUE)
EPT_Average <- EPT_Average %>% rename(Subj_Sum = x)
EPT_Average <- left_join(EPT, EPT_Average, by = c("Part_ID" = "Group.1")) %>% 
  mutate(Subj_Percentage = (Subj_Sum/8)*100)

FCT_Average <- aggregate(FCT$Mood_Use, list(FCT$Part_ID), FUN = sum, na.rm = TRUE)
FCT_Average <- FCT_Average %>% rename(Subj_Sum = x)
FCT_Average <- left_join(FCT, FCT_Average, by = c("Part_ID" = "Group.1")) %>% 
  mutate(Subj_Percentage = (Subj_Sum/8)*100)


## Join datasets
Aggregate <- rbind(EPT_Average, FCT_Average)
Aggregate$Group <- factor(Aggregate$Group, levels = c("SDBA", "HSA", "HS7/8", "HS5"))
Aggregate$Task <- factor(Aggregate$Task, levels = c("EPT", "FCT"))


# Create graph
Aggregate %>% 
  ggplot(aes(x = Group, y = Subj_Sum, fill = Task)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq (0, 8, 2),
                     limits = c(0, 8)) +
  labs(x = "Group", y = "Number of subjunctive responses", title = "Subjunctive by Group and Task") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"))
