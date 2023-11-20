library(tidyverse)


# Load data
DLI78 <- read_csv("./CSV Files/DLI-78/DLI-78 Subjunctive EPT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Group = "DLI-7/8",
         Grade = "Older",
         School = "DLE",
         Task = "Production")

MLS78 <- read_csv("./CSV Files/MLS-78/MLS-78 Subjunctive EPT.csv") %>%
  filter(Property == "Intensional subjunctive") %>% 
  filter(!School == "GBCS") %>% 
  mutate(Group = "MLE-7/8",
         Grade = "Older",
         School = "MLS",
         Task = "Production")

DLI5 <- read_csv("./CSV Files/DLI-5/DLI-5 Subjunctive EPT.csv")  %>%
  filter(Property == "Intensional subjunctive")%>% 
  mutate(Group = "DLI-5",
         Grade = "Fifth",
         School = "DLE",
         Task = "Production")

MLS5 <- read_csv("./CSV Files/MLS-5/MLS-5 Subjunctive EPT.csv") %>%
  filter(!School == "GBCS") %>% 
  mutate(Group = "MLE-5",
         Grade = "Fifth",
         School = "MLS",
         Task = "Production")

SDB <- read_csv("./CSV Files/SDB/SDB Subjunctive EPT.csv") %>% 
  mutate(Group = "SDB",
         Task = "Production")


# Merge data
Combined <- rbind(DLI78, MLS78, DLI5, MLS5, SDB)


# View unique
unique(Combined$Part_ID)
