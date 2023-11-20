library(tidyverse)


## Production
DLI78_EPT <- read_csv("./CSV Files/DLI-78/DLI-78 DOM EPT.csv") %>% 
  mutate(Group = "BES-7/8",
         Task = "SCT")

MLS78_EPT <- read_csv("./CSV Files/MLS-78/MLS-78 DOM EPT.csv") %>%
  filter(!School == "GBCS") %>% 
  mutate(Group = "ME-7/8",
         Task = "SCT")

DLI5_EPT <- read_csv("./CSV Files/DLI-5/DLI-5 DOM EPT.csv") %>% 
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>% 
  mutate(Group = "BES-5",
         Task = "SCT")

MLS5_EPT <- read_csv("./CSV Files/MLS-5/MLS-5 DOM EPT.csv") %>%
  filter(!School == "GBCS") %>% 
  mutate(Group = "ME-5",
         Task = "SCT")


# Selection
DLI78_FCT <- read_csv("./CSV Files/DLI-78/DLI-78 DOM FCT.csv") %>% 
  mutate(Group = "BES-7/8",
         Task = "MST")

MLS78_FCT <- read_csv("./CSV Files/MLS-78/MLS-78 DOM FCT.csv") %>%
  filter(!School == "GBCS") %>% 
  mutate(Group = "ME-7/8",
         Task = "MST")

DLI5_FCT <- read_csv("./CSV Files/DLI-5/DLI-5 DOM FCT.csv") %>% 
  mutate(Group = "BES-5",
         Task = "MST")

MLS5_FCT <- read_csv("./CSV Files/MLS-5/MLS-5 DOM FCT.csv") %>%
  filter(!School == "GBCS") %>% 
  mutate(Group = "ME-5",
         Task = "MST")


# Rejoin data
EPT <- rbind(DLI78_EPT, MLS78_EPT, DLI5_EPT, MLS5_EPT)
FCT <- rbind(DLI78_FCT, MLS78_FCT, DLI5_FCT, MLS5_FCT)


# Generate aggregate dataset
## Participant averages
EPT_Part_Avg <- aggregate(EPT$DOM_Use, list(EPT$Part_ID), FUN = sum, na.rm = TRUE)
EPT_Part_Avg <- EPT_Part_Avg %>% rename(Part_Avg = x)
EPT_Part_Avg <- left_join(EPT, EPT_Part_Avg, by = c("Part_ID" = "Group.1"))

FCT_Part_Avg <- aggregate(FCT$DOM_Use, list(FCT$Part_ID), FUN = sum, na.rm = TRUE)
FCT_Part_Avg <- FCT_Part_Avg %>% rename(Part_Avg = x)
FCT_Part_Avg <- left_join(FCT, FCT_Part_Avg, by = c("Part_ID" = "Group.1"))


## Select individual item for ease of viewing
EPT_Modified <- EPT_Part_Avg %>% 
  filter(Item == "EPT-02") %>% 
  mutate(DOM_Percentage = (Part_Avg/10)*100)

FCT_Modified <- FCT_Part_Avg %>% 
  filter(Item == "FCT-03") %>% 
  mutate(DOM_Percentage = (Part_Avg/8)*100)


## Rejoin dataframes
Aggregate <- rbind(EPT_Modified, FCT_Modified)
Aggregate$Group <- factor(Aggregate$Group, levels = c("BES-7/8", "ME-7/8", "BES-5", "ME-5"))
Aggregate$Task <- factor(Aggregate$Task, levels = c("SCT", "MST"))


# Generate graphs
## Frequency of use
Aggregate %>% 
  ggplot(aes(x = Use_Joint, y = DOM_Percentage)) + 
  geom_jitter(mapping = aes(color = Group)) +
  geom_smooth(method = glm) +
  facet_grid(rows = vars(Task)) +
  scale_x_continuous(breaks = seq (0, 30, 5),
                     limits = c(4, 31)) +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(-5, 105)) +
  labs(x = "Frequency of use of Spanish", y = "Percentage of DOM responses per participant", 
       title = "DOM by Frequency of Use and Task") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"))



## Proficiency
Aggregate %>% 
  ggplot(aes(x = BESA_Total, y = DOM_Percentage)) + 
  geom_jitter(mapping = aes(color = Group)) +
  geom_smooth(method = glm) +
  facet_grid(rows = vars(Task)) +
  scale_x_continuous(breaks = seq (0, 18, 3),
                     limits = c(-1, 19)) +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(-5, 105)) +
  labs(x = "BESA morphosyntactic proficiency score", y = "Percentage of DOM responses per participant", 
       title = "DOM by Morphosyntactic Proficiency and Task") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"))
