library(tidyverse)


# Load and prepare data
## EPT
HS_EPT <- read_csv("./CSV Files/Adult HS/Adult HS Subjunctive EPT.csv") %>% 
  filter(Short_Prop == "Intensional") %>% 
  mutate(Group = "HS",
         Task = "SCT")

L2L_EPT <- read_csv("./CSV Files/Adult L2L/Adult L2L Subjunctive EPT.csv") %>% 
  filter(Short_Prop == "Intensional") %>% 
  mutate(Group = "L2L",
         Task = "SCT")


## FCT
HS_FCT <- read_csv("./CSV Files/Adult HS/Adult HS Subjunctive FCT.csv") %>% 
  filter(Short_Prop == "Intensional") %>% 
  mutate(Group = "HS",
         Task = "MRT")

L2L_FCT <- read_csv("./CSV Files/Adult L2L/Adult L2L Subjunctive FCT.csv") %>% 
  filter(Short_Prop == "Intensional") %>% 
  mutate(Group = "L2L",
         Task = "MRT")


## Join datasets
EPT <- rbind(HS_EPT, L2L_EPT)
FCT <- rbind(HS_FCT, L2L_FCT)


# Calculate sum of subjunctive responses by participant
EPT_Part_Avg <- aggregate(EPT$Mood_Use, list(EPT$Part_ID), FUN = sum, na.rm = TRUE)
EPT_Part_Avg <- EPT_Part_Avg %>% rename(Part_Avg = x)
EPT_Part_Avg <- left_join(EPT, EPT_Part_Avg, by = c("Part_ID" = "Group.1")) %>% 
  filter(Item == "EPT-02")

FCT_Part_Avg <- aggregate(FCT$Mood_Use, list(FCT$Part_ID), FUN = sum, na.rm = TRUE)
FCT_Part_Avg <- FCT_Part_Avg %>% rename(Part_Avg = x)
FCT_Part_Avg <- left_join(FCT, FCT_Part_Avg, by = c("Part_ID" = "Group.1")) %>% 
  filter(Item == "FCT-02")


# Join datsets
Aggregate <- rbind(EPT_Part_Avg, FCT_Part_Avg)
Aggregate$Group <- factor(Aggregate$Group, levels = c("HS", "L2L"))
Aggregate$Task <- factor(Aggregate$Task, levels = c("SCT", "MRT"))


# Generate graphs
## By task
Aggregate %>% 
  ggplot(aes(x = DELE, y = Part_Avg)) + 
  geom_jitter(mapping = aes(color = Group)) +
  geom_smooth(method = glm) +
  facet_grid(rows = vars(Task)) +
  scale_y_continuous(breaks = seq (0, 8, 2),
                     limits = c(-1, 9)) +
  scale_x_continuous(breaks = seq (0, 50, 5),
                     limits = c(15, 50)) +
  labs(x = "BESA score", y = "Number of expected responses with subjunctive", 
       title = "Subjunctive by Proficiency and Task") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"))


## Across tasks
Aggregate %>% 
  ggplot(aes(x = DELE, y = Part_Avg)) + 
  geom_jitter(mapping = aes(color = Group)) +
  geom_smooth(method = glm) +
  scale_y_continuous(breaks = seq (0, 8, 2),
                     limits = c(-1, 9)) +
  scale_x_continuous(breaks = seq (0, 50, 5),
                     limits = c(15, 50)) +
  labs(x = "BESA score", y = "Number of expected responses with subjunctive", 
       title = "Subjunctive by Proficiency Across Tasks") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"))
