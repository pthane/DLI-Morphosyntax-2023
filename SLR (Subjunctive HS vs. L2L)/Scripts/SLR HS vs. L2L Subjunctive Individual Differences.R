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


# Create dataframe for graph
EPT_Pivoted <- EPT %>%
  filter(!is.na(Mood_Use)) %>%
  group_by(Part_ID, Group) %>%
  summarize(Subj_Production = sum(Mood_Use))

FCT_Pivoted <- FCT %>%
  filter(!is.na(Mood_Use)) %>%
  group_by(Part_ID, Group) %>%
  summarize(Subj_Selection = sum(Mood_Use))

Aggregate = left_join(EPT_Pivoted, FCT_Pivoted, by = "Part_ID", "Group") %>% 
  mutate(Sum = (Subj_Production + Subj_Selection))
Aggregate$Group.x <- factor(Aggregate$Group.x, levels = c("HS", "L2L"))


# Generate graph
Aggregate %>% 
  ggplot(aes(x = Subj_Production, y = Subj_Selection, color = Group.x)) +
  geom_jitter() +
  scale_x_continuous(breaks = seq (0, 8, 2),
                     limits = c(-0.5, 8.5)) +
  scale_y_continuous(breaks = seq (0, 8, 2),
                     limits = c(-0.5, 8.5)) +
  labs(x = "Sentences with DOM produced in EPT", y = "Sentences with DOM selected in FCT", title = "Individual Rates of Subjunctive Use", color = "Group") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold", size = 10))
