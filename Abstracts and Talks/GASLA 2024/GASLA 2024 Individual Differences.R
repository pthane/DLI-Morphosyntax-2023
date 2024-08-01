library(tidyverse)

# Load and prepare data
## EPT
HSA_EPT <- read.csv("./CSV Files/Adult HS/Adult HS DOM EPT.csv") %>% 
  mutate(Group = "HSA",
         Task = "Production",
         School = "English")

DLI78_EPT <- read.csv("./CSV Files/DLI-78/DLI-78 DOM EPT.csv") %>% 
  mutate(Group = "HS7/8",
         Task = "Production",
         School = "Bilingual")

MLS78_EPT <- read.csv("./CSV Files/MLS-78/MLS-78 DOM EPT.csv") %>% 
  filter(!School == "GBCS") %>%
  mutate(Group = "HS7/8",
         Task = "Production",
         School = "English")

DLI5_EPT <- read.csv("./CSV Files/MLS-5/MLS-5 DOM EPT.csv") %>% 
  mutate(Group = "HS5",
         Task = "Production",
         School = "Bilingual")

MLS5_EPT <- read.csv("./CSV Files/MLS-5/MLS-5 DOM EPT.csv") %>% 
  filter(!School == "GBCS") %>% 
  mutate(Group = "HS5",
         Task = "Production",
         School = "English")


## FCT
HSA_FCT <- read.csv("./CSV Files/Adult HS/Adult HS DOM FCT.csv") %>% 
  mutate(Group = "HSA",
         Task = "Forced Choice",
         School = "English")

DLI78_FCT <- read.csv("./CSV Files/DLI-78/DLI-78 DOM FCT.csv") %>% 
  mutate(Group = "HS7/8",
         Task = "Forced Choice",
         School = "Bilingual")

MLS78_FCT <- read.csv("./CSV Files/MLS-78/MLS-78 DOM FCT.csv") %>% 
  filter(!School == "GBCS") %>%
  mutate(Group = "HS7/8",
         Task = "Forced Choice",
         School = "English")

DLI5_FCT <- read.csv("./CSV Files/MLS-5/MLS-5 DOM FCT.csv") %>% 
  mutate(Group = "HS5",
         Task = "Forced Choice",
         School = "Bilingual")

MLS5_FCT <- read.csv("./CSV Files/MLS-5/MLS-5 DOM FCT.csv") %>% 
  filter(!School == "GBCS") %>% 
  mutate(Group = "HS5",
         Task = "Forced Choice",
         School = "English")


## Join datasets
EPT <- rbind(HSA_EPT, DLI78_EPT, MLS78_EPT, DLI5_EPT, MLS78_EPT)
FCT <- rbind(HSA_FCT, DLI78_FCT, MLS78_FCT, DLI5_FCT, MLS78_FCT)


# Generate individual differences statistics
EPT_Diffs <- EPT %>%
  filter(!is.na(DOM_Use)) %>%
  group_by(Part_ID, Group) %>%
  summarize(Production = sum(DOM_Use))

FCT_Diffs <- FCT %>%
  filter(!is.na(DOM_Use)) %>%
  group_by(Part_ID, Group) %>%
  summarize(Production = sum(DOM_Use))

Diffs = left_join(EPT_Diffs, FCT_Diffs, by = "Part_ID", "Group") %>%
  rename(Group = Group.x,
         Production = Production.x,
         Preference = Production.y) %>% 
  mutate(Total = Preference + Production) %>% 
  filter(!Group == "SDBA")

Diffs$Group <- factor(Diffs$Group, levels = c("HSA", "HS7/8", "HS5"))


# Generate scatter plot
Diffs %>% 
  ggplot(aes(x = Production, y = Preference)) +
  geom_jitter(mapping = aes(color = Group)) +
  scale_x_continuous(breaks = seq (0, 10, 2),
                     limits = c(-1, 11)) +
  scale_y_continuous(breaks = seq (0, 8, 2),
                     limits = c(-1, 9)) +
  labs(x = "Number of sentences with DOM produced", y = "Number of sentences with DOM selected", title = "Individual Rates of DOM Selection and Production", color = "Group") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold", size = 10))
