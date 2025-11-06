library(tidyverse)
library(here)
library(patchwork)


# Load data
## Production
DLI78_EPT <- read_csv(here("./CSV Files/DLI-78/DLI-78 DOM EPT.csv")) %>% 
  mutate(Group = "DLI-7/8",
         Task = "Production",
         School = "Immersion",
         Age = "7th/8th")

MLS78_EPT <- read_csv(here("./CSV Files/MLS-78/MLS-78 DOM EPT.csv")) %>%
  filter(!School == "GBCS") %>% 
  mutate(Group = "MLS-7/8",
         Task = "Production",
         School = "Monolingual",
         Age = "7th/8th")

DLI5_EPT <- read_csv(here("./CSV Files/DLI-5/DLI-5 DOM EPT.csv")) %>% 
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>% 
  mutate(Group = "DLI-5",
         Task = "Production",
         School = "Immersion",
         Age = "5th Grade")

MLS5_EPT <- read_csv(here("./CSV Files/MLS-5/MLS-5 DOM EPT.csv")) %>%
  filter(!School == "GBCS") %>% 
  mutate(Group = "MLS-5",
         Task = "Production",
         School = "Monolingual",
         Age = "5th Grade")

HSA_EPT <- read_csv(here("./CSV Files/Adult HS/Adult HS DOM EPT.csv")) %>%
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>%
  mutate(Group = "Adults",
         Task = "Production",
         School = "Adults",
         Age = "Adults")


## Selection
DLI78_FCT <- read_csv(here("./CSV Files/DLI-78/DLI-78 DOM FCT.csv")) %>% 
  mutate(Group = "DLI-7/8",
         Task = "Selection",
         School = "Immersion",
         Age = "7th/8th")

MLS78_FCT <- read_csv(here("./CSV Files/MLS-78/MLS-78 DOM FCT.csv")) %>%
  filter(!School == "GBCS") %>% 
  mutate(Group = "MLS-7/8",
         Task = "Selection",
         School = "Monolingual",
         Age = "7th/8th")

DLI5_FCT <- read_csv(here("./CSV Files/DLI-5/DLI-5 DOM FCT.csv")) %>% 
  mutate(Group = "DLI-5",
         Task = "Selection",
         School = "Immersion",
         Age = "5th Grade")

MLS5_FCT <- read_csv(here("./CSV Files/MLS-5/MLS-5 DOM FCT.csv")) %>%
  filter(!School == "GBCS") %>% 
  mutate(Group = "MLS-5",
         Task = "Selection",
         School = "Monolingual",
         Age = "5th Grade")

HSA_FCT <- read_csv(here("./CSV Files/Adult HS/Adult HS DOM FCT.csv")) %>%
  mutate(Group = "Adults",
         Task = "Selection",
         School = "Adults",
         Age = "Adults")


# Join and tidy data
## Unify by task
Production <- rbind(DLI78_EPT, MLS78_EPT, DLI5_EPT, MLS5_EPT, HSA_EPT)
Selection <- rbind(DLI78_FCT, MLS78_FCT, DLI5_FCT, MLS5_FCT, HSA_FCT)


## Create unified dataframe
Master <- rbind(Production, Selection)
Master$Group <- factor(Master$Group, levels = c("MLS-5", "DLI-5", "MLS-7/8", "DLI-7/8", "Adults"))


## Prepare averages by structures
Master_Bar <- Master %>%
  filter(!is.na(DOM_Use)) %>%
  group_by(Group, Task) %>%
  summarize(Total_Accuracy  = sum(DOM_Use, na.rm = TRUE),
            Total_Responses = n(),
            Ratio = 100 * Total_Accuracy / Total_Responses,
            .groups = "drop")

Master_Box <- Master %>%
  filter(!is.na(DOM_Use)) %>%
  group_by(Part_ID, Group, Task) %>%
  summarize(Total_Accuracy  = sum(DOM_Use, na.rm = TRUE),
            Total_Responses = n(),
            Ratio = 100 * Total_Accuracy / Total_Responses,
            .groups = "drop")


## Summary of SDs for bar graph
Master_Bar_Summary <- Master_Box %>%
  group_by(Group, Task) %>%
  summarize(Average = mean(Ratio, na.rm = TRUE), 
            SD = sd(Ratio, na.rm = TRUE), 
            .groups = "drop") %>% 
  left_join(Master_Box, Master_Bar_Summary, by = c("Group", "Task"))



# Create plots
## Bar graph
Bar_Graph <- Master_Bar_Summary %>% 
  ggplot(aes(x = Group, y = Average, fill = Task)) +
  geom_bar(position = "dodge", color = "black", stat = "identity") +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 100)) +
    geom_text(aes(label = paste0(round(Average), "\n(", round(SD), ")")),
            position = position_dodge(width = .9),
            vjust = 0.5,
            size = 3.5,
            fontface = "bold") +
  scale_fill_manual(values = c("#BF5700", "#F4EFE0")) +
  labs(x = "Average (SD) by group and task", y = "Percentage of DOM responses", title = "") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none",
        strip.text = element_text(face = "bold"),
        strip.text.x = element_text(face = "bold"))

Bar_Graph

## Generate boxplot
Boxplot <- Master_Box %>% 
  ggplot(aes(x = Group, y = Ratio, fill = Task)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 100)) +
  scale_fill_manual(values = c("#BF5700", "#F4EFE0")) +
  labs(x = "Distribution by group and task", y = "Percentage of DOM responses", title = "") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        strip.text.x = element_text(face = "bold"),
        axis.title.y = element_blank())

Boxplot  


## Create joint plot
Group_Plot <- (Bar_Graph + Boxplot) + 
  plot_annotation(title = "Summary of Responses by Group and Task") & 
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

Group_Plot

ggsave(filename = here("Abstracts and Talks", "HLS 2025", "HLS 2025 Summary Graph.pdf"),
       plot = Group_Plot,
       device = "pdf",
       width = 10,
       height = 6,
       units = "in")
