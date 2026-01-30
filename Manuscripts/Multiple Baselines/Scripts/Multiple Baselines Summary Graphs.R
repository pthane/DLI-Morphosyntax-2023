library(tidyverse)
library(here)
library(patchwork)


# Create dataframes
## Load CSVs
DOM_EPT <- read_csv(here("Manuscripts", "Multiple Baselines", "Data", "Multiple Baselines DOM Production.csv")) %>% 
  mutate(Structure = "DOM")%>% 
  select(!Mood)

DOM_FCT <- read_csv(here("Manuscripts", "Multiple Baselines", "Data", "Multiple Baselines DOM Selection.csv")) %>% 
  mutate(Structure = "DOM")%>% 
  select(!Mood)


## Join dataframes
Master <- rbind(DOM_EPT, DOM_FCT)
Master$Group <- factor(Master$Group, levels = c("MLS-5", "DLBE-5", "MLS-7/8", "DLBE-7/8", "Adults"))


# Compute graphs
## Create axis modification
Abbreviations <- c("MLS5", "DLBE5", "MLS78", "DLBE78", "Adults")


## Calculate graphs
Master_Bar <- Master %>%
  filter(!is.na(Response)) %>%
  group_by(Structure, Group, Task) %>%
  summarize(Total_Accuracy  = sum(Response, na.rm = TRUE),
            Total_Responses = n(),
            Ratio = 100 * Total_Accuracy / Total_Responses,
            .groups = "drop")

Master_Box <- Master %>%
  filter(!is.na(Response)) %>%
  group_by(Part_ID, Structure, Group, Task) %>%
  summarize(Total_Accuracy  = sum(Response, na.rm = TRUE),
            Total_Responses = n(),
            Ratio = 100 * Total_Accuracy / Total_Responses,
            .groups = "drop")


## Summary of SDs for bar graph
Master_Bar_Summary <- Master_Box %>%
  group_by(Structure, Group, Task) %>%
  summarize(Average = mean(Ratio, na.rm = TRUE), 
            SD = sd(Ratio, na.rm = TRUE), 
            .groups = "drop") %>% 
  left_join(Master_Box, Master_Bar_Summary, by = c("Structure", "Group", "Task"))


# Create plots
## Bar graph
Bar_Graph <- Master_Bar_Summary %>% 
  ggplot(aes(x = Group, y = Average, fill = Task)) +
  geom_bar(position = "dodge", color = "black", stat = "identity") +
  scale_x_discrete(labels= Abbreviations) +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 100)) +
  geom_text(aes(label = paste0(round(Average), "\n(", round(SD), ")")),
            position = position_dodge(width = .9),
            vjust = 0.5,
            size = 2.75,
            fontface = "bold") +
  scale_fill_manual(values = c("#BF5700", "#F4EFE0")) +
  labs(x = "Average (SD)", y = "Percentage of target responses") +
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
  scale_x_discrete(labels= Abbreviations) +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 100)) +
  scale_fill_manual(values = c("#BF5700", "#F4EFE0"), labels = c("PROD", "SEL")) +
  labs(x = "Distribution", y = "Percentage of responses") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        strip.text.x = element_text(face = "bold"),
        axis.title.y = element_blank())

Boxplot  


## Create joint plot
Group_Plot <- (Bar_Graph + Boxplot) + 
  plot_annotation(title = "Statistical Summary of Responses by Group and Task") & 
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

Group_Plot

ggsave(filename = here("Manuscripts", "Multiple Baselines", "Graphs", "Multiple Baselines Figure 4.pdf"),
       plot = Group_Plot,
       device = "pdf",
       width = 7,
       height = 3.5,
       units = "in")
 