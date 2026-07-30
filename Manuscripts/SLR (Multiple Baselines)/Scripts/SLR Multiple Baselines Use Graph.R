library(tidyverse)
library(here)
library(patchwork)


# Create dataframes
## Load CSVs
DOM_EPT <- read_csv(here("Manuscripts", "SLR (Multiple Baselines)", "Data", "SLR Multiple Baselines DOM Production.csv")) %>% 
  mutate(Structure = "DOM") %>%
  group_by(Part_ID, Group, Task, Use) %>%
  summarize(Total_Structure = sum(Response, na.rm = TRUE),
            Total_Responses = sum(!is.na(Response)),
            Prod_Ratio = Total_Structure / Total_Responses,
            .groups = "drop") %>% 
  mutate(Production = Prod_Ratio * 100)

DOM_FCT <- read_csv(here("Manuscripts", "SLR (Multiple Baselines)", "Data", "SLR Multiple Baselines DOM Selection.csv")) %>% 
  mutate(Structure = "DOM") %>%
  group_by(Part_ID, Group, Task, Use) %>%
  summarize(Total_Structure = sum(Response, na.rm = TRUE),
            Total_Responses = sum(!is.na(Response)),
            Sel_Ratio = Total_Structure / Total_Responses,
            .groups = "drop") %>% 
  mutate(Selection = Sel_Ratio * 100)


## Join dataframes
Aggregate <- left_join(DOM_EPT, DOM_FCT, by = c("Part_ID", "Group", "Use")) %>% 
  mutate(Sum = Production + Selection) %>% 
  rename(Number_Produced = Total_Structure.x,
         Number_Selected = Total_Structure.y,
         EPT_Total = Total_Responses.x,
         FCT_Total = Total_Responses.y) %>% 
  mutate(Immersion = case_when(Group %in% c("DLBE-5", "DLBE-7/8") ~ "DLBE",
                               Group %in% c("MLS-5", "MLS-7/8", "Adults") ~ "English-only",
                               TRUE ~ NA_character_)) %>% 
  pivot_longer(cols = c(Production, Selection),
               names_to = "Task",
               values_to = "Ratio") %>% 
  mutate(Group = (factor(Group, levels = c("MLS-5", "DLBE-5", "MLS-7/8", "DLBE-7/8", "Adults"))))


## Calculate graphs
Use_Plot <- Aggregate %>% 
  ggplot(aes(x = Use, y = Ratio)) + 
  geom_jitter(aes(color = Group), width = 0.3, height = 0) +
  geom_smooth(method = "glm", formula = y ~ x, se = TRUE) +
  facet_grid(rows = vars(Immersion),
             cols = vars(Task)) +
  scale_y_continuous(breaks = seq(0, 100, 20),
                     limits = c(-2, 102)) +
  scale_x_continuous(breaks = seq(0, 25, 5),
                     limits = c(-2, 27)) +
  labs(x = "Self-reported frequency of use outside of school contexts",
       y = "Percentage of use of DOM",
       title = "DOM by Frequency of Use, Schooling, and Task") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"))


Use_Plot


## Save plot
ggsave(filename = here("Manuscripts", "SLR (Multiple Baselines)", "Graphs", "SLR Multiple Baselines Figure 5.pdf"),
       plot = Use_Plot,
       device = "pdf",
       width = 6.5,
       height = 4,
       units = "in")

