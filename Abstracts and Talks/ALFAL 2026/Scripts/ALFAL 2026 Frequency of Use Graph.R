library(tidyverse)
library(here)


# Load data
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
  mutate(Group = case_when(Group == "MLS-5" ~ "ML-5",
                           Group == "MLS-7/8" ~ "ML-7/8",
                           Group == "DLBE-5" ~ "BL-5",
                           Group == "DLBE-7/8" ~ "BL-7/8",
                           Group == "Adults" ~ "Adultos",
                           TRUE ~ Group)) %>% 
  mutate(Immersion = case_when(Group %in% c("BL-5", "BL-7/8") ~ "Inmersión doble",
                               Group %in% c("ML-5", "ML-7/8", "Adultos") ~ "Monolingüe en inglés",
                               TRUE ~ NA_character_)) %>% 
  pivot_longer(cols = c(Production, Selection),
               names_to = "Tarea",
               values_to = "Ratio") %>% 
  mutate(Task = case_when(Tarea == "Production" ~ "Producción",
                          Tarea == "Selection" ~ "Selección",
                          TRUE ~ Tarea),
         Group = (factor(Group, levels = c("ML-5", "BL-5", "ML-7/8", "BL-7/8", "Adultos"))))


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
  labs(x = "Autoevaluación del uso de español fuera de escuela",
       y = "Porcentaje del uso de MDO",
       title = "MDO por frecuencia de uso, tarea, y escuela",
       color = "Grupo") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"))


Use_Plot

ggsave(filename = here("Abstracts and Talks", "ALFAL 2026", "Graphs", "ALFAL 2026 Frequency of Use Plot.pdf"),
       plot = Use_Plot,
       device = "pdf",
       width = 6,
       height = 4,
       units = "in")
