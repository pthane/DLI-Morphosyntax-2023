library(tidyverse)
library(here)


# Load data
Master <- read_csv(here("Manuscripts", "Multiple Baselines", "Data", "Multiple Baselines DOM Selection.csv")) %>% 
  filter(Item == "FCT-03") %>% 
  mutate(BESA = BESA_Other + BESA_Subj)


# Format for Word
Manuscript_Summary <- function(data, value, var_name) {
  data %>% 
    group_by(Group) %>% 
    summarize(
      μ = mean({{ value }}, na.rm = TRUE),
      SD = sd({{ value }}, na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    mutate(Variable = var_name)
  }

Summary_Table <- bind_rows(Manuscript_Summary(Master, BESA, "BESA"),
                           Manuscript_Summary(Master, Use_Joint, "Use"),
                           Manuscript_Summary(Master, Exposure_Score, "Parents")) %>% 
  pivot_longer(cols = c(μ, SD),
               names_to = "Stat",
               values_to = "Value") %>% 
  unite(Group_Stat, Group, Stat) %>% 
  pivot_wider(names_from = Group_Stat,
              values_from = Value) %>% 
  select(Variable,
    `MLS-5_μ`, `MLS-5_SD`,
    `DLBE-5_μ`, `DLBE-5_SD`,
    `MLS-7/8_μ`, `MLS-7/8_SD`,
    `DLBE-7/8_μ`, `DLBE-7/8_SD`,
    Adults_μ, Adults_SD)
