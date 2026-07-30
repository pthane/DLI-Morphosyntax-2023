library(tidyverse)
library(here)
library(TOSTER)


# Load data
Master <- read_csv(here("Manuscripts", "SLR (Multiple Baselines)", "Data", "SLR Multiple Baselines DOM Selection.csv")) %>% 
  filter(Item == "FCT-03") %>% 
  mutate(BESA = BESA_Other + BESA_Subj)


# Format for Word
Manuscript_Summary <- function(data, value, var_name) {
  data %>% 
    group_by(Group) %>% 
    summarize(
      μ = mean({{ value }}, na.rm = TRUE),
      SD = sd({{ value }}, na.rm = TRUE),
      .groups = "drop") %>% 
    mutate(Variable = var_name)}

Summary_Table <- bind_rows(Manuscript_Summary(Master, BESA, "BESA"),
                           Manuscript_Summary(Master, Use, "Use"),
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


# Get group summaries directly
Group_Summary <- Children %>%
  group_by(School_Type) %>%
  summarize(
    m = mean(Use, na.rm = TRUE),
    sd = sd(Use, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# Inspect to make sure ordering is correct
Group_Summary


# TOST for frequency of use
## Parcel out child participants only; sort by immersion
TOST <- Master %>% 
  filter(!Group == "Adults") %>% 
  group_by(Part_ID, Group) %>%
  summarize(Use = mean(Use, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(School_Type = case_when(str_detect(Group, "DLBE") ~ "DLBE",
                                 str_detect(Group, "MLS") ~ "English-only",
                                 TRUE ~ NA_character_))


## Run TOST
tsum_TOST(m1 = Group_Summary$m[Group_Summary$School_Type == "DLBE"],
          sd1 = Group_Summary$sd[Group_Summary$School_Type == "DLBE"],
          n1 = Group_Summary$n[Group_Summary$School_Type == "DLBE"],
          m2 = Group_Summary$m[Group_Summary$School_Type == "English-only"],
          sd2 = Group_Summary$sd[Group_Summary$School_Type == "English-only"],
          n2 = Group_Summary$n[Group_Summary$School_Type == "English-only"],
          low_eqbound = -0.5,
          high_eqbound = 0.5,
          alpha = 0.05)

