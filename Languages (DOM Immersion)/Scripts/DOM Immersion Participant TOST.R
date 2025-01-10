library(tidyverse)
library(TOSTER)
library(here)

options(scipen = 99)


# Load and prepare data
DLI78 <- read_csv(here("./CSV Files/DLI-78/DLI-78 DOM FCT.csv")) %>% 
  mutate(Group = "DLE-7/8",
         School = "DLE") %>% 
  filter(Item == "FCT-03")

MLS78 <- read_csv(here("./CSV Files/MLS-78/MLS-78 DOM FCT.csv")) %>% 
  mutate(School = "MLS") %>% 
  filter(Item == "FCT-03")

DLI5 <- read_csv(here("./CSV Files/DLI-5/DLI-5 DOM FCT.csv")) %>% 
  mutate(Group = "DLE-5",
         School = "DLE") %>% 
  filter(Item == "FCT-03")

MLS5 <- read_csv(here("./CSV Files/MLS-5/MLS-5 DOM FCT.csv")) %>% 
  mutate(School = "MLS") %>% 
  filter(Item == "FCT-03")

Full <- rbind(DLI78, MLS78, DLI5, MLS5)


# Run equivalence tests
## Proficiency
TOSTER::dataTOSTtwo(
  data = Full,
  deps = "BESA_Other_Std",
  group = "School",
  low_eqbound = -0.5,
  high_eqbound = 0.5,
  desc = TRUE,
  plots = TRUE)


## Frequency of use
TOSTER::dataTOSTtwo(
  data = Full,
  deps = "Use_Joint_Std",
  group = "School",
  low_eqbound = -0.5,
  high_eqbound = 0.5,
  desc = TRUE,
  plots = TRUE)


## Exposure
TOSTER::dataTOSTtwo(
  data = Full,
  deps = "Exposure_Score_Std",
  group = "School",
  low_eqbound = -0.5,
  high_eqbound = 0.5,
  desc = TRUE,
  plots = TRUE)

