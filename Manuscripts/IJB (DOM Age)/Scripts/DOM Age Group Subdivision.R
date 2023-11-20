library(tidyverse)
library(TOSTER)

options(scipen = 99)


# Load data
## EPT data
DLI5_EPT <- read_csv("./CSV Files/DLI-5/DLI-5 DOM EPT.csv") %>%
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42"))

MLS5_EPT <- read_csv("./CSV Files/MLS-5/MLS-5 DOM EPT.csv") %>%
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>%   mutate(Group = "HS5")

DLI78_EPT <- read_csv("./CSV Files/DLI-78/DLI-78 DOM EPT.csv") %>%
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>%   mutate(Group = "HS7/8")

MLS78_EPT <- read_csv("./CSV Files/MLS-78/MLS-78 DOM EPT.csv") %>%
  filter(Item %in% c("EPT-02", "EPT-08", "EPT-12", "EPT-14", "EPT-20", "EPT-24", "EPT-26", "EPT-32", "EPT-38", "EPT-42")) %>%   mutate(Group = "HS7/8")


## FCT data
DLI5_FCT <- read_csv("./CSV Files/DLI-5/DLI-5 DOM FCT.csv")
MLS5_FCT <- read_csv("./CSV Files/MLS-5/MLS-5 DOM FCT.csv")
DLI78_FCT <- read_csv("./CSV Files/DLI-78/DLI-78 DOM FCT.csv")
MLS78_FCT <- read_csv("./CSV Files/MLS-78/MLS-78 DOM FCT.csv")


## Create subsets by grade
HS5 <- rbind(DLI5_EPT, MLS5_EPT, DLI5_FCT, DLI78_FCT)
HS78 <- rbind(DLI78_EPT, MLS78_EPT, DLI78_FCT, MLS78_FCT)


## Resegment by school
HS5_GBCS <- HS5 %>% 
  filter(School == "GBCS")

HS5_RBMS <- HS5 %>% 
  filter(School == "RBMS")

HS78_GBCS <- HS78 %>% 
  filter(School == "GBCS")

HS78_RBMS <- HS78 %>% 
  filter(School == "RBMS")


## Join by school and grade
GBCS <- rbind(HS5_GBCS, HS78_GBCS)
RBMS <- rbind(HS5_RBMS, HS78_RBMS)
Aggregate <- rbind(GBCS, RBMS)


# Calculate participant averages
Average <- aggregate(Aggregate$DOM_Use, list(Aggregate$Part_ID), FUN = sum, na.rm = TRUE)
Average <- Average %>% rename(DOM_Sum = x)
Average <- left_join(Aggregate, Average, by = c("Part_ID" = "Group.1")) %>% 
  mutate(DOM_Sum_Std = (DOM_Sum - mean(DOM_Sum))/sd(DOM_Sum))


## Generate TOSTs
TOSTER::dataTOSTtwo(
  data = Average,
  deps = "DOM_Sum",
  group = "School",
  low_eqbound = -0.5,
  high_eqbound = 0.5,
  desc = TRUE,
  plots = TRUE)



# List children with bilingual education
unique(GBCS$Part_ID)
unique(RBMS$Part_ID)
