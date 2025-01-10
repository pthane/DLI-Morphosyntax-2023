library(tidyverse)
library(here)

options(scipen = 99)


## Load data
###### Note: FCT-02 was selected because it was the first "obligatory" question that all participants completed in each group.
SDB <- read_csv(here("./CSV Files/SDB/SDB Subjunctive FCT.csv")) %>%
  filter(Item == "FCT-02")

DLE78 <- read_csv(here("./CSV Files/DLI-78/DLI-78 Subjunctive FCT.csv")) %>%
  filter(Item == "FCT-02")

MLS78 <- read_csv(here("./CSV Files/MLS-78/MLS-78 Subjunctive FCT.csv")) %>%
  filter(Item == "FCT-02")

DLE5 <- read_csv(here("./CSV Files/DLI-5/DLI-5 Subjunctive FCT.csv")) %>%
  filter(Item == "FCT-02")

MLS5 <- read_csv(here("./CSV Files/MLS-5/MLS-5 Subjunctive FCT.csv")) %>%
  filter(Item == "FCT-02")

Combined <- rbind(SDB, DLE78, MLS78, DLE5, MLS5)


# Generate statistical averages
Use_Stats <- Combined %>%  
  group_by(Group) %>% 
  summarize(Average = mean(Use, na.rm = T), SD = sd(Use, na.rm = T), Max = max(Use, na.rm = T), Min = min(Use, na.rm = T))

Proficiency_Stats <- Combined %>%  
  group_by(Group) %>% 
  summarize(Average = mean(BESA_Total, na.rm = T), SD = sd(BESA_Total, na.rm = T), Max = max(BESA_Total, na.rm = T), Min = min(BESA_Other, na.rm = T))

Parental_Stats <- Combined %>%  
  group_by(Group) %>% 
  summarize(Average = mean(Exposure_Score, na.rm = T), SD = sd(Exposure_Score, na.rm = T), Max = max(Exposure_Score, na.rm = T), Min = min(Exposure_Score, na.rm = T))
            
            
