library(tidyverse)
library(lme4)
library(lmerTest)

options(scipen = 99)


# Prepare data
## Load production data
SDB_EPT <- read_csv("./CSV Files/SDB/SDB Infinitive EPT.csv") %>% 
  mutate(Group = "SDB",
         DELE_Std = (DELE - mean(DELE))/sd(DELE))

HSA_EPT <- read_csv("./CSV Files/Adult HS/Adult HS Infinitive EPT.csv") %>% 
  mutate(Group = "HS")

L2L_EPT <- read_csv("./CSV Files/Adult L2L/Adult L2L Infinitive EPT.csv") %>% 
  mutate(Group = "L2L")


## Load FCT data
SDB_FCT <- read_csv("./CSV Files/SDB/SDB Infinitive FCT.csv") %>% 
  mutate(Group = "SDB",
         DELE_Std = (DELE - mean(DELE))/sd(DELE))

HSA_FCT <- read_csv("./CSV Files/Adult HS/Adult HS Infinitive FCT.csv") %>% 
  mutate(Group = "HS")

L2L_FCT <- read_csv("./CSV Files/Adult L2L/Adult L2L Infinitive FCT.csv") %>% 
  mutate(Group = "L2L")


## Join dataframes, all groups
EPT <- rbind(SDB_EPT, HSA_EPT, L2L_EPT)
EPT$Group <- factor(EPT$Group, levels = c("SDB", "HS", "L2L"))
EPT$Short_Prop <- factor(EPT$Short_Prop, levels = c("Subject", "Object", "Preposition"))

FCT <- rbind(SDB_FCT, HSA_FCT, L2L_FCT)
FCT$Group <- factor(FCT$Group, levels = c("SDB", "HS", "L2L"))
FCT$Short_Prop <- factor(FCT$Short_Prop, levels = c("Subject", "Object", "Preposition"))

Master <- rbind(EPT, FCT)
Master$Group <- factor(Master$Group, levels = c("SDB", "HS", "L2L"))
Master$Short_Prop <- factor(Master$Short_Prop, levels = c("Subject", "Object", "Preposition"))


## Join dataframes, experimental groups
EPT_Exp <- EPT %>% 
  filter(!Group == "SDB")

FCT_Exp <- FCT %>% 
  filter(!Group == "SDB")


# Run correlations
## Master correlation
Master_Correlation <- glmer(Comp_Use ~ Group + Short_Prop + Task + Group:Short_Prop + Group:Task +
                              (1 | Part_ID) + (1 | Item),
                            data = Master,
                            family = "binomial")

summary(Master_Correlation)


## Production
Production <- glmer(Comp_Use ~ Group + DELE_Std + Use_Joint_Std + Composite_Freq + Group:Use_Joint_Std + Group:Composite_Freq +
                      (1 | Part_ID) + (1 | Item),
                    data = EPT_Exp,
                    family = "binomial")

summary(Production)


# FCT
# FCT
FCT_Correlation <- glmer(Comp_Use ~ Group + DELE_Std + Use_Joint_Std + Composite_Freq + Group:Use_Joint_Std + Group:Composite_Freq +
                           (1 | Part_ID) + (1 | Item),
                         data = FCT_Exp,
                         family = "binomial")

summary(FCT_Correlation)
