library(tidyverse)
library(here)


# Load data
EPT_Master <- read_csv(here("./CSV Files/EPT Master.csv")) %>% 
  mutate(Davies_Std = (Davies - mean(Davies))/sd(Davies),
         CORPES_Std = (CORPES - mean(CORPES))/sd(CORPES),
         LOR = (Age - Age_Arrival),
         Composite_Freq = (Davies_Std + CORPES_Std)/2)
  
FCT_Master <- read_csv(here("./CSV Files/FCT Master.csv")) %>% 
  mutate(Davies_Std = (Davies - mean(Davies))/sd(Davies),
         CORPES_Std = (CORPES - mean(CORPES))/sd(CORPES),
         LOR = (Age - Age_Arrival),
         Composite_Freq = (Davies_Std + CORPES_Std)/2)


# 5th grade bilingual HS students at GBCS
DLI5_EPT <- EPT_Master %>% 
  filter(Group == "Bilingual HS 5th") %>% 
  mutate(Use_Std = (Use - mean(Use))/sd(Use),
         Use_Joint = (Use + Use_School),
         Use_Joint_Std = (Use_Joint - mean(Use_Joint))/sd(Use_Joint),
         BESA_Other_Std = (BESA_Other - mean(BESA_Other))/sd(BESA_Other),
         BESA_Subj_Std = (BESA_Subj - mean(BESA_Subj))/sd(BESA_Subj),
         BESA_Total = (BESA_Other) + (BESA_Subj),
         BESA_Total_Std = (BESA_Total - mean(BESA_Total))/sd(BESA_Total),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Exposure_Score_Std = (Exposure_Score - mean(Exposure_Score))/sd(Exposure_Score),
         Group = "DLI-5")

DLI5_FCT <- FCT_Master %>% 
  filter(Group == "Bilingual HS 5th") %>% 
  mutate(Use_Std = (Use - mean(Use))/sd(Use),
         Use_Joint = (Use + Use_School),
         Use_Joint_Std = (Use_Joint - mean(Use_Joint))/sd(Use_Joint),
         BESA_Other_Std = (BESA_Other - mean(BESA_Other))/sd(BESA_Other),
         BESA_Subj_Std = (BESA_Subj - mean(BESA_Subj))/sd(BESA_Subj),
         BESA_Total = (BESA_Other) + (BESA_Subj),
         BESA_Total_Std = (BESA_Total - mean(BESA_Total))/sd(BESA_Total),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Exposure_Score_Std = (Exposure_Score - mean(Exposure_Score))/sd(Exposure_Score),
         Group = "DLI-5")


## Mood
DLI5_Subjunctive_EPT <- DLI5_EPT %>% 
  filter(Structure == "Subjunctive") %>% 
  write_csv(here("./CSV Files/DLI-5/DLI-5 Subjunctive EPT.csv"))

DLI5_Subjunctive_FCT <- DLI5_FCT %>% 
  filter(Structure == "Subjunctive") %>% 
  write_csv(here("./CSV Files/DLI-5/DLI-5 Subjunctive FCT.csv"))

DLI5_Indicative_EPT <- DLI5_EPT %>% 
  filter(Structure == "Indicative") %>% 
  write_csv(here("./CSV Files/DLI-5/DLI-5 Indicative EPT.csv"))

DLI5_Indicative_FCT <- DLI5_FCT %>% 
  filter(Structure == "Indicative") %>% 
  write_csv(here("./CSV Files/DLI-5/DLI-5 Indicative FCT.csv"))

DLI5_Mood_EPT <- rbind(DLI5_Subjunctive_EPT, DLI5_Indicative_EPT) %>% 
  write_csv(here("./CSV Files/DLI-5/DLI-5 Mood EPT.csv"))

DLI5_Mood_FCT <- rbind(DLI5_Subjunctive_FCT, DLI5_Indicative_FCT) %>% 
  write_csv(here("./CSV Files/DLI-5/DLI-5 Mood FCT.csv"))


## DOM
DLI5_DOM_Subj_EPT <- DLI5_Subjunctive_EPT %>% 
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Mood = "Subjunctive")

DLI5_DOM_Ind_EPT <- DLI5_Indicative_EPT %>% 
  filter(Property == "Intensional indicative") %>% 
  mutate(Mood = "Indicative")

DLI5_DOM_EPT <- rbind(DLI5_DOM_Subj_EPT, DLI5_DOM_Ind_EPT) %>% 
  mutate(Structure = "DOM") %>% 
  write_csv(here("./CSV Files/DLI-5/DLI-5 DOM EPT.csv"))

DLI5_DOM_FCT <- DLI5_FCT %>% 
  filter(Structure == "DOM")  %>% 
  mutate(Mood = "Indicative",
         Structure = "DOM") %>% 
  write_csv(here("./CSV Files/DLI-5/DLI-5 DOM FCT.csv"))


# 5th grade monolingual HS students
MLS5_EPT <- EPT_Master %>% 
  filter(Group == "Monolingual HS 5th") %>% 
  mutate(Use_Std = (Use - mean(Use))/sd(Use),
         Use_Joint = (Use + Use_School),
         Use_Joint_Std = (Use_Joint - mean(Use_Joint))/sd(Use_Joint),
         BESA_Other_Std = (BESA_Other - mean(BESA_Other))/sd(BESA_Other),
         BESA_Subj_Std = (BESA_Subj - mean(BESA_Subj))/sd(BESA_Subj),
         BESA_Total = (BESA_Other) + (BESA_Subj),
         BESA_Total_Std = (BESA_Total - mean(BESA_Total))/sd(BESA_Total),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Exposure_Score_Std = (Exposure_Score - mean(Exposure_Score))/sd(Exposure_Score),
         Group = "MLS-5")

MLS5_FCT <- FCT_Master %>% 
  filter(Group == "Monolingual HS 5th") %>% 
  mutate(Use_Std = (Use - mean(Use))/sd(Use),
         Use_Joint = (Use + Use_School),
         Use_Joint_Std = (Use_Joint - mean(Use_Joint))/sd(Use_Joint),
         BESA_Other_Std = (BESA_Other - mean(BESA_Other))/sd(BESA_Other),
         BESA_Subj_Std = (BESA_Subj - mean(BESA_Subj))/sd(BESA_Subj),
         BESA_Total = (BESA_Other) + (BESA_Subj),
         BESA_Total_Std = (BESA_Total - mean(BESA_Total))/sd(BESA_Total),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Exposure_Score_Std = (Exposure_Score - mean(Exposure_Score))/sd(Exposure_Score),
         Group = "MLS-5")


## Mood
MLS5_Subjunctive_EPT <- MLS5_EPT %>% 
  filter(Structure == "Subjunctive") %>% 
  write_csv(here("./CSV Files/MLS-5/MLS-5 Subjunctive EPT.csv"))

MLS5_Subjunctive_FCT <- MLS5_FCT %>% 
  filter(Structure == "Subjunctive") %>% 
  write_csv(here("./CSV Files/MLS-5/MLS-5 Subjunctive FCT.csv"))

MLS5_Indicative_EPT <- MLS5_EPT %>% 
  filter(Structure == "Indicative") %>% 
  write_csv(here("./CSV Files/MLS-5/MLS-5 Indicative EPT.csv"))

MLS5_Indicative_FCT <- MLS5_FCT %>% 
  filter(Structure == "Indicative") %>% 
  write_csv(here("./CSV Files/MLS-5/MLS-5 Indicative FCT.csv"))

MLS5_Mood_EPT <- rbind(MLS5_Subjunctive_EPT, MLS5_Indicative_EPT) %>% 
  write_csv(here("./CSV Files/MLS-5/MLS-5 Mood EPT.csv"))

MLS5_Mood_FCT <- rbind(MLS5_Subjunctive_FCT, MLS5_Indicative_FCT) %>% 
  write_csv(here("./CSV Files/MLS-5/MLS-5 Mood FCT.csv"))


## DOM
MLS5_DOM_Subj_EPT <- MLS5_Subjunctive_EPT %>% 
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Mood = "Subjunctive")

MLS5_DOM_Ind_EPT <- MLS5_Indicative_EPT %>% 
  filter(Property == "Intensional indicative") %>% 
  mutate(Mood = "Indicative")

MLS5_DOM_EPT <- rbind(MLS5_DOM_Subj_EPT, MLS5_DOM_Ind_EPT) %>% 
  mutate(Structure = "DOM") %>% 
  write_csv(here("./CSV Files/MLS-5/MLS-5 DOM EPT.csv"))

MLS5_DOM_FCT <- MLS5_FCT %>% 
  filter(Structure == "DOM")  %>% 
  mutate(Mood = "Indicative",
         Structure = "DOM") %>% 
  write_csv(here("./CSV Files/MLS-5/MLS-5 DOM FCT.csv"))


# 7th/8th grade bilingual HS students
DLI78_EPT <- EPT_Master %>% 
  filter(Group == "Bilingual HS 7th/8th") %>% 
  mutate(Use_Std = (Use - mean(Use))/sd(Use),
         Use_Joint = (Use + Use_School),
         Use_Joint_Std = (Use_Joint - mean(Use_Joint))/sd(Use_Joint),
         BESA_Other_Std = (BESA_Other - mean(BESA_Other))/sd(BESA_Other),
         BESA_Subj_Std = (BESA_Subj - mean(BESA_Subj))/sd(BESA_Subj),
         BESA_Total = (BESA_Other) + (BESA_Subj),
         BESA_Total_Std = (BESA_Total - mean(BESA_Total))/sd(BESA_Total),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Exposure_Score_Std = (Exposure_Score - mean(Exposure_Score))/sd(Exposure_Score),
         Group = "DLI-7/8")

DLI78_FCT <- FCT_Master %>% 
  filter(Group == "Bilingual HS 7th/8th") %>% 
  mutate(Use_Std = (Use - mean(Use))/sd(Use),
         Use_Joint = (Use + Use_School),
         Use_Joint_Std = (Use_Joint - mean(Use_Joint))/sd(Use_Joint),
         BESA_Other_Std = (BESA_Other - mean(BESA_Other))/sd(BESA_Other),
         BESA_Subj_Std = (BESA_Subj - mean(BESA_Subj))/sd(BESA_Subj),
         BESA_Total = (BESA_Other) + (BESA_Subj),
         BESA_Total_Std = (BESA_Total - mean(BESA_Total))/sd(BESA_Total),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Exposure_Score_Std = (Exposure_Score - mean(Exposure_Score))/sd(Exposure_Score),
         Group = "DLI-7/8")

## Mood
DLI78_Subjunctive_EPT <- DLI78_EPT %>% 
  filter(Structure == "Subjunctive") %>% 
  write_csv(here("./CSV Files/DLI-78/DLI-78 Subjunctive EPT.csv"))

DLI78_Subjunctive_FCT <- DLI78_FCT %>% 
  filter(Structure == "Subjunctive") %>% 
  write_csv(here("./CSV Files/DLI-78/DLI-78 Subjunctive FCT.csv"))

DLI78_Indicative_EPT <- DLI78_EPT %>% 
  filter(Structure == "Indicative") %>% 
  write_csv(here("./CSV Files/DLI-78/DLI-78 Indicative EPT.csv"))

DLI78_Indicative_FCT <- DLI78_FCT %>% 
  filter(Structure == "Indicative") %>% 
  write_csv(here("./CSV Files/DLI-78/DLI-78 Indicative FCT.csv"))

DLI78_Mood_EPT <- rbind(DLI78_Subjunctive_EPT, DLI78_Indicative_EPT) %>% 
  write_csv(here("./CSV Files/DLI-78/DLI-78 Mood EPT.csv"))

DLI78_Mood_FCT <- rbind(DLI78_Subjunctive_FCT, DLI78_Indicative_FCT) %>% 
  write_csv(here("./CSV Files/DLI-78/DLI-78 Mood FCT.csv"))


## DOM
DLI78_DOM_Subj_EPT <- DLI78_Subjunctive_EPT %>% 
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Mood = "Subjunctive")

DLI78_DOM_Ind_EPT <- DLI78_Indicative_EPT %>% 
  filter(Property == "Intensional indicative") %>% 
  mutate(Mood = "Indicative")

DLI78_DOM_EPT <- rbind(DLI78_DOM_Subj_EPT, DLI78_DOM_Ind_EPT) %>% 
  mutate(Structure = "DOM") %>% 
  write_csv(here("./CSV Files/DLI-78/DLI-78 DOM EPT.csv"))

DLI78_DOM_FCT <- DLI78_FCT %>% 
  filter(Structure == "DOM")  %>% 
  mutate(Mood = "Indicative") %>% 
  write_csv(here("./CSV Files/DLI-78/DLI-78 DOM FCT.csv"))


# 8th grade monolingual HS students
MLS78_EPT <- EPT_Master %>% 
  filter(Group == "Monolingual HS 7th/8th") %>% 
  mutate(Use_Std = (Use - mean(Use))/sd(Use),
         Use_Joint = (Use + Use_School),
         Use_Joint_Std = (Use_Joint - mean(Use_Joint))/sd(Use_Joint),
         BESA_Other_Std = (BESA_Other - mean(BESA_Other))/sd(BESA_Other),
         BESA_Subj_Std = (BESA_Subj - mean(BESA_Subj))/sd(BESA_Subj),
         BESA_Total = (BESA_Other) + (BESA_Subj),
         BESA_Total_Std = (BESA_Total - mean(BESA_Total))/sd(BESA_Total),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Exposure_Score_Std = (Exposure_Score - mean(Exposure_Score))/sd(Exposure_Score),
         Group = "MLS-7/8")

MLS78_FCT <- FCT_Master %>% 
  filter(Group == "Monolingual HS 7th/8th") %>% 
  mutate(Use_Std = (Use - mean(Use))/sd(Use),
         Use_Joint = (Use + Use_School),
         Use_Joint_Std = (Use_Joint - mean(Use_Joint))/sd(Use_Joint),
         BESA_Other_Std = (BESA_Other - mean(BESA_Other))/sd(BESA_Other),
         BESA_Subj_Std = (BESA_Subj - mean(BESA_Subj))/sd(BESA_Subj),
         BESA_Total = (BESA_Other) + (BESA_Subj),
         BESA_Total_Std = (BESA_Total - mean(BESA_Total))/sd(BESA_Total),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Exposure_Score_Std = (Exposure_Score - mean(Exposure_Score))/sd(Exposure_Score),
         Group = "MLS-7/8")

## Mood
MLS78_Subjunctive_EPT <- MLS78_EPT %>% 
  filter(Structure == "Subjunctive") %>% 
  write_csv(here("./CSV Files/MLS-78/MLS-78 Subjunctive EPT.csv"))

MLS78_Subjunctive_FCT <- MLS78_FCT %>% 
  filter(Structure == "Subjunctive") %>% 
  write_csv(here("./CSV Files/MLS-78/MLS-78 Subjunctive FCT.csv"))

MLS78_Indicative_EPT <- MLS78_EPT %>% 
  filter(Structure == "Indicative") %>% 
  write_csv(here("./CSV Files/MLS-78/MLS-78 Indicative EPT.csv"))

MLS78_Indicative_FCT <- MLS78_FCT %>% 
  filter(Structure == "Indicative") %>% 
  write_csv(here("./CSV Files/MLS-78/MLS-78 Indicative FCT.csv"))

MLS78_Mood_EPT <- rbind(MLS78_Subjunctive_EPT, MLS78_Indicative_EPT) %>% 
  write_csv(here("./CSV Files/MLS-78/MLS-78 Mood EPT.csv"))

MLS78_Mood_FCT <- rbind(MLS78_Subjunctive_FCT, MLS78_Indicative_FCT) %>% 
  write_csv(here("./CSV Files/MLS-78/MLS-78 Mood FCT.csv"))


## DOM
MLS78_DOM_Subj_EPT <- MLS78_Subjunctive_EPT %>% 
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Mood = "Subjunctive")

MLS78_DOM_Ind_EPT <- MLS78_Indicative_EPT %>% 
  filter(Property == "Intensional indicative") %>% 
  mutate(Mood = "Indicative")

MLS78_DOM_EPT <- rbind(MLS78_DOM_Subj_EPT, MLS78_DOM_Ind_EPT) %>% 
  mutate(Structure = "DOM") %>% 
  write_csv(here("./CSV Files/MLS-78/MLS-78 DOM EPT.csv"))

MLS78_DOM_FCT <- MLS78_FCT %>% 
  filter(Structure == "DOM")  %>% 
  mutate(Mood = "Indicative") %>% 
  write_csv(here("./CSV Files/MLS-78/MLS-78 DOM FCT.csv"))


# Adult HS
HSP_EPT <- EPT_Master %>% 
  filter(Group == "Heritage Adults") %>% 
  mutate(Use_Std = (Use - mean(Use))/sd(Use),
         Use_Joint = (Use + Use_School),
         Use_Joint_Std = (Use_Joint - mean(Use_Joint))/sd(Use_Joint),
         BESA_Other_Std = (BESA_Other - mean(BESA_Other))/sd(BESA_Other),
         BESA_Subj_Std = (BESA_Subj - mean(BESA_Subj))/sd(BESA_Subj),
         BESA_Total = (BESA_Other) + (BESA_Subj),
         BESA_Total_Std = (BESA_Total - mean(BESA_Total))/sd(BESA_Total),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Exposure_Score_Std = (Exposure_Score - mean(Exposure_Score))/sd(Exposure_Score))

HSP_FCT <- FCT_Master %>% 
  filter(Group == "Heritage Adults") %>% 
  mutate(Use_Std = (Use - mean(Use))/sd(Use),
         Use_Joint = (Use + Use_School),
         Use_Joint_Std = (Use_Joint - mean(Use_Joint))/sd(Use_Joint),
         BESA_Other_Std = (BESA_Other - mean(BESA_Other))/sd(BESA_Other),
         BESA_Subj_Std = (BESA_Subj - mean(BESA_Subj))/sd(BESA_Subj),
         BESA_Total = (BESA_Other) + (BESA_Subj),
         BESA_Total_Std = (BESA_Total - mean(BESA_Total))/sd(BESA_Total),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Exposure_Score_Std = (Exposure_Score - mean(Exposure_Score))/sd(Exposure_Score))


## Mood
HSP_Subjunctive_EPT <- HSP_EPT %>% 
  filter(Structure == "Subjunctive") %>% 
  write_csv(here("./CSV Files/Adult HS/Adult HS Subjunctive EPT.csv"))

HSP_Subjunctive_FCT <- HSP_FCT %>% 
  filter(Structure == "Subjunctive") %>% 
  write_csv(here("./CSV Files/Adult HS/Adult HS Subjunctive FCT.csv"))

HSP_Indicative_EPT <- HSP_EPT %>% 
  filter(Structure == "Indicative") %>% 
  write_csv(here("./CSV Files/Adult HS/Adult HS Indicative EPT.csv"))

HSP_Indicative_FCT <- HSP_FCT %>% 
  filter(Structure == "Indicative") %>% 
  write_csv(here("./CSV Files/Adult HS/Adult HS Indicative FCT.csv"))

HSP_Mood_EPT <- rbind(HSP_Subjunctive_EPT, HSP_Indicative_EPT) %>% 
  write_csv(here("./CSV Files/Adult HS/Adult HS Mood EPT.csv"))

HSP_Mood_FCT <- rbind(HSP_Subjunctive_FCT, HSP_Indicative_FCT) %>% 
  write_csv(here("./CSV Files/Adult HS/Adult HS Mood FCT.csv"))


## DOM
HSP_DOM_Subj_EPT <- HSP_Subjunctive_EPT %>% 
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Mood = "Subjunctive")

HSP_DOM_Ind_EPT <- HSP_Indicative_EPT %>% 
  filter(Property == "Intensional indicative") %>% 
  mutate(Mood = "Indicative")

HSP_DOM_EPT <- rbind(HSP_DOM_Subj_EPT, HSP_DOM_Ind_EPT) %>%
  mutate(Structure = "DOM") %>% 
  write_csv(here("./CSV Files/Adult HS/Adult HS DOM EPT.csv"))

HSP_DOM_FCT <- HSP_FCT %>% 
  filter(Structure == "DOM")  %>% 
  mutate(Mood = "Indicative") %>% 
  write_csv(here("./CSV Files/Adult HS/Adult HS DOM FCT.csv"))


## Infinitive
HSP_Infinitive_EPT <- HSP_EPT %>% 
  filter(Structure == "Infinitive") %>% 
  write_csv(here("./CSV Files/Adult HS/Adult HS Infinitive EPT.csv"))

HSP_Infinitive_FCT <- HSP_FCT %>% 
  filter(Structure == "Infinitive") %>% 
  write_csv(here("./CSV Files/Adult HS/Adult HS Infinitive FCT.csv"))


# Adult L2 learners
L2L_EPT <- EPT_Master %>% 
  filter(Group == "L2 Learners") %>% 
  mutate(Use_Std = (Use - mean(Use))/sd(Use),
         Use_Joint = (Use + Use_School),
         Use_Joint_Std = (Use_Joint - mean(Use_Joint))/sd(Use_Joint),
         BESA_Other_Std = (BESA_Other - mean(BESA_Other))/sd(BESA_Other),
         BESA_Subj_Std = (BESA_Subj - mean(BESA_Subj))/sd(BESA_Subj),
         BESA_Total = (BESA_Other) + (BESA_Subj),
         BESA_Total_Std = (BESA_Total - mean(BESA_Total))/sd(BESA_Total),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Exposure_Score_Std = (Exposure_Score - mean(Exposure_Score))/sd(Exposure_Score))

L2L_FCT <- FCT_Master %>% 
  filter(Group == "L2 Learners") %>% 
  mutate(Use_Std = (Use - mean(Use))/sd(Use),
         Use_Joint = (Use + Use_School),
         Use_Joint_Std = (Use_Joint - mean(Use_Joint))/sd(Use_Joint),
         BESA_Other_Std = (BESA_Other - mean(BESA_Other))/sd(BESA_Other),
         BESA_Subj_Std = (BESA_Subj - mean(BESA_Subj))/sd(BESA_Subj),
         BESA_Total = (BESA_Other) + (BESA_Subj),
         BESA_Total_Std = (BESA_Total - mean(BESA_Total))/sd(BESA_Total),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Exposure_Score_Std = (Exposure_Score - mean(Exposure_Score))/sd(Exposure_Score))

## Mood
L2L_Subjunctive_EPT <- L2L_EPT %>% 
  filter(Structure == "Subjunctive") %>% 
  write_csv(here("./CSV Files/Adult L2L/Adult L2L Subjunctive EPT.csv"))

L2L_Subjunctive_FCT <- L2L_FCT %>% 
  filter(Structure == "Subjunctive") %>% 
  write_csv(here("./CSV Files/Adult L2L/Adult L2L Subjunctive FCT.csv"))

L2L_Indicative_EPT <- L2L_EPT %>% 
  filter(Structure == "Indicative") %>% 
  write_csv(here("./CSV Files/Adult L2L/Adult L2L Indicative EPT.csv"))

L2L_Indicative_FCT <- L2L_FCT %>% 
  filter(Structure == "Indicative") %>% 
  write_csv(here("./CSV Files/Adult L2L/Adult L2L Indicative FCT.csv"))

L2L_Mood_EPT <- rbind(L2L_Subjunctive_EPT, L2L_Indicative_EPT) %>% 
  write_csv(here("./CSV Files/Adult L2L/Adult L2L Mood EPT.csv"))

L2L_Mood_FCT <- rbind(L2L_Subjunctive_FCT, L2L_Indicative_FCT) %>% 
  write_csv(here("./CSV Files/Adult L2L/Adult L2L Mood FCT.csv"))


## DOM
L2L_DOM_Subj_EPT <- L2L_Subjunctive_EPT %>% 
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Mood = "Subjunctive")

L2L_DOM_Ind_EPT <- L2L_Indicative_EPT %>% 
  filter(Property == "Intensional indicative") %>% 
  mutate(Mood = "Indicative")

L2L_DOM_EPT <- rbind(L2L_DOM_Subj_EPT, L2L_DOM_Ind_EPT) %>%
  mutate(Structure = "DOM") %>% 
  write_csv(here("./CSV Files/Adult L2L/Adult L2L DOM EPT.csv"))

L2L_DOM_FCT <- L2L_FCT %>% 
  filter(Structure == "DOM")  %>% 
  mutate(Mood = "Indicative") %>% 
  write_csv(here("./CSV Files/Adult L2L/Adult L2L DOM FCT.csv"))


## Infinitive
L2L_Infinitive_EPT <- L2L_EPT %>% 
  filter(Structure == "Infinitive") %>% 
  write_csv(here("./CSV Files/Adult L2L/Adult L2L Infinitive EPT.csv"))

L2L_Infinitive_FCT <- L2L_FCT %>% 
  filter(Structure == "Infinitive") %>% 
  write_csv(here("./CSV Files/Adult L2L/Adult L2L Infinitive FCT.csv"))


# Spanish-dominant comparison adults
SDB_EPT <- EPT_Master %>% 
  filter(Group == "Comparison") %>% 
  mutate(Use_Std = (Use - mean(Use))/sd(Use),
         Use_Joint = (Use + Use_School),
         Use_Joint_Std = (Use_Joint - mean(Use_Joint))/sd(Use_Joint),
         BESA_Other_Std = (BESA_Other - mean(BESA_Other))/sd(BESA_Other),
         BESA_Subj_Std = (BESA_Subj - mean(BESA_Subj))/sd(BESA_Subj),
         BESA_Total = (BESA_Other) + (BESA_Subj),
         BESA_Total_Std = (BESA_Total - mean(BESA_Total))/sd(BESA_Total),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Exposure_Score_Std = (Exposure_Score - mean(Exposure_Score))/sd(Exposure_Score),
         Group = "SDB")

SDB_FCT <- FCT_Master %>% 
  filter(Group == "Comparison") %>% 
  mutate(Use_Std = (Use - mean(Use))/sd(Use),
         Use_Joint = (Use + Use_School),
         Use_Joint_Std = (Use_Joint - mean(Use_Joint))/sd(Use_Joint),
         BESA_Other_Std = (BESA_Other - mean(BESA_Other))/sd(BESA_Other),
         BESA_Subj_Std = (BESA_Subj - mean(BESA_Subj))/sd(BESA_Subj),
         BESA_Total = (BESA_Other) + (BESA_Subj),
         BESA_Total_Std = (BESA_Total - mean(BESA_Total))/sd(BESA_Total),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Exposure_Score_Std = (Exposure_Score - mean(Exposure_Score))/sd(Exposure_Score),
         Group = "SDB")

## Mood
SDB_Subjunctive_EPT <- SDB_EPT %>% 
  filter(Structure == "Subjunctive") %>% 
  write_csv(here("./CSV Files/SDB/SDB Subjunctive EPT.csv"))

SDB_Subjunctive_FCT <- SDB_FCT %>% 
  filter(Structure == "Subjunctive") %>% 
  write_csv(here("./CSV Files/SDB/SDB Subjunctive FCT.csv"))

SDB_Indicative_EPT <- SDB_EPT %>% 
  filter(Structure == "Indicative") %>% 
  write_csv(here("./CSV Files/SDB/SDB Indicative EPT.csv"))

SDB_Indicative_FCT <- SDB_FCT %>% 
  filter(Structure == "Indicative") %>% 
  write_csv(here("./CSV Files/SDB/SDB Indicative FCT.csv"))

SDB_Mood_EPT <- rbind(SDB_Subjunctive_EPT, SDB_Indicative_EPT) %>% 
  write_csv(here("./CSV Files/SDB/SDB Mood EPT.csv"))

SDB_Mood_FCT <- rbind(SDB_Subjunctive_FCT, SDB_Indicative_FCT) %>% 
  write_csv(here("./CSV Files/SDB/SDB Mood FCT.csv"))


## DOM
SDB_DOM_Subj_EPT <- SDB_Subjunctive_EPT %>% 
  filter(Property == "Intensional subjunctive") %>% 
  mutate(Mood = "Subjunctive")

SDB_DOM_Ind_EPT <- SDB_Indicative_EPT %>% 
  filter(Property == "Intensional indicative") %>% 
  mutate(Mood = "Indicative")

SDB_DOM_EPT <- rbind(SDB_DOM_Subj_EPT, SDB_DOM_Ind_EPT) %>% 
  mutate(Structure = "DOM") %>% 
  write_csv(here("./CSV Files/SDB/SDB DOM EPT.csv"))

SDB_DOM_FCT <- SDB_FCT %>% 
  filter(Structure == "DOM")  %>% 
  mutate(Mood = "Indicative") %>% 
  write_csv(here("./CSV Files/SDB/SDB DOM FCT.csv"))


## Infinitive
SDB_Infinitive_EPT <- SDB_EPT %>% 
  filter(Structure == "Infinitive") %>% 
  write_csv(here("./CSV Files/SDB/SDB Infinitive EPT.csv"))

SDB_Infinitive_FCT <- SDB_FCT %>% 
  filter(Structure == "Infinitive") %>% 
  write_csv(here("./CSV Files/SDB/SDB Infinitive FCT.csv"))

