library(tidyverse)


# Load data
## EPT
SDB_EPT <- read_csv("./CSV Files/SDB/SDB Infinitive EPT.csv") %>% 
  mutate(Group = "SDB",
         Short_Group = "SDB")

HSP_EPT <- read_csv("./CSV Files/Adult HS/Adult HS Infinitive EPT.csv") %>% 
  mutate(Group = "Heritage Speakers",
         Short_Group = "HSP")

L2L_EPT <- read_csv("./CSV Files/Adult L2L/Adult L2L Infinitive EPT.csv") %>% 
  mutate(Group = "L2 Learners",
         Short_Group = "L2L")


## FCT
SDB_FCT <- read_csv("./CSV Files/SDB/SDB Infinitive FCT.csv") %>% 
  mutate(Group = "SDB",
         Short_Group = "SDB")

HSP_FCT <- read_csv("./CSV Files/Adult HS/Adult HS Infinitive FCT.csv") %>% 
  mutate(Group = "Heritage Speakers",
         Short_Group = "HSP")

L2L_FCT <- read_csv("./CSV Files/Adult L2L/Adult L2L Infinitive FCT.csv") %>% 
  mutate(Group = "L2 Learners",
         Short_Group = "L2L")


# Separate by syntactic context
## SDB EPT
SDB_EPT_Subject <- SDB_EPT %>% 
  filter(Short_Prop == "Subject")

SDB_EPT_Object <- SDB_EPT %>% 
  filter(Short_Prop == "Object")

SDB_EPT_Preposition <- SDB_EPT %>% 
  filter(Short_Prop == "Preposition")


## SDB FCT
SDB_FCT_Subject <- SDB_FCT %>% 
  filter(Short_Prop == "Subject")

SDB_FCT_Object <- SDB_FCT %>% 
  filter(Short_Prop == "Object")

SDB_FCT_Preposition <- SDB_FCT %>% 
  filter(Short_Prop == "Preposition")


## HSP EPT
HSP_EPT_Subject <- HSP_EPT %>% 
  filter(Short_Prop == "Subject")

HSP_EPT_Object <- HSP_EPT %>% 
  filter(Short_Prop == "Object")

HSP_EPT_Preposition <- HSP_EPT %>% 
  filter(Short_Prop == "Preposition")


## HSP FCT
HSP_FCT_Subject <- HSP_FCT %>% 
  filter(Short_Prop == "Subject")

HSP_FCT_Object <- HSP_FCT %>% 
  filter(Short_Prop == "Object")

HSP_FCT_Preposition <- HSP_FCT %>% 
  filter(Short_Prop == "Preposition")


## L2L EPT
L2L_EPT_Subject <- L2L_EPT %>% 
  filter(Short_Prop == "Subject")

L2L_EPT_Object <- L2L_EPT %>% 
  filter(Short_Prop == "Object")

L2L_EPT_Preposition <- L2L_EPT %>% 
  filter(Short_Prop == "Preposition")


## L2L FCT
L2L_FCT_Subject <- L2L_FCT %>% 
  filter(Short_Prop == "Subject")

L2L_FCT_Object <- L2L_FCT %>% 
  filter(Short_Prop == "Object")

L2L_FCT_Preposition <- L2L_FCT %>% 
  filter(Short_Prop == "Preposition")


# Generate averages
## Subject infinitives
SDB_EPT_Subject_Structure <- aggregate(SDB_EPT_Subject$Comp_Use, list(SDB_EPT_Subject$Structure), FUN = mean, na.rm = TRUE)
SDB_EPT_Subject_Structure <- SDB_EPT_Subject_Structure %>% rename(Structure_Avg = x)
SDB_EPT_Subject_Structure <- left_join(SDB_EPT_Subject, SDB_EPT_Subject_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

SDB_FCT_Subject_Structure <- aggregate(SDB_FCT_Subject$Comp_Use, list(SDB_FCT_Subject$Structure), FUN = mean, na.rm = TRUE)
SDB_FCT_Subject_Structure <- SDB_FCT_Subject_Structure %>% rename(Structure_Avg = x)
SDB_FCT_Subject_Structure <- left_join(SDB_FCT_Subject, SDB_FCT_Subject_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HSP_EPT_Subject_Structure <- aggregate(HSP_EPT_Subject$Comp_Use, list(HSP_EPT_Subject$Structure), FUN = mean, na.rm = TRUE)
HSP_EPT_Subject_Structure <- HSP_EPT_Subject_Structure %>% rename(Structure_Avg = x)
HSP_EPT_Subject_Structure <- left_join(HSP_EPT_Subject, HSP_EPT_Subject_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HSP_FCT_Subject_Structure <- aggregate(HSP_FCT_Subject$Comp_Use, list(HSP_FCT_Subject$Structure), FUN = mean, na.rm = TRUE)
HSP_FCT_Subject_Structure <- HSP_FCT_Subject_Structure %>% rename(Structure_Avg = x)
HSP_FCT_Subject_Structure <- left_join(HSP_FCT_Subject, HSP_FCT_Subject_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

L2L_EPT_Subject_Structure <- aggregate(L2L_EPT_Subject$Comp_Use, list(L2L_EPT_Subject$Structure), FUN = mean, na.rm = TRUE)
L2L_EPT_Subject_Structure <- L2L_EPT_Subject_Structure %>% rename(Structure_Avg = x)
L2L_EPT_Subject_Structure <- left_join(L2L_EPT_Subject, L2L_EPT_Subject_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

L2L_FCT_Subject_Structure <- aggregate(L2L_FCT_Subject$Comp_Use, list(L2L_FCT_Subject$Structure), FUN = mean, na.rm = TRUE)
L2L_FCT_Subject_Structure <- L2L_FCT_Subject_Structure %>% rename(Structure_Avg = x)
L2L_FCT_Subject_Structure <- left_join(L2L_FCT_Subject, L2L_FCT_Subject_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

EPT_Subject <- rbind(SDB_EPT_Subject_Structure, HSP_EPT_Subject_Structure, L2L_EPT_Subject_Structure)
FCT_Subject <- rbind(SDB_FCT_Subject_Structure, HSP_FCT_Subject_Structure, L2L_FCT_Subject_Structure)


## Object infinitives
SDB_EPT_Object_Structure <- aggregate(SDB_EPT_Object$Comp_Use, list(SDB_EPT_Object$Structure), FUN = mean, na.rm = TRUE)
SDB_EPT_Object_Structure <- SDB_EPT_Object_Structure %>% rename(Structure_Avg = x)
SDB_EPT_Object_Structure <- left_join(SDB_EPT_Object, SDB_EPT_Object_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

SDB_FCT_Object_Structure <- aggregate(SDB_FCT_Object$Comp_Use, list(SDB_FCT_Object$Structure), FUN = mean, na.rm = TRUE)
SDB_FCT_Object_Structure <- SDB_FCT_Object_Structure %>% rename(Structure_Avg = x)
SDB_FCT_Object_Structure <- left_join(SDB_FCT_Object, SDB_FCT_Object_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HSP_EPT_Object_Structure <- aggregate(HSP_EPT_Object$Comp_Use, list(HSP_EPT_Object$Structure), FUN = mean, na.rm = TRUE)
HSP_EPT_Object_Structure <- HSP_EPT_Object_Structure %>% rename(Structure_Avg = x)
HSP_EPT_Object_Structure <- left_join(HSP_EPT_Object, HSP_EPT_Object_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HSP_FCT_Object_Structure <- aggregate(HSP_FCT_Object$Comp_Use, list(HSP_FCT_Object$Structure), FUN = mean, na.rm = TRUE)
HSP_FCT_Object_Structure <- HSP_FCT_Object_Structure %>% rename(Structure_Avg = x)
HSP_FCT_Object_Structure <- left_join(HSP_FCT_Object, HSP_FCT_Object_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

L2L_EPT_Object_Structure <- aggregate(L2L_EPT_Object$Comp_Use, list(L2L_EPT_Object$Structure), FUN = mean, na.rm = TRUE)
L2L_EPT_Object_Structure <- L2L_EPT_Object_Structure %>% rename(Structure_Avg = x)
L2L_EPT_Object_Structure <- left_join(L2L_EPT_Object, L2L_EPT_Object_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

L2L_FCT_Object_Structure <- aggregate(L2L_FCT_Object$Comp_Use, list(L2L_FCT_Object$Structure), FUN = mean, na.rm = TRUE)
L2L_FCT_Object_Structure <- L2L_FCT_Object_Structure %>% rename(Structure_Avg = x)
L2L_FCT_Object_Structure <- left_join(L2L_FCT_Object, L2L_FCT_Object_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

EPT_Object <- rbind(SDB_EPT_Object_Structure, HSP_EPT_Object_Structure, L2L_EPT_Object_Structure)
FCT_Object <- rbind(SDB_FCT_Object_Structure, HSP_FCT_Object_Structure, L2L_FCT_Object_Structure)


# Prepositional infinitives
SDB_EPT_Preposition_Structure <- aggregate(SDB_EPT_Preposition$Comp_Use, list(SDB_EPT_Preposition$Structure), FUN = mean, na.rm = TRUE)
SDB_EPT_Preposition_Structure <- SDB_EPT_Preposition_Structure %>% rename(Structure_Avg = x)
SDB_EPT_Preposition_Structure <- left_join(SDB_EPT_Preposition, SDB_EPT_Preposition_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

SDB_FCT_Preposition_Structure <- aggregate(SDB_FCT_Preposition$Comp_Use, list(SDB_FCT_Preposition$Structure), FUN = mean, na.rm = TRUE)
SDB_FCT_Preposition_Structure <- SDB_FCT_Preposition_Structure %>% rename(Structure_Avg = x)
SDB_FCT_Preposition_Structure <- left_join(SDB_FCT_Preposition, SDB_FCT_Preposition_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HSP_EPT_Preposition_Structure <- aggregate(HSP_EPT_Preposition$Comp_Use, list(HSP_EPT_Preposition$Structure), FUN = mean, na.rm = TRUE)
HSP_EPT_Preposition_Structure <- HSP_EPT_Preposition_Structure %>% rename(Structure_Avg = x)
HSP_EPT_Preposition_Structure <- left_join(HSP_EPT_Preposition, HSP_EPT_Preposition_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

HSP_FCT_Preposition_Structure <- aggregate(HSP_FCT_Preposition$Comp_Use, list(HSP_FCT_Preposition$Structure), FUN = mean, na.rm = TRUE)
HSP_FCT_Preposition_Structure <- HSP_FCT_Preposition_Structure %>% rename(Structure_Avg = x)
HSP_FCT_Preposition_Structure <- left_join(HSP_FCT_Preposition, HSP_FCT_Preposition_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

L2L_EPT_Preposition_Structure <- aggregate(L2L_EPT_Preposition$Comp_Use, list(L2L_EPT_Preposition$Structure), FUN = mean, na.rm = TRUE)
L2L_EPT_Preposition_Structure <- L2L_EPT_Preposition_Structure %>% rename(Structure_Avg = x)
L2L_EPT_Preposition_Structure <- left_join(L2L_EPT_Preposition, L2L_EPT_Preposition_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

L2L_FCT_Preposition_Structure <- aggregate(L2L_FCT_Preposition$Comp_Use, list(L2L_FCT_Preposition$Structure), FUN = mean, na.rm = TRUE)
L2L_FCT_Preposition_Structure <- L2L_FCT_Preposition_Structure %>% rename(Structure_Avg = x)
L2L_FCT_Preposition_Structure <- left_join(L2L_FCT_Preposition, L2L_FCT_Preposition_Structure, by = c("Structure" = "Group.1")) %>%  
  mutate(Structure_Percentage = Structure_Avg * 100)

EPT_Preposition <- rbind(SDB_EPT_Preposition_Structure, HSP_EPT_Preposition_Structure, L2L_EPT_Preposition_Structure)
FCT_Preposition <- rbind(SDB_FCT_Preposition_Structure, HSP_FCT_Preposition_Structure, L2L_FCT_Preposition_Structure)


## Combine datasets
EPT <- rbind(EPT_Subject, EPT_Object, EPT_Preposition)
EPT$Group <- factor(EPT$Group, levels = c("SDB", "Heritage Speakers", "L2 Learners"))
EPT$Short_Prop <- factor(EPT$Short_Prop, levels = c("Subject", "Object", "Preposition"))

FCT <- rbind(FCT_Subject, FCT_Object, FCT_Preposition)
FCT$Group <- factor(FCT$Group, levels = c("SDB", "Heritage Speakers", "L2 Learners"))
FCT$Short_Prop <- factor(FCT$Short_Prop, levels = c("Subject", "Object", "Preposition"))

Aggregate <- rbind(EPT, FCT)
Aggregate$Group <- factor(Aggregate$Group, levels = c("SDB", "Heritage Speakers", "L2 Learners"))
Aggregate$Short_Group <- factor(Aggregate$Short_Group, levels = c("SDB", "HSP", "L2L"))
Aggregate$Short_Prop <- factor(Aggregate$Short_Prop, levels = c("Subject", "Object", "Preposition"))


# Generate plots
## Production data
EPT %>% 
  ggplot(aes(x = Group, y = Structure_Percentage, fill = Short_Prop)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(breaks = seq(0, 100, 20),
                     limits = c(0, 100)) +
  labs(title = "Infinitival Production by Task, Position, and Group", y = "Percentage of Infinitival Responses", fill = "Position") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"))


## Selection data
FCT %>% 
  ggplot(aes(x = Group, y = Structure_Percentage, fill = Short_Prop)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(breaks = seq(0, 100, 20),
                     limits = c(0, 100)) +
  labs(title = "Infinitival Selection by Task, Position, and Group", y = "Percentage of Infinitival Responses", fill = "Position") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"))


## Combined
Aggregate %>% 
  ggplot(aes(x = Short_Group, y = Structure_Percentage, fill = Short_Prop)) +
  facet_grid(cols = vars(Task)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(breaks = seq(0, 100, 20),
                     limits = c(0, 100)) +
  labs(title = "Infinitival Production by Task, Position, and Group", x = "Group", y = "Percentage of Infinitival Responses", fill = "Position") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text.x = element_text(face = "bold"))
