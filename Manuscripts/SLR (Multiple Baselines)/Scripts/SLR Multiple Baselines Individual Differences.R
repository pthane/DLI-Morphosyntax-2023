library(tidyverse)
library(here)


# Load data
## Load CSVs
DOM_EPT <- read_csv(here("Manuscripts", "SLR (Multiple Baselines)", "Data", "SLR Multiple Baselines DOM Production.csv")) %>% 
  mutate(Structure = "DOM") %>% 
  select(!Mood)

DOM_FCT <- read_csv(here("Manuscripts", "SLR (Multiple Baselines)", "Data", "SLR Multiple Baselines DOM Selection.csv")) %>% 
  mutate(Structure = "DOM") %>% 
  select(!Mood)


## Create dataframe
Production_Group <- DOM_EPT %>%
  filter(!is.na(Response)) %>%
  group_by(Part_ID, Group) %>%
  summarize(Total_Structure = sum(Response, na.rm = TRUE),
            Total_Responses = sum(!is.na(Response)),
            Prod_Ratio = Total_Structure/Total_Responses) %>% 
  mutate(Production = (Prod_Ratio*100))

Selection_Group <- DOM_FCT %>%
  filter(!is.na(Response)) %>%
  group_by(Part_ID, Group) %>%
  summarize(Total_Structure = sum(Response, na.rm = TRUE),
            Total_Responses = sum(!is.na(Response)),
            Sel_Ratio = Total_Structure/Total_Responses) %>% 
  mutate(Selection = (Sel_Ratio*100))


Aggregate = left_join(Production_Group, Selection_Group, by = c("Part_ID", "Group")) %>% 
  mutate(Sum = (Production + Selection)) %>% 
  rename(Number_Produced = Total_Structure.x,
         Number_Selected = Total_Structure.y,
         EPT_Total = Total_Responses.x,
         FCT_Total = Total_Responses.y) %>% 
  select(!c(Prod_Ratio, Sel_Ratio))
Aggregate$Group <- factor(Aggregate$Group, levels = c("MLS-5", "DLBE-5", "MLS-7/8", "DLBE-7/8", "Adults"))


# Generate plot
Ind_Diffs <- Aggregate %>% 
  ggplot(aes(x = Production, y = Selection, color = Group)) +
  geom_jitter() +
  scale_x_continuous(breaks = seq (0, 100, 20),
                     limits = c(-5, 105)) +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(-5, 105)) +
  labs(x = "Percentage of DOM produced", y = "Percentage of DOM selected", title = "Individual Production and Selection Rates", color = "Group") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold", size = 10))

Ind_Diffs

ggsave(filename = here("Manuscripts", "SLR (Multiple Baselines)", "Graphs", "SLR Multiple Baselines Figure 6.pdf"),
       plot = Ind_Diffs,
       device = "pdf",
       width = 6.5,
       height = 3,
       units = "in")


# Highlight categorical producers
DOM_Joint <- rbind(DOM_EPT, DOM_FCT) %>% 
  group_by(Part_ID, Group) %>%
  summarize(BESA_Total    = first(na.omit(BESA_Total)),
            Use_Joint     = first(na.omit(Use_Joint)),
            Exposure_Group = first(na.omit(Exposure_Group)),
            .groups = "drop")

Categorical_Users <- Aggregate %>%
  filter(Sum == 200) %>%
  select(Part_ID, Group, EPT_Total) %>%
  left_join(DOM_Joint %>%
              group_by(Part_ID, Group) %>%
              summarize(BESA_Total = first(na.omit(BESA_Total)),
                        Use_Joint = first(na.omit(Use_Joint)),
                        Exposure_Group = first(na.omit(Exposure_Group)),
                        .groups = "drop"),
            by = c("Part_ID", "Group"))
