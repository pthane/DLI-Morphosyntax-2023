library(tidyverse)


# Load data
Verbs <- read_csv("./CSV Files/Lexical Items/Volitional Subjunctive Verbs List.csv")


# Mutate data
Verbs_Final <- Verbs %>% 
  mutate(Davies_freq_centered = Davies_freq - mean(Davies_freq)) %>% 
  mutate(Davies_freq_std = (Davies_freq_centered - mean(Davies_freq_centered))/sd(Davies_freq_centered)) %>% 
  write_csv("./CSV Files/Lexical Items/Subjunctive Centered and Standardized Verb Frequency.csv")