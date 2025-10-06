# E. Chung, 2025
# This script gets the average entropy for:
# 1. each participant, averaged across conditions
# 2. each training group, averaged across participant

library(tidyverse)

entropy_data <- read.csv("res/exp_lt_entropy.csv")

# plotting mean entropy for each participant, averaged across conditions
entropy_data <- entropy_data %>% 
  group_by(sid, train_type) %>% 
  summarise(mean_entropy = mean(entropy))

entropy_data <- entropy_data %>% 
  group_by(train_type) %>% 
  mutate(sd = sd(mean_entropy))

boxplot(mean_entropy ~ train_type, data=entropy_data) 

# getting mean entropy for each group, average across participant
group_means <- entropy_data %>% 
  group_by(train_type) %>% 
  summarise(group_entropy = mean(mean_entropy))
            

