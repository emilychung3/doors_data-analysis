# E. Chung, 2026
# getting scores for memory of target locations at the end of the doors task.

library(tidyverse)

memory_data <- read.csv("C:/Users/echung/UNSW/Learning and Attention Group - Emily PhD project - exp1_data/individual_differences/target_locs_data.csv")

# tidying up data and getting mean memory scores for each sub
memory_data <- memory_data %>% 
  group_by(sub) %>% 
  mutate(mean_locs_correct = (ncorrectA + ncorrectB)/2,
         task_config = as.factor(task_config)) %>% 
  select(-wrong_order)
  

write.csv(memory_data, file = "res/target_memory_data.csv", row.names = FALSE)
