# E. Chung, 2026
# getting scores for memory of target locations at the end of the doors task.

library(tidyverse)

memory_data <- read.csv("res/task_memory_data.csv")

memory_data$train_type <- as.factor(memory_data$train_type)
levels(memory_data$train_type) <- c("Stable", "Variable")

# tidying up data and getting mean memory scores for each sub
memory_data_summ <- memory_data %>% 
  group_by(sub, train_type) %>% 
  summarise(mean_locs_correct = (n_task1 + n_task2)/2,
            proportion_setErr1 = n_setErr1/(n_setErr1 + n_genErr1),
            proportion_setErr2 = n_setErr2/(n_setErr2 + n_genErr2),
            proportion_genErr1 = n_genErr1/(n_setErr1 + n_genErr1),
            proportion_genErr2 = n_genErr2/(n_setErr2 + n_genErr2),
            all_complete = n_comp_and_part + n_complete_only,
            comp_and_part = n_comp_and_part/2,
            complete_only = n_complete_only/2,
            partial_only = n_partial_only/2,
            never_transferred = n_never_transferred/2)

  
write.csv(memory_data_summ, file = "res/task_memory_summary.csv", row.names = FALSE)
