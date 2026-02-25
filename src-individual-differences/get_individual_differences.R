#E. Chung, 2026
# this script is used to bind all the individual differences data into one csv file.

library(tidyverse)
library(dplyr)

# reading all the data files
corsi <- read.csv('res/corsi_scores.csv')
digit_span <- read.csv('res/digit_span_scores.csv')
self_report <- read.csv('res/rule_learning_data.csv')

# merging data
merged_data <- corsi %>%
  inner_join(digit_span, by = "sub") %>%
  inner_join(self_report, by = "sub")


# adding a columns with group membership
group_list <- rep(c("Stable", "Variable"), times = nrow(merged_data)/2)
merged_data <- merged_data %>% 
  mutate(train_type = group_list)

merged_data <- merged_data %>%
  relocate(sub, train_type, task_config, .before = everything())


write.csv(merged_data, file = "res/all_individual_differences.csv", row.names = FALSE)
