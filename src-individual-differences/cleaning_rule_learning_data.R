# E.Chung, 2025
# This script is used to clean the self-report rule learning data collected from
# Qualtrics. Data for this measure was collected in two separate data files 
# so I am cleaning both data files and compiling them into the same file.

library(tidyverse)
library(dplyr)

data_path <- file.path("C:/Users/echung/UNSW/Learning and Attention Group - Emily PhD project - exp1_data/individual_differences")
data_files <- c('doors_rule_learning1.csv', 'doors_rule_learning2.csv')

clean_data <- lapply(data_files, function(f) {
  rule_learning_data <- read.csv(file.path(data_path, f))
  
  #rename columns and remove unneeded columns
  rule_learning_data <- rule_learning_data %>% 
    rename('sub' = 'Q1',
           'acq_rule_score' = 'Q2_1',
           'acq_open_resp' = 'Q3',
           'train_rule_score' = 'Q4_1',
           'train_open_resp' = 'Q5') %>% 
    select(sub, acq_rule_score, acq_open_resp, train_rule_score, train_open_resp)
  
  #remove unneeded rows
  if (f == 'doors_rule_learning1.csv'){
    rule_learning_data <- rule_learning_data[-c(1:7), ]
  } else if (f == 'doors_rule_learning2.csv'){
    rule_learning_data <- rule_learning_data[-c(1, 2), ]
  }
  
  # changing data type for sub, acq_rule_score and train_rule_score
  rule_learning_data$sub <- as.numeric(rule_learning_data$sub)
  rule_learning_data$acq_rule_score <- as.numeric(rule_learning_data$acq_rule_score)
  rule_learning_data$train_rule_score <- as.numeric(rule_learning_data$train_rule_score)
  
  rule_learning_data
  }
)

fnl <- bind_rows(clean_data) %>% 
  arrange(sub)


write.csv(fnl, file = "res/rule_learning_data.csv", row.names = FALSE)