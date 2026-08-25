# E.Chung 
# running the revised task jumps and weighted TE code. This takes group level 
# data and calculated task jumps for each trial for each participant. weighted
# TE is calculated for each condition (task A and B) during training.

rm(list = ls())

# packages
library(tidyverse)
source("rerun_taskjumps_TE/get_task_jumps_EC.R")

# get task jumps ==============================================================
# get event data
events <- read.csv("res/exp_lt_evt.csv")
events <- events %>% 
  filter(ses == 2) # getting training data only

task_jumps <- get_task_jumps(events)
write.csv(task_jumps, 
          file = "rerun_taskjumps_TE/output/task_jumps_trl.csv")

# summarise task jumps for each train_typ x switch condition ===================
# data for each sub
task_jumps_per_sub <- task_jumps %>% 
  group_by(sub, train_type, switch) %>% 
  summarise(njumps = mean(njumps))
write.csv(task_jumps_per_sub, 
          file = "rerun_taskjumps_TE/output/task_jumps.csv")

# task jump descriptives
task_jumps_descriptives <- task_jumps_per_sub %>%  
  group_by(train_type, switch) %>% 
  summarise(mean = mean(njumps),
            sd = sd(njumps))
write.csv(task_jumps_descriptives, 
          file = "rerun_taskjumps_TE/output/task_jumps_descriptives.csv",
          row.names = FALSE)


