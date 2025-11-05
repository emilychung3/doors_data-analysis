#E.Chung, August 2024
library(tidyverse)

# read data
dat <- read.csv("res/exp_lt_trl.csv")
entropy_dat <- read.csv("res/exp_lt_entropy.csv")

# get training data
training_dat <- dat %>% 
  filter(ses == 2)  %>%
  group_by(sub, train_type, switch) %>% 
  summarise(mean_task_jumps  = mean(context_changes),
         mean_gen_error = mean(general_errors)) 

mean_entropy <- entropy_dat %>% 
  group_by(train_type) %>% 
  summarise(mean_entropy = mean(entropy))

# plot task jumps
boxplot(mean_task_jumps ~ train_type*switch, 
        data = training_dat,
        names = c("stable:stay", "var:stay", "stable:switch", "var:switch"),
        xlab = "train_type:trial_type",
        ylab = "mean_task_jumps")  

# plot general errors
boxplot(mean_gen_error ~ train_type*switch, 
        data = training_dat,
        names = c("stable:stay", "var:stay", "stable:switch", "var:switch"),
        xlab = "train_type:trial_type",
        ylab = "mean_general_error") 

# plot routines
boxplot(entropy ~ train_type, 
        data = entropy_dat) 
