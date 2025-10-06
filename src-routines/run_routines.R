# E.Chung, 2025
# This script gets the entropy score for all transitions between doors in each 
# condition (task-set) during training. 
# Entropy to tell us how much variance there is in a participants search paths from trial to trial.
# Higher entropy also means less routine.

library(tidyverse)

source(file.path(getwd(), "src-routines", "functions_to_compute_routine_scores.R"))
source(file.path(getwd(), "src", "get_subs.R"))

# params--------------------------------
exp <- 'exp_lt'
version <- 'data_sandpit'
n_doors = 21 # 20 doors in total plus home location
session <- 2 # 2: train phase
conditions <- c(1, 2) # number of conditions during train phase

# get data------------------------------
events <- read.csv(file.path('res', paste(paste(exp, "evt", sep='_'), ".csv", sep='')))

data <- data.frame(sub = integer(), ses = integer(), train_type = integer(), 
                   context = integer(), entropy = integer())

subs <- get_subs(version)

# this will produce an entropy score for each trial in the training stage-----------
for (sub in subs) {
  
  sid <- as.numeric(substring(sub,5,7))
  train_type <- events %>% filter(sub==sid, ses==session) %>% pull(train_type)
  train_type <- train_type[1]
  
  # get the trial numbers for each condition
  for (condition in conditions) {
    events_train <- events %>% 
      filter(sub == sid, ses == session, context == condition)
    trials <- unique(events_train$t)
    ntrials <- length(trials)
    
    # create a running matrix which counts of all transitions made, trial by trial
    ## start off with a zero matrix (n_doors x n_doors) for previous, current and total trials
    prev_counts_mat <- matrix(rep(0, times=n_doors*n_doors), nrow=n_doors, ncol=n_doors) 
    current_counts_mat <- prev_counts_mat
    tot_counts_mat <- prev_counts_mat
    
    ## for each trial, take the number of transitions on the current trial  
    ## and add it to the running matrix which counts all preceding transitions 
    for (i in 1:ntrials){
      tdata <- events_train %>% filter(t == trials[i])
      door_selections <- c(21, tdata$door) # get door selections for each trial (21 is home location)
      
      prev_counts_mat <- tot_counts_mat 
      current_counts_mat <- data_2_counts_matrix(door_selections, n_doors)
          
      tot_counts_mat <- prev_counts_mat + current_counts_mat
      }
    
    # then convert this into a probability matrix out of all trials in that condition
    prob_mat <- p_st1_gs(tot_counts_mat, n_doors) 
    
    # then get entropy for that condition  
    entropy <- H(prob_mat) 
    
    tmp <- data.frame(sid, session, train_type, condition, entropy) 
    data <- rbind(data, tmp)
  }
}

# writing the data into a csv file
write.csv(data,file.path('res',paste(paste(exp,'entropy',sep='_'),'csv', sep='.')))


  
