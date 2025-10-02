# Emily Chung, 2025
# This script gets the routine variability score for each participant. 
# Routine variability uses a calculation of entropy to tell us how much variance there is in 
# a participants search paths in the trainin stage of the doors task.

library(tidyverse)

source(file.path(getwd(), "src-routines", "functions_to_compute_routine_scores.R"))
source(file.path(getwd(), "src", "get_subs.R"))

# params--------------------------------
exp <- 'exp_lt'
n_doors = 20
session <- 2 # 1: task acquisition phase, 2: train phase, 3: learning transfer test
conditions <- c(1, 2)

# get data------------------------------
events <- read.csv(file.path('res', paste(paste(exp, "evt", sep='_'), ".csv", sep='')))

# get routine scores for each condition in each sess
subs <- get_subs(version)

# this will produce an entropy score for each trial in the training stage
for (sub in subs) {
  
  sid <- as.numeric(substring(sub,5,7))

  for (condition in conditions) {
    events <- events %>% filter(sub == sid, ses == session, context == condition)
    trials <- events %>% 
      pull(t) %>% 
        unique()
    
    # create a matrix which counts of all transitions made across trials  
    # this takes the number of transitions on the current trial and adds it to a running matrix which counts all preceding transitions 
    for (trial in trials){
      door_selections <- events %>% filter(t == trials[trial]) #get door selections for each trial
      
      # if there is at least one door transition on that trial
      if(nrow(door_selections) > 1){ 
        if (trials[trial] == 1) {
          current_counts_mat <- data_2_counts_matrix(door_selections$door, n_doors) 
        } else{
          prev_counts_mat <- current_counts_mat 
          current_counts_mat <- data_2_counts_matrix(door_selections$door, n_doors) + prev_counts_mat
        }
      }
      # if there were no transitions on that trial (i.e., target found in one move)
      else{ 
        # transitions for the current trial are all zero
        current_counts_mat <- matrix(rep(0, times=n_doors*n_doors), nrow=n_doors, ncol=n_doors) 
        if (trials[trial] > 1){ # add this matrix to prev_counts_matrix
          current_counts_mat <- prev_counts_mat
        }
      }
    
  
    #p_st1_gs <- p_st1_gs(current_counts_mat, n_doors) #then convert this in a probability matrix out of all trials in that condition
    
    #entropy <- H(events, n_doors) #then get entropy for that condition
    }
  }
}


  
