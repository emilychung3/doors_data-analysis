# E.Chung, 2026
# This script includes the calculation of a weighted transition entropy (wTE) score
# which is a modification to the initial calculation of entropy.
# This wTE score weights the log probability of each by the probability of 
# that transition out of the *total number of transitions* in the calculation of entropy.
# This is in contrast to the initial calculation of TE which weight the log probability
# of each transition (state s --> s')  by the probability of that transition out 
# of the total number of transitions out of state s.
# TE tells us how much variance there is in a participants' routine across all
# trials in the same context. Higher TE also means less routine.

# This script can be run to calculate TE for all trials, switch trials only and
# stay trials only. 

rm(list = ls())

library(tidyverse)

source(file.path(getwd(), "rerun_taskjumps_TE/KG_code", "TE_functions_KG.R"))
source(file.path(getwd(), "src", "get_subs.R"))

# params--------------------------------
exp <- 'exp_lt'
version <- 'data_sandpit'
n_doors = 21 # 20 doors in total plus home location
session <- 2 # 2: train phase
conditions <- c(1, 2) # contexts during train phase

# get data------------------------------
events <- read.csv(file.path('res', paste(paste(exp, "evt", sep='_'), ".csv", sep='')))

data <- data.frame(sub = integer(), ses = integer(), train_type = integer(), 
                   context = integer(), TE = integer(), weightedTE = integer())

subs <- get_subs(version)

# this will produce an entropy score for each trial in the training stage-----------
for (sub in subs) {
  
  sid <- as.numeric(substring(sub,5,7))
  train_type <- events %>% filter(sub==sid, ses==session) %>% pull(train_type)
  train_type <- train_type[1]
  
  # get the trial numbers in each condition
  for (condition in conditions) {
    # get trials for all trials, switch trials only, or stay trials only.
    events_train <- events %>% 
      
      # filter(sub == sid, ses == session, context == condition) # all trials
      # filter(sub == sid, ses == session, context == condition, switch == 1)  # switch trials only
      filter(sub == sid, ses == session, context == condition, switch == 0) # stay trials only
    
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
    
    # then get the entropy for each row of prob_mat and sum across rows
    entropy_per_row <- apply(prob_mat, 1, H)
    TE <- sum(entropy_per_row) # regular entropy
    
    # i will also get the weighted entropy
    weightedTE <- get_wMTE(tot_counts_mat, prob_mat)
    
    tmp <- data.frame(sid, session, train_type, condition, TE, weightedTE) 
    data <- rbind(data, tmp)
  }
}

# writing the data into a csv file
# write.csv(data,file.path('res',paste(paste(exp,'weighted_entropy', sep='_'),'csv', sep='.')), row.names = FALSE)
# write.csv(data,file.path('res',paste(paste(exp,'weighted_entropy_switch_only', sep='_'),'csv', sep='.')), row.names = FALSE)
write.csv(data,file.path('res',paste(paste(exp,'weighted_entropy_stay_only', sep='_'),'csv', sep='.')), row.names = FALSE)
  
