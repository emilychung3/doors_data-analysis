format_data_for_maggi_revised <- function(exp,nsub=1,nses=1,ncontext=1,method="by_trial"){

# lydia barnes, may 2024
# updated by E. Chung, september 2025
# reads event data from doors task
# re-codes accuracy so that "success" trials (<=4 clicks) that inc. context-irrelevant doors are counted as failures
# classifies trials by whether they provide evidence for knowledge of each target door

# sources

# event data
events <- read.csv(file.path("res",paste(paste(exp,"evt", sep="_"), ".csv", sep=""))) 
events <- events %>% filter(sub==nsub, ses==nses) 
if (nses < 3){
  
  # only look at the first learning block for each context, not the consolidation trials
  first_attempt <- diff(events$context)
  first_attempt <- which(first_attempt==-1)
  events <- events[1:first_attempt,1:ncol(events)]
  
  events <- events %>% filter(context==ncontext) 
} else if (nses == 3){
  events <- events %>% filter(transfer == ncontext)
  }

                                                  
# find the first time each door is the target (their first opportunity to learn)
doors <- events %>% filter(door_cc==1) %>% pull(door) %>% unique() # target doors for current context
target_idx <- c(which(diff(events$t)==1),length(events$door)) # row number for last response in each trial
targets <- events$door[target_idx] # get the target door for that trial
first_feedback <- rep(0,1,length(doors)) 
first_feedback_trial <- first_feedback
for (i in 1:length(doors)){ # getting row numbers for the first row/trial where target is shown at each target loc
  first_feedback[i] <- target_idx[min(which(targets==doors[i]))]
  first_feedback_trial[i] <- events$t[first_feedback[i]]
}


# -------------------------------------------------------------------------
# analyse by trial

if(method=="by_trial"){
  # find the point in each trial where they made their first mistake
  first_errors <- rep(0,1,max(unique(events$t)))# make an vector of zeros with the length of the number of trials
  for (i in unique(events$t)){ 
    tdata <- events %>% filter(t==i) # for each trial
    try(if(tdata$door_cc[1]==0){ # if first response is incorrect
      first_errors[i] <- 1 # make that trial = 1 in first_errors vector
      }else { # first response is correct
        first_errors[i] <- min(which(diff(tdata$door_cc)==-1)) # find the click before their first mistake after clicking on a context relevant door
        }, silent = TRUE)
  } 
  
  # preallocate arrays
  trials <- unique(events$t)
  ntrials <- length(trials)
  ndoors <- 4
  nstrategies <- ndoors
  count_evidence <- matrix(0,ndoors,ntrials) # to count to evidence for or against knowledge of specific doors
  know_doors <- matrix(0,nstrategies,ntrials) # to record evidence for knowledge of any subset of 1-4 doors

  # search through each trial for evidence that they know doors
  for (i in 1:ntrials){
    
    trial <- trials[i]
    tdata <- events %>% filter(t==trial) 
    missing_evidence <- FALSE
    
    # if they haven't had a chance to experience a door as a target, treat it as context-irrelevant
    if((nses==1) & (trial < max(first_feedback_trial))){
      chance_doors <- doors[which(first_feedback_trial>=trial)]# find the doors which are considered to be found by chance on trial t
      tdata <- tdata %>% mutate(door_cc = case_when(door %in% chance_doors ~ 0, .default=door_cc)) # make door_cc = 0 for all responses at those doors
    } # do the same if in transfer stage, but for novel transfer doors only
    else if((nses == 3) & (trial < max(first_feedback_trial))){ 

      novel_transfer_doors <- events %>% filter(transfer == 1, door_cc==1) %>% pull(door) %>% unique() # E.C. getting the novel transfer doors
      chance_doors <- novel_transfer_doors[which(first_feedback_trial>=trial)] 
      tdata <- tdata %>% mutate(door_cc = case_when(door %in% chance_doors ~ 0, .default=door_cc)) 
    }
    
    # if the first door clicked is context-irrelevant, count that as evidence against them knowing any doors
    if(tdata$door_cc[1]==0){
      count_evidence [, i] <- 0 
      know_doors[,i] <- 0 # make all strategies k1-k4 = 0 if first doors is incorrect
      
    }else{
      # find out what doors they clicked
      # if we couldn't find their first mistake, they only clicked correct doors
      if(is.infinite(first_errors[trial])){
        these_doors <- unique(tdata$door)
        if(length(these_doors)<4){ # ---------------- or if theres a door which was never clicked
          missing_evidence <- TRUE #they may have known more than the clicked doors; but we cant evaluate that so make these NA later
        }
        
      # if we've found their first mistake, find the correct doors they found before the mistake
      }else{
        these_doors <- unique(tdata$door[1:first_errors[trial]])
      }
      
      # counting evidence for knowledge of each door
      door_idx <- match(these_doors,doors) # identify which of the target doors were selected (target door 1-4)
      count_evidence[door_idx,i] <- 1 # assign that specific door with a 1
        
      # then sum the number of of 1's in a column of count evidence
      current_strat <- sum(count_evidence[ , i], na.rm = TRUE)
      know_doors[current_strat,i] <- 1
      
      # if the target was found in less than four moves and no errors were made
      # and if there was previously an opportunity to learn the target, 
      # make doors that were not recorded NA
      if (missing_evidence == TRUE) {
        not_recorded_idx <- which(is.na(match(doors, these_doors)))
        
        for (j in 1:length(not_recorded_idx)){ 
          if (trial > first_feedback_trial[not_recorded_idx[j]])
            count_evidence[not_recorded_idx[j], i] <- NA
        } 
      }
      
      # if there is an NA in count evidence, check if the last nonNA value >0 for that door
      # if yes, then add 1 to k4 for that door
      if(any(is.na(count_evidence[ , i]))) {
        missing_doors <- which(is.na(count_evidence[ , i])) 
        for (d in missing_doors){
          last_non_NA <- max(which(!is.na(count_evidence[d, 1:i])))
          if (count_evidence[d, last_non_NA] > 0) {
            know_doors[current_strat,i] <- 0 #reallocate evidence to reflect knowledge of other doors that weren't recorded
            current_strat <- current_strat + 1 
            know_doors[current_strat,i] <- 1}
          } 
        }
      }
    }
  }
  
 
  samples <- trials 


# format
strategies <- data.frame(samples,k1=know_doors[1,],k2=know_doors[2,],k3=know_doors[3,],k4=know_doors[4,])
return(strategies)

}
