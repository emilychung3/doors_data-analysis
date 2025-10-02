format_data_for_maggi <- function(exp,nsub=1,nses=1,ncontext=1,method="by_trial",specific_doors=TRUE,competitive=FALSE,evaluate_all=FALSE){

# lydia barnes, may 2024
# edited by E.Chung, september 2025
# reads event data from doors task
# re-codes accuracy so that "success" trials (<=4 clicks) that inc. context-irrelevant doors are counted as failures
# classifies trials by whether they provide evidence for knowledge of each target door

# sources

# event data
#events <- read.csv(file.path("res",paste(paste(exp,"evt", sep="_"), ".csv", sep=""))) # KG: events are read in on line 46 of run_maggi.R. Whats the reason for the replication?
# aka, which one should you delete?
events <- events %>% filter(sub==nsub, ses==nses) 
if (nses < 3){
  
  # only look at the first learning block for each context, not the consolidation trials
  first_attempt <- diff(events$context)
  first_attempt <- which(first_attempt==-1)
  events <- events[1:first_attempt,1:ncol(events)]
  
  events <- events %>% filter(context==ncontext) 
} else if (ses == 3){
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
  nstrategies <- 4
  know_doors <- matrix(0,nstrategies,ntrials)

  # search through each trial for evidence that they know doors
  for (i in 1:ntrials){
    
    trial <- trials[i]
    tdata <- events %>% filter(t==trial) 
    missing_evidence <- FALSE
    
    # if they haven't had a chance to experience a door as a target, treat it as context-irrelevant
    if((nses==1) & (trial < max(first_feedback_trial))){
      chance_doors <- doors[which(first_feedback_trial>=trial)]# find the doors which are considered to be found by chance on trial t
      tdata <- tdata %>% mutate(door_cc = case_when(door %in% chance_doors ~ 0, .default=door_cc)) # make door_cc = 0 for all responses at those doors
    } # E.C. do the same if in transfer stage, but for novel transfer doors only
    else if((nses == 3) & (trial < max(first_feedback_trial))){ 

      novel_transfer_doors <- events %>% filter(transfer == 1, door_cc==1) %>% pull(door) %>% unique() # E.C. getting the novel transfer doors
      chance_doors <- novel_transfer_doors[which(first_feedback_trial>=trial)] # KG: it looks like you are applying this to all 3 transfer conditions,
      # but I am guessing this only has an impact in the novel transfer condition as that's the only one where door_cc could be a 1 for these doors. Is that correct?
      tdata <- tdata %>% mutate(door_cc = case_when(door %in% chance_doors ~ 0, .default=door_cc)) 
    }
    
    # if the first door clicked is context-irrelevant, count that as evidence against them knowing any doors
    if(tdata$door_cc[1]==0){
      know_doors[,i] <- 0 # make all strategies k1-k4 = 0 if first doors is incorrect
      
    }else{
      # find out what doors they clicked
      # if we couldn't find their first mistake, they only clicked correct doors
      if(is.infinite(first_errors[trial])){
        these_doors <- unique(tdata$door)
        if(length(these_doors)<4){
          missing_evidence <- TRUE #they may have known more than the clicked doors; but we cant evaluate that so make these NA later
        }
        
      # if we've found their first mistake, find the correct doors they found before the mistake
      }else{
        these_doors <- unique(tdata$door[1:first_errors[trial]])
      }
      
      # apply criteria for whether a door click counts as knowing that door
      # if we're requiring that they prioritise the same subset of doors before we consider clicks deliberate,
      if(specific_doors){   
        door_idx <- match(these_doors,doors) # identify which of the target doors were selected (target door 1-4)
        know_doors[door_idx,i] <- 1 # assign that specific door with a 1
        
        # E.C. if the target was found in less than four moves and no errors were made
        # and if there was an opportunity to learn the target, make doors that were not recorded NA
        if (missing_evidence) {
          not_recorded_idx <- which(is.na(match(doors, these_doors)))
          for (j in 1:length(not_recorded_idx)){ 
            if (trial > first_feedback_trial[not_recorded_idx[j]])
              know_doors[not_recorded_idx[j], i] <- NA
            }
        }
        
        # if we're allowing that people may click different subsets of doors, but must be consistent with the previous trial,
      }else{ # KG: is the below for if we are just generally counting whether people k1, 2, 3... etc, whereas the above is have people learned door 1, door 2, and so on?
        if(i>1){
          # find out what doors they selected on the previous trial
          those_doors <- events %>% filter(t==trials[i-1]) %>% pull(door) %>% unique()

          # if any correct doors were prioritised on both trials, count that as evidence of knowing however many doors 
          # have stayed the same
          # records a value of 1 if there is a match in the current and previous target door selection
          if(any(!is.na(match(these_doors,those_doors)))){
            if(competitive){
              know_doors[length(which(!is.na(match(these_doors,those_doors)))),i] <- 1 # people can only be performing 1 strategy, e.g., if k2 = 1,  k1 = 0
            }else{
              know_doors[1:length(which(!is.na(match(these_doors,those_doors)))),i] <- 1  # if people know k3, k1 and k2 and are also recorded with 1
            }
            
            if(missing_evidence & !evaluate_all){ # records all other strategies as NA if there was no chance record whether there was knowledge.
              know_doors[(length(which(!is.na(match(these_doors,those_doors))))+1):nstrategies,i] <- NA # should this exclude cases where doors have not been learned?
            }
          }
        }
      }
      
    }
  }
  
  # if we're requiring that they continuing selecting e.g. door 11 as their k1 door,  ------------------ E.C. why?? # KG. This was due to previous reasoning that if you only know 1 door, e.g. door 11, then if the next trial shows 
  # if(specific_doors){                                                                                               # evidence that you only know one door and its door 12, then how can you only know 1 door.
  #   # find the order in which they first selected the doors                                                         # I think that given you are tracking knowledge of specific doors, then you don't need this. What do you think?
  #   first_selection <- rep(0,nstrategies)
  #   for (i in 1:nstrategies){
  #     first_selection[i] <- min(which(know_doors[i,]==1))
  #   }
  #   # exclude trials on which previously-selected doors aren't among those selected  
  #   for (i in 1:ntrials){
  #     for (j in 1:nstrategies){ # for each door in each trial,
  #       if(trials[i]>first_selection[j] & know_doors[j,i]==0){ # if there is previous evidence of knowing that door and there is no current evidence for knowledge of that door
  #         know_doors[,i] <- 0 # make evidence for all the doors in that row 0
  #       }
  #     }
  #   }  
  #   # sort by which doors were selected first
  #   know_doors <- know_doors[order(first_selection),]
  # }
 
  samples <- trials 
}

# format
strategies <- data.frame(samples,k1=know_doors[1,],k2=know_doors[2,],k3=know_doors[3,],k4=know_doors[4,])
return(strategies)

}
