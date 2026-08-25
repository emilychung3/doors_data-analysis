# E. Chung, 2026
# this code gets task jumps scores for each trial, ignoring selections on never
# relevant doors.

task_jumps <- function(data) {
  
  grp_jumps <- data.frame(sub = integer(), t = integer(), task_jumps = integer())
  
  for (sub in subs) {
    
    sid = subs[sub]
    
    ntrials <- total_trials %>% filter(sub == sid) %>% pull(ntrials) # this is the total during training

    
    for (trial in 1:(ntrials*2)) { 
      tmp <- training_data %>% filter(sub == sid, t == trial)
      cc_oc_clicks <- list(tmp$door_cc, tmp$door_oc)
    
    # get the events when a context-correct or other context click was made
    # and create a list of door_cc and door_oc events with first row for door_cc 
    # and second row for door_oc
    relevant_idxs <- c()
    for (i in 1:length(cc_oc_clicks[[1]])){ # keeping events when a click on task 1 or 2 was made
      if (abs(cc_oc_clicks[[1]][i]-cc_oc_clicks[[2]][i]) == 1) {
        relevant_idxs <- c(relevant_idxs, i)
      }
    }
    
    all_task_clicks <- matrix(NA, nrow = 2, ncol = length(relevant_idxs))
    for (i in 1:length(relevant_idxs)) {
      this_event <- relevant_idxs[i]
      all_task_clicks[1, i] <- cc_oc_clicks[[1]][this_event]
      all_task_clicks[2, i] <- cc_oc_clicks[[2]][this_event]
    }
    
    # check how many times a jump was made from one context to another
    # i.e., when the events go from door_cc (or door_oc) = 1 to door_cc == 0 
    # (and vice versa)
    if (ncol(all_task_clicks) > 1){
      jumps <- abs(diff(all_task_clicks[1, ]))
      njumps <- sum(jumps)
    } else if (ncol(all_task_clicks) == 1) {
      njumps <- 0
    }
    
    jumps_data <- data.frame(sub = sid, t = trial, task_jumps = njumps)
    grp_jumps <- rbind(grp_jumps, jumps_data)
    }
  }
  
  
  return(grp_jumps)
}
  