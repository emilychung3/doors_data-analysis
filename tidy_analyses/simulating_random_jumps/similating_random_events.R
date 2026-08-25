# E. Chung, 2026
# In this script, I am generating some random event data to simulate what a task jumps
# would look like for a participant who is performing randomly in the stable and
# variable group?

rm(list = ls())

library(tidyverse)
source(file.path("src", "get_switch.R"))
source(file.path("src/get_setting_stability.R"))
source(file.path("src/get_task_jumps_EC.R"))
set.seed(123)

# To do this, I am going to generate a sequence of trials for a participant in the
# stable and variable group.

task_order <- c(1, 2)  # using 1 and 2 to be consistent with the src code
ntrials <- 160 # total number of trials per task
subs <- c(1, 2)
p_switch <- c(0.05, 0.3)

# I am going to assign a set of target location for each task and each trial.
doors <- list(
  c(1, 5, 13, 18), # i have randomly picked these numbers 
  c(3, 7, 12, 19)
)

grp_data <- data.frame(sub = integer(), ses = integer(), train_type = integer(),
                       t = integer(), switch = integer(), door = integer(), 
                       door_cc = integer(), door_oc = integer())

# for each fake participant
for (sub in subs) {
  sid = subs[sub]
  ses = 2

  nsegments <- ntrials*p_switch[sub] # split n trials into this many segments
  train_type <- sid # 1: stable, 2: variable
  
  # number of trials in each segment of each task
  taskA_segments <- c(0, sort(sample(2:(ntrials-1), replace = FALSE, nsegments - 1)), ntrials)
  taskA_runs <- diff(taskA_segments)
  taskB_segments <- c(0, sort(sample(2:(ntrials-1), replace = FALSE, nsegments - 1)), ntrials)
  taskB_runs <- diff(taskB_segments)
  
  # now create those task segments
  trials <- c()
  targets <- c()
  for (i in 1:nsegments){
    trials <- c(trials, rep(1, each = taskA_runs[i]), rep(2, each = taskB_runs[i]))
      }
  
  # Then, I am going to generate a use a for loop to generate a random door selections
  # on each trial with the last selection being on the target location. I want this 
  # data frame to look like the evts data frame generated from get_data.R
  
  simulated_events <- data.frame(sub = integer(), ses = integer(), train_type = integer(),
                                 t = integer(), task = integer(), door = integer())
  
  
  for (t in 1:length(trials)) {
    task <- trials[[t]]
    
    # for each trial, randomly pick a target location from the relevant context
    target <- sample(doors[[task]], 1)
    targets <- c(targets, target)
    
    
    events <- c() #keeping track of events for the current trial trial
    repeat {
      new_event <- sample(1:20, 1)
      events <- c(events, new_event) 
        
      simulated_events <- rbind(
        simulated_events,
        data.frame(sub = sid, ses = ses, train_type = train_type, t = t, 
                   context = task, door = new_event))
        
      if (new_event == target) {
        break }
      }
  }
  
  # now, tell me which trials are switch trials using the src/get_switch.R script
  simulated_events <- get_switch(simulated_events)
  
  # now I need to get code that will tell me whether a door selection is from the 
  # current task (door_cc) or the other task (door_oc) or neither (door_nc).
  
  tmp <- c()
  for (i in 1:2){
    tmp[[i]] <- simulated_events %>% 
      filter(context == i) %>% 
      mutate(door_cc = case_when(door %in% doors[[i]] ~ 1, .default = 0),
             door_oc = case_when(door %in% doors[[(3 - i)]] ~ 1, .default = 0)) # this is hardcoded for 2 tasks
  }
  simulated_events <- rbind(tmp[[1]], tmp[[2]]) %>%
    arrange(t)
  
  ## Now, I am going to run the task jumps code ("src/get_setting_stability.R") script
  ## on this data to to get the number task jumps on this code
  
  simulated_events <- get_setting_stability(simulated_events)
  
  grp_data <- rbind(grp_data, simulated_events)
}

# by trial
res <- grp_data %>%
  group_by(sub, ses, t, context, train_type) %>%
  summarise(
    switch = max(switch), n_clicks = n(), n_cc = sum(door_cc), n_oc = sum(door_oc),
    context_changes = sum(select_cc)+sum(select_oc),
  )
#res$context_changes[intersect(which(res$switch==1),which(res$ses==2))] <- res$context_changes[intersect(which(res$switch==1),which(res$ses==2))]-1

# continue here - check this runs

jumps <- task_jumps(grp_data)
res <- inner_join(res, jumps, by = c('sub', 't'))

res %>% 
  group_by(sub, switch) %>% 
  summarise(mean_context_changes = mean(context_changes),
            mean_jumps = mean(task_jumps))
