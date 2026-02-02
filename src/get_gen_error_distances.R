# E.Chung 2026
# In this script, I am looking at at the event data and coding for whether each 
# general error is 1, 2 or 2+ door locations away from the nearest target location
# from the other task set.

library(tidyverse)

source(file.path("src", "get_subs.R"))
exp <- 'exp_lt'
version <- "data_sandpit"
event_data <- read.csv("res/exp_lt_evt.csv")
contexts <- c(1, 2) # 1: learned task-set 1, 2: learned task-set 2

subs <- get_subs(version)

group_data <- data.frame(sub = integer(), ses = integer(), subses = integer(), t = integer(), door = integer(), 
                         door_cc = integer(), door_oc = integer(), door_nc = integer, on = integer(), off = integer(), 
                         switch = integer(), train_type = integer(), transfer = integer(), order = integer(),
                         original_house = integer(), scca = integer(), sccb = integer(), select_cc = integer(),
                         soca = integer(), socb = integer(), select_oc = integer(), select_oc_late = integer(), order_id = integer(),
                         gen_error_dist = integer())

# setting some parameters. these will need to change if the set up of the display changes
n_innerdoors <- 8 
n_outerdoors <- 12
n_targets_per_layer <- 2


for (sub in subs) {
  print(sub)
  sid <- as.numeric(substring(sub,5,7))

  # get doors for each task-set (context) from learn phase
  learn_events <- event_data %>% filter(sub == sid, ses == 1)
  
  tmp <- list() # the door numbers will go into this list
  for (i in contexts) {
    this_context <- learn_events %>% filter(context == i) # get all events where target door is clicked
    correct_clicks <- which(this_context$door_cc == 1) # get idxs for those events
    context_doors <- sort(unique(this_context$door[correct_clicks])) # get the target door numbers
    
    tmp[[as.character(i)]] <- context_doors 
  }
  context1_doors <- tmp[[1]]
  context2_doors <- tmp[[2]]
  
  # now, determine the distance of general errors from the nearest target location in the same
  # layer (inner or outer) from the other task set during training
  
  gen_error_events <- event_data %>% filter(sub == sid, ses == 2,  door_nc == 1) 
  these_errors <- gen_error_events$door
  
  if(length(these_errors) == 0){
    print('no general errors')
    
  } else {
    # these matrices will record the distance of errors from target doors in the same layer 
    # accounting for distances when counting clockwise and anticlockwise
    n_errors <- length(these_errors)
    clockwise_mat <- matrix(NA, 4, n_errors) # distance clockwise 
    anticlockwise_mat <- matrix(NA, 4, n_errors) # distance anticlockwise
    
    # now, loop through each error and determine distance from each target loc in the same layer
    for (error in 1:n_errors) {
      if (gen_error_events$context[error] == 1) {
        other_context <- context2_doors
      } else if (gen_error_events$context[error] == 2) {
        other_context <- context1_doors
      }
  
      if (these_errors[error] %in% 1:12){ # door numbers from 1-12 are in the outer layer
        layer <- 'out'
        layer_idx <- c(1, 2) 
      } else if (these_errors[error] %in% 13:20){ # door numbers from 13-20 are in the inner layer
        layer <- 'in'
        layer_idx <- c(3, 4)
      }
      
      for (idx in 1:n_targets_per_layer) {
        clockwise_dist <- abs(other_context[layer_idx[idx]] - these_errors[error])
        clockwise_mat[layer_idx[idx], error] <- clockwise_dist  
        
        if (layer == 'out') {
          anticlockwise_mat[layer_idx[idx], error] <- abs(n_outerdoors - clockwise_dist)
          } else if (layer == 'in') {
            anticlockwise_mat[layer_idx[idx], error] <- abs(n_innerdoors - clockwise_dist)
          }
      }
      
    }
    
    # now, concatenate matrices and find get the minimum value to find distance from 
    # nearest target door
    
    full_distance_mat <- rbind(clockwise_mat, anticlockwise_mat)
    gen_error_dist <- apply(full_distance_mat, 2, min, na.rm = TRUE)
    
    gen_error_events <- cbind(gen_error_events, gen_error_dist)
    
    # add subject data to group data frame
    group_data <- rbind(group_data, gen_error_events)
  }  
}  
  
write.csv(group_data, "res/exp_lt_gen-error_events.csv", row.names = FALSE)


