# E.Chung 2026
# In this script, I am looking at at the event data and coding for whether each 
# general error is 1, 2 or 2+ door locations away from the nearest target location
# from the other task set.

library(tidyverse)

source(file.path("src", "get_subs.R"))
version <- "data_sandpit"
event_data <- read.csv("res/exp_lt_evt.csv")
contexts <- c(1, 2) # 1: learned task-set 1, 2: learned task-set 2

subs <- get_subs(version)


for (sub in subs) {
  print(sub)
  
  sid <- as.numeric(substring(sub,5,7))

  learn_events <- event_data %>% filter(sub == sid, ses == 1)

  for (i in contexts) {
      this_context <- learn_events %>% filter(context == i)
      correct_clicks <- which(this_context$door_cc == 1)
      
      context_doors <- sort(this_context$door[correct_clicks] %>% unique())
        
      gen_error_events <- event_data %>% filter(sub == sid, ses == 2, context == i, door_nc == 1) # only looking at training data now
      these_errors <- gen_error_events$door
    
      ndoors <- 4
      n_errors <- length(these_errors)
      error_dist <- matrix(NA, 4, n_errors)
    
      for (error in these_errors){
        if (error < 12) {
          same_layer_idx <- c(1, 2) # first two targer doors are in the same layer if door no. <12
        } else if (error >= 12) {
          same_layer_idx <- c(3, 4) # last two target doors are in the same layer if door no. >= 12
        }
          
        for (door in same_layer_idx)[
            distances <- c(error - context_doors[3 - i] # using 3 - i will index the other contex
          ]
      }
    }
  }
  
    # gen_error_events <- gen_error_events %>% 
    #   mutate(other_context_dist = case
  
  }
}
