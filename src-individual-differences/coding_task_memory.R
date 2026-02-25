# E. Chung, 2026
# In this script, I will be getting the scores used to look at participants' memory
# of the target locations for the two learned task for the learning (task acuiqision)
# and training phase.

library(tidyverse)
source(file.path("src", "get_subs.R"))

version <- 'individual_differences'
event_data <- read.csv("res/exp_lt_evt.csv")
learn_contexts <- c(1, 2)

data_path <- file.path("/Users/emilychung/Library/CloudStorage/OneDrive-SharedLibraries-UNSW/Learning and Attention Group - Emily PhD project - Emily PhD project/exp1_data/individual_differences")
memory_conditions <- read.csv(file.path(data_path, "sub_memory_conditions.csv"))
door_selections <- read.csv(file.path(data_path, "individual_door_selections.csv"))

subs <- get_subs(version)

group_data <- data.frame(sub = integer(), train_type = integer(), task_config = character(),
                         complete_task = integer(), incorrect_order = integer(), n_task1 = integer(), 
                         n_task2 = integer(), n_setErr1 = integer(), n_setErr2 = integer(), 
                         n_genErr1 = integer(), n_genErr2 = integer(), n_comp_and_part = integer(), 
                         n_complete_only = integer(), n_partial_only = integer(), 
                         n_never_transferred = integer()
)

for (sub in subs) {
  print(sub)
  sid <- as.numeric(substring(sub,5,7))
  
  train_type <- event_data %>%
    filter(sub == sid, ses == 3) %>%
    pull(train_type) %>% 
    unique()
  task_config <- memory_conditions %>% filter(sub == sid) %>% pull(task_config)
  wrong_order <- memory_conditions %>% filter(sub == sid) %>% pull(wrong_order)
  complete_task <- memory_conditions %>% filter(sub == sid) %>% pull(complete_task)
  
  # get doors for each task-set (context) from learn phase
  if (sub == 'sub-35') { # using train phase data because missing learn phase data for sub-35
    learn_events <- event_data %>% filter(sub == sid, ses == 2)
  } else {
    learn_events <- event_data %>% filter(sub == sid, ses == 1)
  }
  
  tmp <- list() # the door numbers will go into this list
  for (i in learn_contexts) {
    this_context <- learn_events %>% filter(context == i) # get all events where target door is clicked
    correct_clicks <- which(this_context$door_cc == 1) # get idxs for those events
    context_doors <- sort(unique(this_context$door[correct_clicks])) # get the target door numbers
    
    tmp[[as.character(i)]] <- context_doors 
  }
  
  if(sub == 'sub-35'){
    correct_context1_doors <- tmp[[2]] # door numbers and context numbers are swapped for training phase data for sub-35?
    correct_context2_doors <- tmp[[1]]
  } else{
    correct_context1_doors <- tmp[[1]]
    correct_context2_doors <- tmp[[2]]
  }
  
  partial_trls <- event_data %>% filter(sub == sid, transfer == 2)
  correct_clicks <- which(partial_trls$door_cc == 1) 
  partial_doors <- sort(unique(partial_trls$door[correct_clicks]))
  
  doors_selected <- door_selections %>% filter(sub == sid)
  
  if (wrong_order == FALSE){
    task1_selected <- doors_selected[[2]] %>% na.omit()
    task2_selected <- doors_selected[[3]] %>% na.omit()
  } else if (wrong_order == TRUE){
    task1_selected <- doors_selected[[3]] %>% na.omit()
    task2_selected <- doors_selected[[2]] %>% na.omit()
  }
  

  n_task1 <- length(intersect(task1_selected, correct_context1_doors))
  n_task2 <- length(intersect(task2_selected, correct_context2_doors))
  
  n_setErr1 <- length(intersect(task1_selected, correct_context2_doors))
  n_setErr2 <- length(intersect(task2_selected, correct_context1_doors))
  
  n_genErr1 <- length(which(!task1_selected %in% c(correct_context1_doors, correct_context2_doors)))
  n_genErr2 <- length(which(!task2_selected %in% c(correct_context1_doors, correct_context2_doors)))
  
  if (complete_task == 1) { # if task 1 was transferred to complete condition
    completeTask_doors <- correct_context1_doors
    otherTask_doors <- correct_context2_doors
    these_complete_selected <- task1_selected
    these_other_selected <- task2_selected
  } else if (complete_task == 2) {
    completeTask_doors <- correct_context2_doors
    other_Task_doors <- correct_context1_doors
    these_complete_selected<- task2_selected
    these_other_selected <- task1_selected
  }
  
  # these are the door numberes relevant to each condition  
  completeTask_partialIn <- intersect(completeTask_doors, partial_doors)
  completeTask_partialOut <- completeTask_doors[!(completeTask_doors %in% completeTask_partialIn)]
  otherTask_partialIn <- intersect(otherTask_doors, partial_doors)
  otherTask_partialOut <- otherTask_doors[!(otherTask_doors %in% otherTask_partialIn)]
  
  # getting the number of correct doors selections in each condition
  n_comp_and_part <- length(intersect(these_complete_selected, completeTask_partialIn))
  n_complete_only <- length(intersect(these_complete_selected, completeTask_partialOut))
  n_partial_only <- length(intersect(these_other_selected, otherTask_partialIn))
  n_never_transferred <- length(intersect(these_other_selected, otherTask_partialOut))
  
  
  sub_data <- data.frame(sub = sid, train_type = train_type, task_config = task_config,
                         complete_task = complete_task, incorrect_order = wrong_order, 
                         n_task1 = n_task1, n_task2 = n_task2, n_setErr1 = n_setErr1, 
                         n_setErr2 = n_setErr2, n_genErr1 = n_genErr1, n_genErr2 = n_genErr2, 
                         n_comp_and_part = n_comp_and_part, n_complete_only = n_complete_only,
                         n_partial_only = n_partial_only, n_never_transferred = n_never_transferred
                         )
  
  group_data <- rbind(group_data, sub_data) 
  
}

write.csv(group_data, "res/task_memory_data.csv", row.names = FALSE)
