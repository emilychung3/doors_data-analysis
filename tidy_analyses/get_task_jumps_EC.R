# E.Chung, 2026
# This script is used to get task jumps scores.

get_task_jumps <- function(dat) {
  
  # getting all the trials where at least gen error was made and removing from 
  # the dataset
  nc_count <- dat %>% 
    group_by(sub, ses, t, context) %>% 
    summarise(door_nc = sum(door_nc))
  nc_trls <- nc_count %>% 
    filter(door_nc > 0)
  
  dat <- anti_join(dat, nc_trls, by = c("sub", "t"))
  
  # now, create a variable that will tell us if theyre first click on each trial
  # was a task jump or not.
  
  tmp <- dat %>% 
    group_by(sub, t) %>% 
    mutate(first_click_jump = case_when(row_number() == 1 & switch == 1 & door_cc == 1 ~ 1,
                                        row_number() == 1 & switch == 0 & door_oc == 1 ~ 1,
                                        TRUE ~ 0)) %>% 
    ungroup()
  first_click_jump_trls <- tmp %>% 
    group_by(sub, t) %>% 
    summarise(first_click_jump = sum(first_click_jump))
  

  # then count the number of jumps from the remaining clicks
  # and add 1 if their first click was a jump from the other context i.e., when
  # switch = 1 and door_cc == 1, and when switch = 0 and door_oc = 1 on the first
  # event of each trial.
  
  tmp <- tmp %>% 
    group_by(sub, train_type, switch, context, t) %>%  
    summarise(njumps = sum(abs(diff(door_cc)))) %>% 
    arrange(sub, t)
  jumps <- inner_join(tmp, first_click_jump_trls, by = c('sub', 't'))
  
  jumps <- jumps %>% 
    mutate(njumps = if_else(first_click_jump == 1, njumps+1, njumps))

  # now subtract 1 from all switch trials to account for the fact that a switch
  # trial requires a minimum of one jump.
  
  jumps <- jumps %>% 
    mutate(njumps = if_else(switch == 1, njumps - 1, njumps)
    )

  return(jumps)
}
