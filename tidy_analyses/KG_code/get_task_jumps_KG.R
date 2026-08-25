# K. Garner, 2026: Here we take the grp level data, and for the sessions of our
# choice, we count the number of task jumps on a given trial. This is the number
# of times that somebody moved between contexts on a given trial, relative to the
# context in which they just found a target.

# a guide to task jumps
# Task jumps were defined as the number of times participants moved between
# Task A and Task B locations over the course of finding a single target
# (completing a single trial).
# The first move on any trial was assessed relative to the location of
# the target on the previous trial. For example, if the participant previously
# found a target at location $A_4$, and (unknown to them) the next target was
# at location $A_3$ (making this a Stay trial since the current trial and the
# previous trial were both from Task A), the selections
# $A_1$, $A_2$, $B_1$, $B_2$, $A_3$ would incur a task jump score of two.
# If the target on the previous trial was at location $B_1$, the same selections
# on the current trial would incur a task jump score of three.

get_task_jumps <- function(dat){

  # get the value of the context on the previous trial, to help us with coding task
  # jumps on switch trials
  trial_context <- dat %>%
    distinct(sub, ses, #block, 
             t, context) %>%   # one row per trial
    arrange(sub, ses, #block, 
            t) %>%
    group_by(sub, ses) %>% # block,
    mutate(context_prev = lag(context)) %>%
    ungroup()

  # now join that info to the regular data frame
  dat <- dat %>%
    left_join(
      trial_context %>% select(sub, ses, t, context_prev), #block
      by = c("sub", "ses","t") #block
    )

  # The below needs recoding so that we count each time there was
  # a jump from A to B, and we don't count jumps to and from nc locations.
  # now get trial numbers for each participant where there was a
  # 'nc' click, as this limits our ability to draw conclusions about jumps between
  # Task A and Task B
  nc_trials <- dat %>% arrange(sub, ses, t) %>% #block
    group_by(sub, ses, t) %>%  #block
    summarise(nc = sum(door_nc == 1) > 0, .groups='drop')

  # now, join the two datasets back together, and drop trials with nc clicks
  tmp <- dat %>% left_join(nc_trials, by=c('sub', 'ses', 't')) %>% #block
    filter(nc == FALSE) %>%
    select(-nc)

  # now, lets count the number of task jumps per trial.

  n_jumps <- tmp %>%
    arrange(sub, ses, t) %>% #block
    group_by(sub, ses, t) %>% #block
    summarise(
      task_jumps = sum(door_cc != lag(door_cc), na.rm = TRUE),
      first_sel_cc = first(door_cc), # this will be useful for coding whether or not
      # we should add an extra '1' based on where a target was last found
      switch = first(switch), # this will also be helpful
      context = first(context), # as will this - see below
      context_prev = first(context_prev), # and probably this but its also for sanity checks
      .groups = "drop"
    ) %>%
    mutate(task_jumps = ifelse(first_sel_cc == 1 & coalesce(switch) & !is.na(context_prev) &
                                context_prev != context, task_jumps + 1, task_jumps)) %>%
    # this adds an extra jump if it is a switch trial, if the first click comes from the context that has
    # just been switched to (i.e. not where a target was last found), but ignores times
    # when context_prev = NA, as these are the first trials of a block, where we don't
    # have a previous target to compare to, and so we can't be sure if the first click is a jump or not.
   mutate(task_jumps = ifelse(coalesce(switch) & task_jumps > 0, task_jumps - 1, task_jumps )) # and finally,
   # subtract one from task jumps on switch trials, to account
   # for the fact that one task jump is necessary on switch trials.

  # and now pop this info back into the main data frame
  dat <- dat %>% left_join(n_jumps %>% select(sub, ses, t, task_jumps), by=c('sub', 'ses', 't')) %>%
    mutate(task_jumps = ifelse(is.na(task_jumps), 0, task_jumps)) # replace NA with 0
  # return dat

  return(dat)
}
#







