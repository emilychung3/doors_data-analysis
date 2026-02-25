# E. Chung, 2025
# This script gets the summarised data frames containing the key measures required 
# for data analysis.

library(tidyverse)
library(dplyr)

exp_lt_data <- read.csv("res/exp_lt_trl.csv")
entropy_data <- read.csv("res/exp_lt_entropy.csv")
gen_error_events <- read.csv("res/exp_lt_gen-error_events.csv")
learn_onset_data <- read.csv("res/exp_lt_maggi-k4.csv")

# getting data frames for training phase for each participant
training_data <- exp_lt_data %>% 
  filter(ses == 2) 
training_data$train_type <- as.factor(training_data$train_type)
levels(training_data$train_type) <- c("Stable", "Variable")
training_data$switch <- as.factor(training_data$switch)
levels(training_data$switch) <- c("Stay", "Switch")

training_by_epoch <- training_data %>%  # for later when looking at training data overtime
  mutate(epoch = case_when(t < 40 ~ 1, 
                           t >= 40 & t < 80 ~ 2, # hardcoded - will need to change if trial numbers change
                           t >= 80 & t < 120 ~ 3,
                           t >= 120 & t < 160 ~ 4,
                           t >= 160 & t < 200 ~ 5,
                           t >= 200 & t < 240 ~ 6,
                           t >= 240 & t < 280 ~ 7,
                           t >= 280 ~ 8))

gen_error_stay <- training_data %>%  # checking which subs have mean gen_error>0.25 on stay trials only
  filter(switch == 'Stay') %>%        # to later exclude from transfer
  group_by(sub) %>%                   
  summarise(mean_gen_error = mean(general_errors))
exclude_summary <- gen_error_stay %>% 
  filter(mean_gen_error > 0.25) 
exclude_subs <- exclude_summary %>% pull(sub)

if (length(exclude_subs) > 0) { # write excluded subs to csv., if any
  write.csv(exclude_summary, "res/excluded_subs.csv", row.names = FALSE)
}

# getting a data frame with the number of average number of clicks per trial across all trials
nclicks <- training_data %>% 
  group_by(sub, train_type) %>% 
  summarise(nclicks = mean(n_clicks))

# getting data frame for task jumps, gen error and nclicks across switch and stay trials
training_data <- training_data %>%  
  group_by(sub, ses, train_type, switch) %>% 
  summarise(mean_task_jumps  = mean(context_changes), # mean task jumps and
            mean_gen_error = mean(general_errors),    # general error
            mean_nclicks = mean(n_clicks))            # n clicks


write.csv(training_data, "res/training_jumps_gen-error.csv", row.names = FALSE)
write.csv(nclicks, "res/training_nclicks.csv", row.names = FALSE)


# getting transition entropy for training phase
entropy_data <- entropy_data %>% 
  group_by(sid, train_type) %>% 
  summarise(mean_entropy = mean(entropy)) %>% 
  rename('sub' = 'sid')
entropy_data$train_type <- as.factor(entropy_data$train_type)
levels(entropy_data$train_type) <- c("Stable", "Variable")

write.csv(entropy_data, "res/training_entropy.csv", row.names = FALSE)


# getting data frames for transfer phase data
transfer_data <- exp_lt_data %>% 
  filter(ses == 3)
transfer_data$train_type <- as.factor(transfer_data$train_type) # renaming conditions and groups
levels(transfer_data$train_type) <- c("Stable", "Variable")
transfer_data$transfer <- as.factor(transfer_data$transfer)
levels(transfer_data$transfer) <- c("Novel", "Partial", "Complete")

transfer_data <- transfer_data %>%  # adding a learning task-sets variable
  mutate(task_set = factor(
    case_when(sub <= 24 ~ 'A', # hardcoded - change if number of subs change
              sub >= 24 & sub <= 48 ~ 'B',
              sub >= 49 & sub <= 72 ~ 'C',
              sub >= 73 ~ 'D'), 
    levels = c('A', 'B', 'C', 'D')))

transfer_data <- transfer_data %>% 
  mutate(first_transfer = case_when(order_id == 1 | order_id == 2 ~ "novel first",
                                    order_id == 3 | order_id == 4 ~ "complete first",
                                    order_id == 5 | order_id == 6 ~ "partial first"))

transfer_first_half <- transfer_data %>% 
  filter(!sub %in% exclude_subs) %>% # only relevant if there are any subs to exclude
  group_by(sub, ses, train_type, transfer, order_id, task_set, first_transfer) %>% 
  slice_head(n = 20) %>% # looking at first 20 trials only
  summarise(mean_acc  = mean(accuracy),
            mean_set_error = mean(learned_setting_errors)) 

transfer_data <- transfer_data %>% 
  filter(!sub %in% exclude_subs) %>%
  group_by(sub, ses, train_type, transfer, order_id, task_set, first_transfer) %>% 
  summarise(mean_acc  = mean(accuracy), # this is averaging across all trials
            mean_set_error = mean(learned_setting_errors)) 

maggi_data <- learn_onset_data %>% filter(ses == 3)
maggi_data$train_type <- as.factor(maggi_data$train_type)
levels(maggi_data$train_type) <- c("Stable", "Variable")
maggi_data$transfer <- as.factor(maggi_data$transfer)
levels(maggi_data$transfer) <- c("Novel", "Partial", "Complete")

maggi_data <- maggi_data %>% 
  rename("sub" = "sid") %>% 
  select(-context, -train_type, -ses, -nsamples)

all_transfer_data <- inner_join(transfer_data, maggi_data, by = c("sub", "transfer")) 
non_learners <- all_transfer_data %>%  # find subs who never learn 4 doors
  filter(k4_onset == Inf)
post_exclusion_transfer <- all_transfer_data %>% filter(k4_onset != Inf) # remove rows where participant never learns 4 doors

write.csv(non_learners, "res/nonlearners.csv", row.names = FALSE)
write.csv(all_transfer_data, "res/all_transfer_data.csv", row.names = FALSE)
write.csv(transfer_first_half, "res/transfer_first_half.csv", row.names = FALSE)
write.csv(post_exclusion_transfer, "res/post_exclusion_transfer_data.csv", row.names = FALSE)

# now getting filtering data for between-subject analyses
# to only contain data for participants' first transfer task
all_btwn_subs_data <- all_transfer_data %>%
  filter(
    (order_id %in% c(1, 2) & transfer == "Novel") |
    (order_id %in% c(3, 4) & transfer == "Complete") |
    (order_id %in% c(5, 6) & transfer == "Partial")
  )

btwn_non_learners <- all_btwn_subs_data %>%  # get nonlearners for between subjects data
  filter(k4_onset == Inf)
btwn_post_exclusion_data <- all_btwn_subs_data %>% 
  filter (k4_onset != Inf)

write.csv(btwn_non_learners, "res/btwn_nonlearners.csv", row.names = FALSE)
write.csv(all_btwn_subs_data, "res/all_btwn_transfer_data.csv", row.names = FALSE)
write.csv(btwn_post_exclusion_data, "res/post_exclusion_btwn_data.csv", row.names = FALSE)

## getting data frames to look at training performance overtime
training_epochs <- training_by_epoch %>% 
  group_by(sub, ses, train_type, switch, epoch) %>% 
  summarise(mean_task_jumps  = mean(context_changes),
            mean_gen_error = mean(general_errors)) 

training_epochs_btwn_groups <- training_epochs %>%
  group_by(train_type, switch, epoch) %>% # for looking at task jumps overtime
  summarise(group_task_jumps  = mean(mean_task_jumps),
            group_gen_error = mean(mean_gen_error)) 

write.csv(training_epochs, "res/training_epochs.csv", row.names = FALSE)
write.csv(training_epochs_btwn_groups, "res/training_epochs_btwn_groups.csv", row.names = FALSE)

## performing a median split of participants on general error to look
## whether general error is stable over time

### 1. get average gen_error across all trials
average_gen_error <- training_data %>% 
  group_by(sub, train_type) %>% 
  summarise(mean = mean(general_errors))

gen_error_median <- median(average_gen_error$mean)

gen_error_groups <- average_gen_error %>% 
  mutate(error_group = case_when(mean > gen_error_median ~ 'high error',
                                 mean < gen_error_median ~ 'low error'))

### 2. now, inner join error training_epochs with group membership for each participant

training_epochs <- inner_join(training_epochs, gen_error_groups, by = c('sub', 'train_type'))

med_split_gen_error <- training_epochs %>% 
  group_by(train_type, error_group, epoch) %>%
  summarise(task_jumps = mean(mean_task_jumps),
            gen_error = mean(mean_gen_error))

write.csv(med_split_gen_error, "res/med_split_gen_error.csv", row.names = FALSE)


## getting bias scores partial/(partial+complete) and partial/(partial+novel)
all_transfer_wide <- transfer_data %>% 
  select(-mean_set_error) %>% 
  pivot_wider(names_from = transfer, values_from = mean_acc)
transfer_bias_scores <- all_transfer_wide %>% 
  mutate('bias_ovr_complete' = Partial/(Partial+Complete),
         'bias_ovr_novel' = Partial/(Partial+Novel)) %>% 
  select(-Novel, -Complete, -Partial)

write.csv(transfer_bias_scores, "res/transfer_bias_scores.csv", row.names = FALSE)


## getting distance of gen-errors from the nearest target location of the other
## task-set during training

gen_error_events$train_type <- as.factor(gen_error_events$train_type)
levels(gen_error_events$train_type) <- c("Stable", "Variable")
gen_error_events$switch <- as.factor(gen_error_events$switch)
levels(gen_error_events$switch) <- c("Stay", "Switch")

gen_error_events <- gen_error_events %>% 
  mutate(error_dist_ndoors = case_when(gen_error_dist == 1 ~ as.factor(1),
                                       gen_error_dist == 2 ~ as.factor(2),
                                       gen_error_dist > 2 ~ '2+')) %>% 
  select(sub, ses, t, context, door, door_nc, switch, train_type, gen_error_dist, error_dist_ndoors)

gen_error_summary <- gen_error_events %>% 
  group_by(sub, train_type, switch) %>% 
  summarise(one_door = mean(error_dist_ndoors == 1),
            two_door = mean(error_dist_ndoors == 2),
            two_plus_doors = mean(error_dist_ndoors == '2+'))

write.csv(gen_error_summary, "res/gen_error_summary.csv", row.names = FALSE)

# get average train times for each group
train_time <- read.csv("res/train_time.csv")

avg_train_time <- train_time %>% 
  group_by(train_type) %>% 
  summarise(train_time_s = mean(train_time)) %>% # in seconds
  mutate(train_time_min = train_time_s/60) # in minutes

stable_train_time <- train_time %>% 
  filter(train_type == 'Stable')
max_time <- max(stable_train_time$train_time)/60 # in minutes