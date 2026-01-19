# E. Chung, 2025
# This script gets the summarised data frames containing the key measures required 
# for data analysis.

library(tidyverse)
library(dplyr)

exp_lt_data <- read.csv("res/exp_lt_trl.csv")
entropy_data <- read.csv("res/exp_lt_entropy.csv")
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

gen_error_means <- training_data %>%  # checking which subs have mean gen_error>0.25
  group_by(sub) %>%                   # across all training trials to exclude from transfer
  summarise(mean_gen_error = mean(general_errors))
exclude_summary <- gen_error_means %>% 
  filter(mean_gen_error > 0.25) 
exclude_subs <- exclude_summary %>% pull(sub)

if (length(exclude_subs) > 0) { # write exluded subs to csv., if any
  write.csv(exclude_summary, "res/excluded_subs.csv", row.names = FALSE)
}

training_data <- training_data %>%  
  group_by(sub, ses, train_type, switch) %>% 
  summarise(mean_task_jumps  = mean(context_changes), # mean task jumps and
            mean_gen_error = mean(general_errors))    # general error

write.csv(training_data, "res/training_jumps_gen-error.csv", row.names = FALSE)

# getting transition entropy for training phase
entropy_data <- entropy_data %>% 
  group_by(sid, train_type) %>% 
  summarise(mean_entropy = mean(entropy)) %>% 
  rename('sub' = 'sid')
entropy_data$train_type <- as.factor(entropy_data$train_type)
levels(entropy_data$train_type) <- c("Stable", "Variable")

write.csv(entropy_data, "res/training_entropy.csv", row.names = FALSE)


# getting dataframes for transfer phase data
transfer_data <- exp_lt_data %>% 
  filter(ses == 3)
transfer_data$train_type <- as.factor(transfer_data$train_type) # renaming conditions and groups
levels(transfer_data$train_type) <- c("Stable", "Variable")
transfer_data$transfer <- as.factor(transfer_data$transfer)
levels(transfer_data$transfer) <- c("Novel", "Partial", "Complete")

transfer_data <- transfer_data %>%  # adding a learning task-sets variable
  mutate(task_set = case_when(sub <= 24 ~ 'A', # hardcoded - change if number of subs change
                              sub >= 24 & sub <= 48 ~ 'B',
                              sub >= 49 & sub <= 72 ~ 'C',
                              sub >= 73 ~ 'D'))


transfer_data <- transfer_data %>% 
  filter(!sub %in% exclude_subs) %>% # only relevant if there are any subs to exclude
  group_by(sub, ses, train_type, transfer, order_id, task_set) %>% 
  # slice_head(n = 20) %>% # looking at first 20 trials only
  summarise(mean_acc  = mean(accuracy),
            mean_set_error = mean(learned_setting_errors)) 

maggi_data <- learn_onset_data %>% filter(ses == 3)
maggi_data$train_type <- as.factor(maggi_data$train_type)
levels(maggi_data$train_type) <- c("Stable", "Variable")
maggi_data$transfer <- as.factor(maggi_data$transfer)
levels(maggi_data$transfer) <- c("Novel", "Partial", "Complete")

maggi_data <- maggi_data %>% 
  rename("sub" = "sid") %>% 
  select(-context, -train_type, -ses, - nsamples)

all_transfer_data <- inner_join(transfer_data, maggi_data, by = c("sub", "transfer")) 
non_learners <- all_transfer_data %>%  # find subs who never learn 4 doors
  filter(k4_onset == Inf)
post_exclusion_transfer <- all_transfer_data %>% filter(k4_onset != Inf) # remove rows where participant never learns 4 doors

write.csv(non_learners, "res/nonlearners.csv", row.names = FALSE)
write.csv(all_transfer_data, "res/all_transfer_data.csv", row.names = FALSE)
write.csv(post_exclusion_transfer, "res/post_exclusion_transfer_data.csv", row.names = FALSE)

# now getting filtering data for between subject analyses
# only containing data for participants' first transfer task
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


## getting dataframes to look at training performance overtime
training_epochs <- training_by_epoch %>% 
  group_by(sub, ses, train_type, epoch) %>% 
  summarise(mean_task_jumps  = mean(context_changes),
            mean_gen_error = mean(general_errors)) 

training_epochs <- training_epochs %>%
  group_by(train_type, epoch) %>% # for looking at task jumps overtime
  summarise(group_task_jumps  = mean(mean_task_jumps),
            group_gen_error = mean(mean_gen_error)) 

write.csv(training_epochs, "res/training_by_epoch.csv", row.names = FALSE)
