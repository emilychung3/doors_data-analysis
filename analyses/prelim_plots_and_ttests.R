# E. Chung, 2025
# This script contains code for the and statistical analyses of the first XX 
# data sets using Bayesian t-tests. Data is also plotted in violin and boxplots

library(tidyverse)
library(dplyr)
library(BayesFactor)
library(ggplot2)
library(wesanderson)
library(gghalves)
library(showtext)

source(file.path(getwd(), "analyses", "training_plots.R"))
source(file.path(getwd(), "analyses", "transfer_plots.R"))
source(file.path(getwd(), "analyses", "scatterplots.R"))

exp_lt_data <- read.csv("res/exp_lt_trl.csv")
entropy_data <- read.csv("res/exp_lt_entropy.csv")
maggi_data <- read.csv("res/exp_lt_maggi-k4.csv")

# training analyses ============================================================
# task-jumps and general errors ------------------------------------------------
training_data <- exp_lt_data %>% 
  filter(ses == 2) 
training_data$train_type <- as.factor(training_data$train_type)
levels(training_data$train_type) <- c("stable", "variable")
training_data$switch <- as.factor(training_data$switch)
levels(training_data$switch) <- c("Stay", "Switch")

training_data <- training_data %>% 
  group_by(sub, ses, train_type, switch) %>% 
  summarise(mean_task_jumps  = mean(context_changes),
            mean_gen_error = mean(general_errors)) 

## plot task-jumps data
iv <- training_data$switch
x_axis_lab <- 'Trial type'
dv <- training_data$mean_task_jumps
y_axis_lab <- 'Task-jumps'
these_cols <- wes_palette("AsteroidCity3")[c(1, 4)]

task_jumps_plt <- training_plots(training_data)
task_jumps_plt

## plot general-error data
iv <- training_data$switch
x_axis_lab <- 'Trial type'
dv <- training_data$mean_gen_error
y_axis_lab <- 'General Errors'
these_cols <- wes_palette("AsteroidCity3")[c(1, 4)]

general_errors_plt <- training_plots(training_data)
general_errors_plt

# entropy ---------------------------------------------------------------------
entropy_data <- entropy_data %>% 
  group_by(sid, train_type) %>% 
  summarise(mean_entropy = mean(entropy)) # get mean entropy for each participant
entropy_data$train_type <- as.factor(entropy_data$train_type)
levels(entropy_data$train_type) <- c("Stable", "Variable")

# plot entropy data
iv <- entropy_data$train_type
x_axis_lab <- 'Group'
dv <- entropy_data$mean_entropy
y_axis_lab <- 'Transition Entropy'
these_cols <- wes_palette("AsteroidCity3")[c(1, 4)]

entropy_plt <- training_plots(entropy_data) 
entropy_plt

# t-tests ---------------------------------------------------------------------
## stable vs. variable group on task-jumps for switch and stay trials separately
switch_trials <- training_data %>% 
  filter(switch == 'Switch')
ttestBF(formula = mean_task_jumps ~ train_type, data = switch_trials)

stay_trials <- training_data %>% 
  filter(switch == 'Stay')
ttestBF(formula = mean_task_jumps ~ train_type, data = stay_trials)

## stable vs. variable group on general-errors for switch and stay trials separately
ttestBF(formula = mean_gen_error ~ train_type, data = switch_trials)

ttestBF(formula = mean_gen_error ~ train_type, data = stay_trials)

## stable vs. variable group on entropy for stay trials only
ttestBF(formula = mean_entropy ~ train_type, data = entropy_data)



# transfer analyses ============================================================
# accuracy --------------------------------------------------------------------
transfer_data <- exp_lt_data %>% 
  filter(ses == 3)
transfer_data$train_type <- as.factor(transfer_data$train_type) # renaming conditions and groups
levels(transfer_data$train_type) <- c("Stable", "Variable")
transfer_data$transfer <- as.factor(transfer_data$transfer)
levels(transfer_data$transfer) <- c("Novel", "Partial", "Complete")

transfer_data <- transfer_data %>% 
  group_by(sub, ses, train_type, transfer, order_id) %>% 
  slice_head(n = 20) %>%
  summarise(mean_acc  = mean(accuracy),
            mean_set_error = mean(learned_setting_errors)) 

## plot accuracy data
iv <- transfer_data$transfer
x_axis_lab <- 'Transfer type'
dv <- transfer_data$mean_acc
y_axis_lab <- 'Accuracy'
these_cols <- c("#cb6ce6", "#ff66c4", "#0097b2")



accuracy_plt <- transfer_plots(transfer_data)
accuracy_plt

# k4 data ----------------------------------------------------------------------
maggi_data <- maggi_data %>% filter(ses == 3, k4_onset != Inf)
maggi_data$train_type <- as.factor(maggi_data$train_type)
levels(maggi_data$train_type) <- c("Stable", "Variable")
maggi_data$transfer <- as.factor(maggi_data$transfer)
levels(maggi_data$transfer) <- c("Novel", "Partial", "Complete")

## plot k4 onset
iv <- maggi_data$transfer
x_axis_lab <- 'Transfer type'
dv <- maggi_data$k4_onset
y_axis_lab <- 'Learning Onset'
these_cols <- c("#cb6ce6", "#ff66c4", "#0097b2")

k4_plt <- transfer_plots(maggi_data)
k4_plt

# t-tests ---------------------------------------------------------------------
## accuracy and k4 onset: stable vs. variable on *partial* trf
partial_trf <- transfer_data %>% filter(transfer == 'Partial')
partial_maggi <- maggi_data %>% filter(transfer == 'Partial')
ttestBF(formula = mean_acc ~ train_type, data = partial_trf)
ttestBF(formula = k4_onset ~ train_type, data = partial_maggi)


## accuracy and k4 onset: stable vs. variable on *novel* trf
novel_trf <- transfer_data %>% filter(transfer == 'Novel')
novel_maggi <- maggi_data %>% filter (transfer == 'Novel')
ttestBF(formula = mean_acc ~ train_type, data = novel_trf)
ttestBF(formula = k4_onset ~ train_type, data = novel_maggi)

## accuracy and k4 onset: stable vs. variable on *complete* trf
complete_trf <- transfer_data %>% filter(transfer == 'Complete')
complete_maggi <- maggi_data %>% filter(transfer == 'Complete')
ttestBF(formula = mean_acc ~ train_type, data = complete_trf)
ttestBF(formula = k4_onset ~ train_type, data = complete_maggi)


## accuracy and k4 onset: within groups comparisons
transfer_acc_wide <- transfer_data %>% 
  select(sub, ses, train_type, transfer, order_id, mean_acc) %>% 
  pivot_wider(names_from = transfer, values_from = mean_acc)
ttestBF(x = transfer_acc_wide$Novel,  # accuracy: novel vs. partial 
        y = transfer_acc_wide$Partial,
        paired = TRUE)
ttestBF(x = transfer_acc_wide$Complete, # accuracy: complete vs. partial
        y = transfer_acc_wide$Partial,
        paired = TRUE)
ttestBF(x = transfer_acc_wide$Complete, # accuracy: complete vs. partial
        y = transfer_acc_wide$Novel,
        paired = TRUE)


maggi_wide <- maggi_data %>% 
  select(sid, ses, train_type, transfer, k4_onset) %>% 
  pivot_wider(names_from = transfer, values_from = k4_onset) %>% # getting NA for some rows?
  na.omit()
ttestBF(x = maggi_wide$Novel,  # accuracy: novel vs. partial 
        y = maggi_wide$Partial,
        paired = TRUE)
ttestBF(x = maggi_wide$Complete, # accuracy: complete vs. partial
        y = maggi_wide$Partial,
        paired = TRUE)
ttestBF(x = maggi_wide$Complete, # accuracy: complete vs. partial
        y = maggi_wide$Novel,
        paired = TRUE)

  
## stable group
stable_transfer_acc <- transfer_acc_wide %>% 
  filter(train_type == 'Stable')
stable_maggi <- maggi_wide %>% 
  filter(train_type == 'Stable') 

## accuracy 
ttestBF(x = stable_transfer_acc$Novel,  # accuracy: novel vs. partial 
        y = stable_transfer_acc$Partial,
        paired = TRUE)
ttestBF(x = stable_transfer_acc$Complete, # accuracy: complete vs. partial
        y = stable_transfer_acc$Partial,
        paired = TRUE)
ttestBF(x = stable_transfer_acc$Complete, # accuracy: complete vs. novel
        y = stable_transfer_acc$Novel,
        paired = TRUE)

## learn onset
ttestBF(x = stable_maggi$novel, # k4_onset: novel vs. partial
        y = stable_maggi$partial,
        paired = TRUE)
ttestBF(x = stable_maggi$complete, # accuracy: complete vs. partial
        y = stable_maggi$partial,
        paired = TRUE)
ttestBF(x = stable_maggi$complete, # accuracy: complete vs. novel
        y = stable_maggi$novel,
        paired = TRUE)

## variable group
variable_transfer_acc <- transfer_acc_wide %>% 
  filter(train_type == 'Variable')
variable_maggi <- maggi_wide %>% 
  filter(train_type == 'Variable')

## accuracy
ttestBF(x = variable_transfer_acc$complete, # accuracy: complete vs. partial
        y = variable_transfer_acc$partial,
        paired = TRUE)
ttestBF(x = variable_transfer_acc$complete, # accuracy: complete vs. novel
        y = variable_transfer_acc$novel,
        paired = TRUE)
ttestBF(x = variable_transfer_acc$novel, # accuracy: novel vs. partial
        y = variable_transfer_acc$partial,
        paired = TRUE)

## k4 onset
ttestBF(x = variable_maggi$novel, # k4_onset: novel vs. partial
        y = variable_maggi$partial,
        paired = TRUE)
ttestBF(x = variable_maggi$novel, # k4_onset: novel vs. complete
        y = variable_maggi$complete,
        paired = TRUE)
ttestBF(x = variable_maggi$partial, # k4_onset: partial vs. complete
        y = variable_maggi$complete,
        paired = TRUE)



# correlating entropy with a bias score for performance on complete vs. partial 
# transfer
transfer_acc_wide <- transfer_acc_wide %>% 
  mutate(bias = (complete - partial)/(complete+partial))
entropy_data_to_join <- entropy_data %>% 
  select(sid, mean_entropy) %>% 
  rename('sub' = 'sid')

transfer_acc_wide <- inner_join(transfer_acc_wide, entropy_data_to_join, 
                                by = 'sub')

iv <- transfer_acc_wide$mean_entropy
x_axis_lab <- "Transition Entropy"
dv <- transfer_acc_wide$bias
y_axis_lab <- "Bias Scores"

acc_entropy_plt <- scatterplot(transfer_acc_wide)
acc_entropy_plt
cor(transfer_acc_wide$bias, transfer_acc_wide$mean_entropy)

# 
transfer_by_taskset <- transfer_data %>% 
  mutate(task_set = case_when(sub <= 24 ~ 'A',
                              sub >= 24 & sub <= 48 ~ 'B',
                              sub >= 49 & sub <= 72 ~ 'C',
                              sub >= 73 ~ 'D'))
iv <- transfer_by_taskset$transfer
x_axis_lab <- 'Transfer type'
dv <- transfer_by_taskset$mean_acc
y_axis_lab <- 'Accuracy'
these_cols <- c("#cb6ce6", "#ff66c4", "#0097b2")

transfer_plots(transfer_by_taskset)




