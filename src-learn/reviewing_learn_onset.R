# E. Chung, 2026
# This script is used to examine accuracy and beta maps for participants
# performing at around 70% accuracy, on a trial by trial basis.
# This is so that we can determine whether our learn onset measure is appropriately
# capturing when participants are learning target locations during transfer.

transfer_data <- read.csv("res/all_transfer_data.csv")
trl_acc <- read.csv("res/exp_lt_trl.csv")
beta_maps_strat <- read.csv("res/exp_lt_maggi-map.csv")
beta_maps_door <- read.csv("res/exp_lt_TMP_maggi-map.csv") # ignore win variable for this dataframe


# finding participants performing <75% accuracy and those who score Inf of k4_onset
poor_performers <- transfer_data %>% 
  filter(mean_acc < 0.80 | k4_onset == Inf | k4_onset > 35)
view(poor_performers)

# interesting participants
# sub-06 partial transfer (transfer= 2): high (ish) accuracy but only learned 2 doors
## looking at their accuracy data trial and the number of doors learned on a 
## trial by trial basis
sub_trl_acc <- trl_acc %>% 
  filter(sub == 6, ses == 3, transfer == 2)

par(mfrow = c(1, 2))

plot(
  sub_trl_acc$t, sub_trl_acc$accuracy,
  type = 'l',
  xlab = "trial",
  ylab = "accuracy",
  pch = 19,        # point shape
  col = "blue"
)

beta_maps_by_strat <- beta_maps_strat %>% 
  filter(sid == 6, ses == 3, transfer == 2)

plot(
  beta_maps_by_strat$event, beta_maps_by_strat$win,
  type = 'l',
  xlab = "trial",
  ylab = "n doors learned",
  pch = 19,        # point shape
  col = "blue"
)

## now looking at the evidence for knowledge of each door separately
## does the algorithm appropriate capture the number of doors learned based on 
## evidence for knowledge of each individual door? YES
beta_maps_by_door <- beta_maps_door %>% 
  filter(sid == 6, ses == 3, transfer == 2)

par(mfrow = c(2, 2))
plot( # plot for k1, 2, 3, and 4 i.e., each door separately
  beta_maps_by_door$event, beta_maps_by_door$k4,
  type = 'l',
  xlab = "trial",
  ylab = "p(know door 4)",
  pch = 19,        # point shape
  col = "blue"
)


# now, looking at sub-51 novel task (transfer = 1) : high accuracy (0.91) 
# but only learned 3 doors
## does the algorithm appropriate capture the number of doors learned based on 
## evidence for knowledge of each individual door? YES
sub_trl_acc <- trl_acc %>% 
  filter(sub == 51, ses == 3, transfer == 1)

par(mfrow = c(1, 2))

plot(
  sub_trl_acc$t, sub_trl_acc$accuracy,
  type = 'l',
  xlab = "trial",
  ylab = "accuracy",
  pch = 19,        # point shape
  col = "blue"
)

beta_maps_by_strat <- beta_maps_strat %>% 
  filter(sid == 51, ses == 3, transfer == 1)

plot(
  beta_maps_by_strat$event, beta_maps_by_strat$win,
  type = 'l',
  xlab = "trial",
  ylab = "n doors learned",
  pch = 19,        # point shape
  col = "blue"
)


beta_maps_by_door <- beta_maps_door %>% 
  filter(sid == 51, ses == 3, transfer == 1)

par(mfrow = c(2, 2))
plot(
  beta_maps_by_door$event, beta_maps_by_door$k3,
  type = 'l',
  xlab = "trial",
  ylab = "p(know door 3)",
  pch = 19,        # point shape
  col = "blue"
)

# now, looking at sub-31 novel transfer (transfer = 1): high accuracy but very late k4 onset
## does the algorithm appropriate capture the number of doors learned based on 
## evidence for knowledge of each individual door? YES

sub_trl_acc <- trl_acc %>% 
  filter(sub == 31, ses == 3, transfer == 1)

par(mfrow = c(1, 2))

plot(
  sub_trl_acc$t, sub_trl_acc$accuracy,
  type = 'l',
  xlab = "trial",
  ylab = "accuracy",
  pch = 19,        # point shape
  col = "blue"
)

beta_maps_by_strat <- beta_maps_strat %>% 
  filter(sid == 31, ses == 3, transfer == 1)

plot(
  beta_maps_by_strat$event, beta_maps_by_strat$win,
  type = 'l',
  xlab = "trial",
  ylab = "n doors learned",
  pch = 19,        # point shape
  col = "blue"
)


beta_maps_by_door <- beta_maps_door %>% 
  filter(sid == 31, ses == 3, transfer == 1)

par(mfrow = c(2, 2))
plot(
  beta_maps_by_door$event, beta_maps_by_door$k4,
  type = 'l',
  xlab = "trial",
  ylab = "p(know door 4)",
  pch = 19,        # point shape
  col = "blue"
)

# also looking at sub-77 complete transfer (transfer = 3): again, high acc but late k4 onset
## does the algorithm appropriate capture the number of doors learned based on 
## evidence for knowledge of each individual door? YES

sub_trl_acc <- trl_acc %>% 
  filter(sub == 77, ses == 3, transfer == 3)

par(mfrow = c(1, 2))

plot(
  sub_trl_acc$t, sub_trl_acc$accuracy,
  type = 'l',
  xlab = "trial",
  ylab = "accuracy",
  pch = 19,        # point shape
  col = "blue"
)

beta_maps_by_strat <- beta_maps_strat %>% 
  filter(sid == 77, ses == 3, transfer == 3)

plot(
  beta_maps_by_strat$event, beta_maps_by_strat$win,
  type = 'l',
  xlab = "trial",
  ylab = "n doors learned",
  pch = 19,        # point shape
  col = "blue"
)


beta_maps_by_door <- beta_maps_door %>% 
  filter(sid == 77, ses == 3, transfer == 3)

par(mfrow = c(2, 2))
plot(
  beta_maps_by_door$event, beta_maps_by_door$k4,
  type = 'l',
  xlab = "trial",
  ylab = "p(know door 4)",
  pch = 19,        # point shape
  col = "blue"
)
