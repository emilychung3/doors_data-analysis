#E.Chung, August 2024
library(tidyverse)

dat <- read.csv("res/exp_lt_trl.csv")

# get summarised training data
training_dat <- dat %>% 
  filter(ses == 2)  %>%
  group_by(sub, train_type, switch) %>% 
  summarise(mean_task_jumps  = mean(context_changes),
         mean_gen_error = mean(general_errors)) 

boxplot(mean_task_jumps ~ train_type*switch, data=training_dat) 
boxplot(mean_gen_error ~ train_type*switch, data=training_dat) 

# get summarised transfer data
# accuracy and setting-error
transfer_dat <- dat %>% filter(ses == 3) %>% group_by(sub, context) %>% slice_head(n = 20) %>% 
  group_by(sub, train_type, transfer) %>% 
  summarise(mean_acc = mean(accuracy),
         mean_set_error = mean(learned_setting_errors)) %>% 
  arrange(sub)

boxplot(mean_acc ~ train_type*transfer, data=transfer_dat) 
boxplot(mean_set_error ~ train_type*transfer, data=transfer_dat) 

# get summarised learning onset
maggi_dat <- read.csv("res/exp_lt_maggi-k4.csv")

learn_onset_dat <- maggi_dat %>%  filter(ses == 3, k4_onset != Inf)

boxplot(k4_onset~ train_type*transfer, data=learn_onset_dat) 

# correlate accuracy to learning onset
maggi_dat <- maggi_dat %>% filter(ses == 3)

transfer_dat_w_k4 <- cbind(transfer_dat, "k4_onset" = maggi_dat$k4_onset) %>% arrange(sub)
transfer_dat_w_k4 <- transfer_dat_w_k4 %>%  filter(k4_onset != Inf)

plot(transfer_dat_w_k4$mean_acc,  transfer_dat_w_k4$k4_onset, xlab = "accuracy", ylab = "k4_onset") # create plot

model <- lm(k4_onset ~ mean_acc, data = transfer_dat_w_k4) # add regression line
abline(model, col = "blue")

correlation <- cor(transfer_dat_w_k4$mean_acc,  transfer_dat_w_k4$k4_onset) # get r value


# get number of participants per group who learned all 4 doors
learn_onset_dat <- learn_onset_dat %>% 
  group_by(train_type, transfer) %>% 
  summarise(n = n())

learn_onset_dat <- learn_onset_dat %>% # put into wide format
  pivot_wider(names_from = transfer, values_from = n)
