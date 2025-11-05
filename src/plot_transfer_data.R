#E. Chung, October 2025
library(tidyverse)

# read data
dat <- read.csv("res/exp_lt_trl.csv")
k4_dat <- read.csv("res/exp_lt_maggi-k4.csv")

# get transfer data
transfer_dat <- dat %>% filter(ses == 3) %>% 
  group_by(sub, context) %>% 
  slice_head(n = 20) %>%  # using first 20 trials for each condition during transfer
  group_by(sub, train_type, transfer, order_id) %>% 
  summarise(mean_acc = mean(accuracy),
            mean_set_error = mean(learned_setting_errors)) %>% 
  arrange(sub)

# plot accuracy by group x transfer type
boxplot(mean_acc ~ train_type*transfer, 
        data = transfer_dat,
        names = c("stable:novel", "var:novel", "stable:partial", "var:partial", "stable:complete", "var:complete"),
        xlab = "train_type:transfer_type",
        ylab = "mean_accuracy",
        cex.axis = 0.7) 

transfer_dat %>% 
  ggplot(aes(x = as.factor(train_type), y=mean_acc, 
                            colour = as.factor(train_type), 
                            group = train_type)) +
  geom_boxplot() + 
  facet_wrap(~order_id*transfer)

# plot accuracy by group x transfer type 
boxplot(mean_set_error ~ train_type*transfer, 
        data=transfer_dat,
        names = c("stable:novel", "var:novel", "stable:partial", "var:partial", "stable:complete", "var:complete"),
        xlab = "train_type:transfer_type",
        ylab = "mean_setting_error",
        cex.axis = 0.7) 
#transfer_dat %>% ggplot(aes(x=as.factor(train_type), y=mean_acc, colour=as.factor(train_type), group=train_type)) +
#  geom_boxplot() + facet_wrap(~order*transfer)

# get summarised learning onset
k4_dat <- k4_dat %>%  filter(ses == 3)

acc_and_k4 <- cbind(transfer_dat, "k4_onset" = k4_dat$k4_onset) %>% arrange(sub)

learners_only <- acc_and_k4 %>%  filter(k4_onset != Inf) # get data for all learners
n_learners <- learners_only %>%  # count n learners
  group_by(train_type, transfer) %>% 
  summarise(n = n())

non_learners <- acc_and_k4 %>%  filter(k4_onset == Inf) # get all non learners
n_non_learners <- non_learners %>%  # count n non learners
  group_by(train_type, transfer) %>% 
  summarise(n = n())

#plot k4 onset for group x transfer type
boxplot(k4_onset~ train_type*transfer, data = k4_dat) 

# plot correlation between  accuracy to learning onse
cor <- cor(learners_only$mean_acc,  learners_only$k4_onset) # get r value

plot(learners_only$mean_acc,  learners_only$k4_onset, 
     xlab = "accuracy", 
     ylab = "k4_onset") # create plot

model <- lm(k4_onset ~ mean_acc, data = learners_only) # add regression line
abline(model, col = "blue")


