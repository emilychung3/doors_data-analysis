# E. Chung, 2026
# visualising the number of clicks made by participants in each group x trial type
# condition.

rm(list = ls())

library(tidyverse)

trial_data <- read.csv("res/exp_lt_trl.csv")

nclicks <- trial_data %>% 
  filter(ses == 2) %>% 
  group_by(sub, train_type, switch) %>% 
  summarise(nclicks = mean(n_clicks)) 

nclicks$train_type <- as.factor(nclicks$train_type)
levels(nclicks$train_type) <- c("Rare", "Frequent") 
nclicks$switch <- as.factor(nclicks$switch)
levels(nclicks$switch) <- c("Stay", "Switch")

nclicks_plot <- ggplot(nclicks, mapping = aes(x = factor(switch), y = nclicks, 
                              fill = switch))+
  geom_boxplot()+
  facet_wrap(~train_type) +
  theme_classic()

ggsave(filename = 'res/plots/archives/nclicks_trial_type.png', 
       plot = nclicks_plot,
       height = 5,
       width = 6)