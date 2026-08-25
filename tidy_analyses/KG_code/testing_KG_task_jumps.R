# E. Chung, 2026

source("rerun_taskjumps_TE/get_task_jumps_KG.R")

event_data <- read.csv("/Users/emilychung/Documents/GitHub/doors_data-analysis/res/exp_lt_evt.csv")
event_data <- event_data %>% filter(ses == 2)

# testing
sub1t24 <- event_data %>% filter(sub == 1, t == 24)

sub2t61 <- event_data %>% filter(sub == 1, t == 61) # pre-emptive switch on switch trl

sub2t10 <- event_data %>% filter(sub == 2, t == (9:10)) # pre-emptive switch on stay trl

# getting jumps data
jumps_count_KG <- get_task_jumps(event_data) # get jumps count

jumps_KG <- jumps_count_KG %>% 
  group_by(sub, train_type, switch, context, t) %>%  
  summarise(task_jumps = task_jumps[1]) %>% 
  arrange(sub, t) 
  
jumps_KG <- jumps_KG %>% 
  group_by(sub, train_type, switch) %>% 
  summarise(task_jumps = mean(task_jumps))

jumps_KG %>% 
  group_by(train_type, switch) %>% 
  summarise(mean = mean(task_jumps),
            sd = sd(task_jumps))
