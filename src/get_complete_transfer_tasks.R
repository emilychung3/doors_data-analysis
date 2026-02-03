# E. Chung, 2026
# In this script, I am getting which task set was transferred from the learning 
# stage to the complete transfer condition during the transfer test phase.

exp_lt_data <- read.csv('res/exp_lt_trl.csv')

complete_transfer <- exp_lt_data %>% 
  group_by(sub, ses, train_type, transfer, order_id) %>% 
  filter(ses == 3, transfer == 3) %>% 
  summarise(complete_task = unique(original_house))

write.csv(complete_transfer, "src/complete_transfer_tasks.csv", row.names = FALSE)

