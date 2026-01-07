library(tidyverse)
library(combinat)

get_transfer_order <- function(data) {
# generate a data frame which matches transfer orders to a unique group
transfer_conditions <- c(1, 2, 3)
transfer_orders <- permn(transfer_conditions)
transfer_orders <- do.call(rbind, transfer_orders)
order_id <- c(1, 2, 3, 4, 5, 6)

all_orders <- data.frame(order_id  = order_id,
                     first = transfer_orders[, 1],
                     second = transfer_orders[, 2],
                     third = transfer_orders[, 3]
                     )

write.csv(all_orders, file = "src/transfer_orders.csv")

# now get the transfer orders for each participant and assign them with an order ID
grp_order_data <- data.frame(sub = integer(), first = integer(), second = integer(), third = integer(), order = integer())

for (sub in subs) {
  
  print(sub)
  
  sid <- as.numeric(substring(sub,5,7))
  
  this_order <- grp_data %>% 
    filter(sub == sid, ses == 3) %>% 
    group_by(sub, context) %>% 
    distinct(transfer)
  
  this_order_wide <- this_order %>% 
    pivot_wider(names_from = context, values_from = transfer)
  
  this_order_wide <- inner_join(this_order_wide, all_orders, 
                               by = c('1' = 'first', 
                                      '2' = 'second', 
                                      '3' = 'third'))
  this_order_wide <- this_order_wide %>% 
    rename(first = '1',
           second = '2',
           third = '3')
  
  grp_order_data <- rbind(grp_order_data, this_order_wide)
  grp_order_data <- grp_order_data %>% select(sub, order_id)
}
grp_order_data
}

