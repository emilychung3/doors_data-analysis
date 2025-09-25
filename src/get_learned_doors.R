get_learned_doors <- function(grp_data){
  
  door_lc <- c()
  
  for (su in unique(grp_data$sub)){
    
    # getting doors numbers for novel (transfer = 1), partial (transfer = 2) and complete transfer (transfer = 3)
    novel_doors <-grp_data %>% filter(sub==su,ses==3,transfer==1,door_cc==1) %>% pull(door) %>% unique()
    partial_doors <- grp_data %>% filter(sub==su,ses==3,transfer==2,door_cc==1) %>% pull(door) %>% unique()
    full_doors <- grp_data %>% filter(sub==su,ses==3,transfer==3,door_cc==1) %>% pull(door) %>% unique()
    
    # getting doors for each learned task
    house_1 <- grp_data %>% filter(sub==su,ses==1,context==1,door_cc==1) %>% pull(door) %>% unique()
    house_2 <- grp_data %>% filter(sub==su,ses==1,context==2,door_cc==1) %>% pull(door) %>% unique()
    
    # getting learned doors from House 1 and 2 which were not reused (setting-errors) in each trf condition
    learned_doors_novel <- c(house_1, house_2) #house 1, and house 2
    learned_doors_partial <- c(house_1[is.na(match(house_1,partial_doors))], house_2[is.na(match(house_2,partial_doors))])
    learned_doors_complete <- c(house_1[is.na(match(house_1,full_doors))], house_2[is.na(match(house_2,full_doors))])

    #getting rows where setting error is made
    tmp <- grp_data %>% 
      filter(sub == su) %>% 
      mutate(door_lc = case_when(ses == 1 ~ NA, ses == 2 ~ NA, ses == 3 ~ 0, .default = 0))
    
    when_setting_error <- list()
    
    for (i in 1:3) {
      if (i == 1) {
        when_setting_error[[i]] <- which(tmp$transfer == 1 & tmp$door %in% learned_doors_novel)
      } else if (i == 2) {
        when_setting_error[[i]] <- which(tmp$transfer == 2 & tmp$door %in% learned_doors_partial)
      } else if (i == 3) {
        when_setting_error[[i]] <- which(tmp$transfer == 3 & tmp$door %in% learned_doors_complete)
      } 
    }
    when_setting_error <- unlist(when_setting_error, use.names = FALSE)
  
 
    # recording trials where setting error is made with doors_lc = 1
    tmp$door_lc[when_setting_error] <- 1
    
    door_lc <- c(door_lc, tmp %>% 
      select(door_lc))

  }
  door_lc <- unlist(door_lc)
  return(door_lc)
}