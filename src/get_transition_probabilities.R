get_transition_probabilities <- function(data){
  
  transition_probabilities <- c()
  for (su in unique(data$sub)) {
    
    # find which partial transfer doors came from train phase context 1 or 2
    partial_doors <- data %>% filter(sub==su,ses==3,transfer==2,door_cc==1) %>% pull(door) %>% unique()
    house_1 <- data %>% filter(sub==su,ses==2,context==1,door_cc==1) %>% pull(door) %>% unique()
    house_1 <- house_1[!is.na(match(house_1,partial_doors))]
    house_2 <- data %>% filter(sub==su,ses==2,context==2,door_cc==1) %>% pull(door) %>% unique()
    house_2 <- house_2[!is.na(match(house_2,partial_doors))]
    
    # reduce to train phase data
    train_data <- data %>% filter(ses==2)
    
    for (co in unique(train_data$context)){

        # reduce to correct click events on stay trials
        tmp <- train_data %>% filter(sub == su, ses == 2, context == co, switch == 0, door_cc == 1)
        if(co==1){this_house <- house_1}else{this_house <- house_2}
        
        # -------------------------------------------------------------------------
        # make the full transitions matrix
        transition_matrix <- matrix(0, nrow = 4, ncol = 4)
        doors <- unique(tmp$door)
        rownames(transition_matrix) <- doors
        colnames(transition_matrix) <- doors
        
        # select a trial
        for (tr in unique(tmp$t)) {
          trial <- tmp %>% filter(t == tr)
          
          # if there's more than one event, record door transitions
          if (nrow(trial) > 1) {
            for (i in 2:nrow(trial)) {
              door <- trial$door[i]
              previous <- trial$door[i - 1]
              transition_matrix[which(doors==previous), which(doors==door)] <- transition_matrix[which(doors==previous),which(doors==door)]+1 
            }
          }
        }
        
        hits <- transition_matrix[match(this_house,colnames(transition_matrix)),match(this_house,colnames(transition_matrix))]
        hits <- c(hits[2,1],hits[1,2])
        transition_probabilities <-  c(transition_probabilities,
                                       mean(c( hits[1]/sum(transition_matrix[,match(this_house[1],colnames(transition_matrix))]), 
                                               hits[2]/sum(transition_matrix[,match(this_house[2],colnames(transition_matrix))]))),
                                       NA)
        
    }
  }
  
  return(transition_probabilities)
}