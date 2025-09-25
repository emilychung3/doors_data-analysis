get_wrangled_data <- function(project_path,exp){
  
  if(exp=="exp_lt"){
    trial_data <- read_csv(file.path(project_path, 'res','exp_lt_trl.csv'), show_col_types = FALSE)
    avg_data <- read_csv(file.path(project_path, 'res','exp_lt_avg.csv'), show_col_types = FALSE)
    train_data <- trial_data %>%
      group_by(sub, ses, train_type, switch, transfer, full_transfer_first, original_house) %>%
      summarise_all(mean) %>% 
      ungroup() %>% 
      select(!c(subses,context)) %>% 
      filter(ses==2) %>% 
      select(sub, general_errors, context_changes, train_type, switch) %>% 
      rename(exploration = context_changes, ge = general_errors) %>% 
      mutate(train_type = train_type-1) %>% 
      mutate(sub = factor(sub), 
             train_type = case_when(train_type==0~"stable_context",train_type==1~"variable_context"),
             train_type = factor(train_type),
             switch = case_when(switch==0~"stay", switch==1~"switch"),
             switch = factor(switch))
    tmp <- train_data %>% filter(switch=="stay") %>% pull(exploration)
    train_data <- train_data %>%
      select(!exploration) %>% 
      pivot_wider(names_from = switch, names_prefix = "ge_", values_from = ge)
    train_data$exploration <- tmp
    
    test_data <- avg_data %>% 
      filter(ses == 3) %>% 
      filter(switch == 0) %>% 
      select(sub, transfer, accuracy, setting_errors, rt) %>% 
      mutate(sub = factor(sub), 
             transfer = case_when(transfer==1~"complete",transfer==2~"partial"),
             transfer = factor(transfer))
    data <- inner_join(train_data, test_data, by="sub")
    
    k4_data <- read_csv(file.path(project_path, "res", "exp_lt_maggi-k4.csv"), show_col_types = FALSE)
    k4_data <- k4_data %>% 
      rename(sub = sid) %>% 
      mutate(sub = factor(sub),
             transfer = case_when(transfer==1~"complete",transfer==2~"partial"),
             transfer = factor(transfer))
    k4_data$k4_onset[k4_data$k4_onset == Inf] <- k4_data$nclicks[k4_data$k4_onset == Inf]
    k4_data <- k4_data %>% 
      filter(ses == 3) %>% 
      select(sub, transfer, k4_onset) %>% 
      rename(k4 = k4_onset)
    data <- inner_join(data, k4_data, by=c("sub","transfer"))
    
    stereo_data <- read_csv(file.path(project_path,"res", "exp_lt_stereotypy.csv"), show_col_types = FALSE)
    stereo_data <- stereo_data %>% 
      select(sub, context, reclicks) %>%
      group_by(sub) %>% 
      summarise(perseveration = mean(reclicks)) %>% 
      mutate(sub = factor(sub))
    data <- inner_join(data, stereo_data, by="sub")
    
  }else if(exp=="exp_ts"){
    trial_data <- read_csv(file.path(project_path, 'res','exp_ts_trl.csv'), show_col_types = FALSE)
    avg_data <- read_csv(file.path(project_path, 'res','exp_ts_avg.csv'), show_col_types = FALSE)
    
    # get their training metrics
    train_data <- trial_data %>%
      group_by(sub, ses, train_type, switch) %>%
      summarise_all(mean) %>% 
      ungroup() %>% 
      filter(ses==2) %>% 
      select(sub, general_errors, context_changes, train_type, setting_errors, switch) %>% 
      rename(ge = general_errors, exploration = context_changes, setting_errors_train = setting_errors) %>% 
      mutate(train_type = train_type-1) %>% 
      mutate(sub = factor(sub), 
             train_type = case_when(train_type==0~"stable",train_type==1~"variable"),
             train_type = factor(train_type),
             switch = case_when(switch==0~"stay", switch==1~"switch"),
             switch = factor(switch))
    # select just the exploration scores from stay trials. for the other training phase metrics, pivot wider
    tmp <- train_data %>% filter(switch=="stay") %>% pull(exploration)
    train_data <- train_data %>%
      select(!exploration) %>% 
      pivot_wider(names_from = switch, values_from = c(ge,setting_errors_train))
    train_data$exploration <- tmp
      
    # get their test performance
    test_data <- avg_data %>% 
      filter(ses == 3) %>% 
      group_by(sub, switch) %>% 
      summarise_all(mean) %>% 
      select(sub, switch, accuracy, setting_errors, rt) %>% 
      mutate(sub = factor(sub), 
             switch = case_when(switch==0~"stay",switch==1~"switch"),
             switch = factor(switch))
    data <- inner_join(train_data, test_data, by="sub")
    
    stereo_data <- read_csv(file.path(project_path,"res", "exp_ts_stereotypy.csv"), show_col_types = FALSE)
    stereo_data <- stereo_data %>% 
      select(sub, context, reclicks) %>%
      group_by(sub) %>% 
      summarise(perseveration = mean(reclicks)) %>% 
      mutate(sub = factor(sub))
    data <- inner_join(data, stereo_data, by="sub")

  }else{
    message("Please select either 'exp_lt' or 'exp_ts' as the experiment.")
  }
  
  return(data)
}