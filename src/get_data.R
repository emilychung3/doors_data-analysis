get_data <- function(data_path, sub, ses, train_type, train_doors) {
  # reads in trial info and sample data from 'trls' and 'beh' files and formats into a
  # one-row-per-trial data frame

  if (version == "pilot-data-00" || version == "pilot-data-01" || ses != "ses-learn") {
    success <- c()
    success <- rbind(success, file.exists(file.path(data_path, sub, ses, "beh", paste(sub, ses,
      "task-mforage_trls.tsv",
      sep = "_"
    ))))
    success <- rbind(success, file.exists(file.path(data_path, sub, ses, "beh", paste(sub, ses,
      "task-mforage_beh.tsv",
      sep = "_"
    ))))
  } else {
    haus <- c("house-1", "house-2")
    success <- c()
    for (h in haus) {
      success <- rbind(success, file.exists(file.path(data_path, sub, ses, "beh", paste(sub,
        ses, h, "task-mforage_trls.tsv",
        sep = "_"
      ))))
      success <- rbind(success, file.exists(file.path(data_path, sub, ses, "beh", paste(sub,
        ses, h, "task-mforage_beh.tsv",
        sep = "_"
      ))))
    }
  }

  if (all(success)) {
    if (version == "pilot-data-00" || version == "pilot-data-01" || ses != "ses-learn") {
      trials <- read.table(file.path(data_path, sub, ses, "beh", paste(sub, ses, "task-mforage_trls.tsv",
        sep = "_"
      )), header = TRUE)
      resps <- read.table(file.path(data_path, sub, ses, "beh", paste(sub, ses, "task-mforage_beh.tsv",
        sep = "_"
      )), header = TRUE)
    } else {
      trials <- read.table(file.path(data_path, sub, ses, "beh", paste(sub, ses, "house-1",
        "task-mforage_trls.tsv",
        sep = "_"
      )), header = TRUE)
      trials <- rbind(trials, read.table(file.path(data_path, sub, ses, "beh", paste(sub,
        ses, "house-2", "task-mforage_trls.tsv",
        sep = "_"
      )), header = TRUE))

      resps_1 <- read.table(file.path(data_path, sub, ses, "beh", paste(sub, ses, "house-1",
        "task-mforage_beh.tsv",
        sep = "_"
      )), header = TRUE)
      resps_2 <- read.table(file.path(data_path, sub, ses, "beh", paste(sub, ses, "house-2",
        "task-mforage_beh.tsv",
        sep = "_"
      )), header = TRUE)
      resps_3 <- read.table(file.path(data_path, sub, ses, "beh", paste(sub, ses, "house-9",
        "task-mforage_beh.tsv",
        sep = "_"
      )), header = TRUE)
    }

    ### trim the data remove practice trials and reset trial numbers
    if (ses == "ses-learn") {
      trials <- trials %>%
        filter(t != 999)
      if (version == "pilot-data-00" || version == "pilot-data-01") {
        resps <- resps %>%
          filter(cond != 3)
        resps$t <- resps$t - 5
      } else {
        resps_1 <- resps_1 %>%
          filter(cond != 3) # remove practice trials from house-1
        resps_1$t <- resps_1$t - 5 # adjust the trial counter for house-1, now that practice trials are gone
        resps_2$t <- resps_2$t + resps_1$t[nrow(resps_1)]
        resps_3$t <- resps_3$t + resps_2$t[nrow(resps_2)]

        resps <- rbind(resps_1, resps_2, resps_3)
        #resps$learn_phase <- c(rep(0,nrow(resps_1)),rep(0,nrow(resps_2)),rep(1,nrow(resps_3)))
      }
    }
    resps <- resps %>%
      filter(door > 0) # we only care about samples in which people hovered or clicked on a door
    resps <- resps %>%
      rename(context = cond) %>%
      mutate(door_cc = case_when(door_p > 0 ~ 1, door_p == 0 ~ 0, .default = 0))
    if (ses == "ses-learn") {
      resps <- resps %>%
        rename(ses = learn)
    } else if (ses == "ses-train") {
      resps <- resps %>%
        rename(ses = train)
    } else {
      resps <- resps %>%
        rename(ses = test)
    }

    ### find the important events     
    resps <- resps %>%
      mutate(on = c(onset[[1]], case_when(diff(open_d) != 0 ~ onset[2:length(onset)], diff(door) != 
        0 ~ onset[2:length(onset)], .default = NA))) %>% 
      mutate(off = c(case_when(diff(open_d) != 0 ~ onset[1:length(onset) - 1], diff(door) != 0 ~
        onset[1:length(onset) - 1], .default = NA), onset[[length(onset)]])) %>%
      filter(!is.na(on) | !is.na(off)) %>% 
      mutate(off = c(off[2:length(off)], NA)) %>%
      filter(!is.na(on)) %>%
      mutate(off = case_when(!is.na(off) ~ off, is.na(off) ~ c(on[2:length(on)], NA), .default = NA)) # if two onsets occured back-to-back, use the second onset as the first offset
    trials <- unique(resps$t)
    resps <- resps %>%
      mutate(subses = case_when(t %in% trials[1:round(length(trials) / 2)] ~ 1, .default = 2), .after=ses)

    ### code door by whether it's part of current context, other context, or no context
    doors <- resps %>%
      filter(door_cc == 1) %>%
      group_by(context) %>%
      distinct(door)
    tmp <- list()
    
    if (ses == 'ses-learn' || ses == 'ses-train'){
      for (i in 1:2) {
        tmp[[i]] <- resps %>%
          filter(context == i) %>%
          mutate(door_oc = case_when((!(door %in% filter(doors, context == i)$door) & door %in%
            doors$door) ~ 1, .default = 0),.after=door_cc)
      }
      resps <- rbind(tmp[[1]], tmp[[2]]) %>%
        arrange(t)
    } else if (ses == 'ses-test') {
        resps <- resps %>% 
          mutate(door_oc = rep(NA, nrow(resps)))
                      
      }

    ### format events
    resps <- resps %>% select(!c(onset, door_p:y)) # remove unnecessary variables
    resps <- resps %>%
      filter(open_d == 1) %>%
      select(!open_d) # find clicks

    # record whether each trial starts with a context switch
    resps <- get_switch(resps)

    # mark the train phase switch rate and context-one doors in the test phase data
    if (ses == "ses-train") {
      # calculate the switch rate
      sr <- resps %>%
        group_by(t) %>%
        summarise(sr = max(switch)) %>%
        ungroup() %>%
        summarise(sr = mean(sr))
      if (sr$sr[[1]] > 0.2) {
        train_type <- 2 # high switch rate (30%, but will be nearer 0.296875)
      } else {
        train_type <- 1 # low switch rate
      }
    }
    resps <- resps %>%
      mutate(train_type = c(kronecker(matrix(1, nrow(resps), 1), train_type)))
    
    if (ses == "ses-test"){
      
      # the codes under "context" actually indicate transfer (1 = full transfer)
      # copy them to a "transfer" column
      resps <- resps %>% 
        mutate(transfer = context)

      # record which order the transfer condition is completed during transfer test
      # and update "context" with new house numbers
      change <- resps %>% 
        mutate(change = c(diff(resps$transfer), 0))
      last_row_per_cond = c(which(!change$change == 0), nrow(resps))
      nrows_per_cond = c(last_row_per_cond[1], diff(last_row_per_cond))
      n_orders = 3
      
      resps <- resps %>% 
        mutate(order = rep(1:n_orders, times = nrows_per_cond), # this variable gives the same thing as 'context' 
               context = rep(1:3, times = nrows_per_cond)) # this is the same as order...
      
      #   compare test context 3 (complete transfer) to the relevant door numbers from train phase house 1 and 2  
      if(all(sort(unlist(doors %>% filter(context==3) %>% pull(door))) == sort(unlist(train_doors %>% filter(ses==1, context==1) %>% pull(door))) )) {
        house <- 1
      } else if (all( sort(unlist(doors %>% filter(context== 3) %>% pull(door))) == sort(unlist(train_doors %>% filter(ses==1, context==2) %>% pull(door))) )){
        house <- 2
      } 
      
      #   update "context" with new house numbers
      #   record which train phase house no. maps to full transfer (house 3)
      #   transfer = 1 : novel
      #   transfer = 2: partial 
      #   transfer = 3: complete
      resps <- resps %>% 
        mutate(original_house = case_when(transfer == 3 ~ house, .default = NA))

      
    }else{
      resps <- resps %>% 
        mutate(transfer = c(kronecker(matrix(1, nrow(resps), 1), NA)),
          order = c(kronecker(matrix(1, nrow(resps), 1), NA)),
          original_house = c(kronecker(matrix(1, nrow(resps), 1), NA)))
    }

    return(resps) 
  } else {
    stop(paste("check data for", file.path(data_path, sub, ses)))
  }
}
