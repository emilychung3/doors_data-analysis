# lydia barnes, may 2024
# edited by emily chung, september 2025 
# applies maggi algorithm. algorithm estimates probability of using a given strategy, weighting by recency.

library(zeallot) #unpack/destructure with %<-%
library(tidyverse)

source(file.path(getwd(), "src-learn", "get_maggi.R"))
#source(file.path(getwd(), "src-learn", "format_data_for_maggi.R"))
source(file.path(getwd(), "src-learn", "format_data_for_maggi_revised.R"))
source(file.path(getwd(), "src", "get_subs.R"))

  project_path <- getwd()
  
  # settings ----------------------------------------------------------------
  version <- "data_sandpit"
  exp <- "exp_lt" # 'exp_lt' (learning transfer)
  sess <- c(1,3) # session: 1 = 'ses-learn', 3 = 'ses-test'.
  
  subs <- get_subs(version)
  events <- read.csv(file.path('res',paste(paste(exp, "evt", sep='_'), ".csv", sep=''))) 
  group_data <- data.frame(
    sub = integer(), ses = integer(), context = integer(), train_type = integer(), transfer = integer(), event = integer(), 
    k11 = numeric(), k2 = numeric(), k3 = numeric(), k4 = numeric(), win = integer(), stable_k4 = integer()
  )
  session_names <- c('ses-learn','ses-test') 
  for (subject in subs){
    
    print(subject)
    sid <- as.numeric(substring(subject,5,7))

    train_type <- events %>% filter(sub==sid, ses==2) %>% pull(train_type)
    train_type <- train_type[[1]]
    
    for (ses in sess){
      
      if(exp=="exp_lt" && ses==1 && subject=="sub-35"){
        print("skipping missing data")
      }else{
        
        if (ses <3) {
          conditions <- c(1,2) # 1: context 1, 2: context 2
        } else{
          conditions <- c(1, 2, 3) # 1: novel, 2: partial, 3: complete transfer
        }
        
        for (condition in conditions){
          
          if (ses < 3){
            condition_names <- c("context-1","context-2")
            context <- condition
            transfer <- NA
          }else{
            condition_names <- c("novel-transfer", "partial-transfer","complete-transfer") #E.C. Added novel transfer
            context <- NA
            transfer <- condition
          }
          
          # count data (i.e., evidence) ----------------------------------------------------------------
          strategies <- format_data_for_maggi_revised(exp,nsub=sid,nses=ses,ncontext=condition,method="by_trial")
          
          # maggi -------------------------------------------------------------------
          i <- 0 
          beta_maps <- matrix(NA,4,nrow(strategies)) 
          for (strategy in names(strategies)[2:length(names(strategies))]){
            i <- i+1 # door/strategy index
            strategy <- strategies %>% pull(strategy) # get the column of count data for current strategy/door
            
            # calculate recency-weighted probability of finding strategy s
            c(alphas,betas,beta_map,beta_variance) %<-% get_maggi(strategy) # KG: oooh, this reverse pipe is news to me. How exciting!
            
            # store data
            beta_maps[i,1:ncol(beta_maps)] <- beta_map 
          }
          
          # format the data
          data <- data.frame(sub = integer(), ses = integer(), context = integer(), 
                             train_type = integer(), transfer = integer(), event = integer(), 
                             k1 = numeric(), k2 = numeric(), k3 = numeric(), k4 = numeric(), 
                             win = integer())
          
          # creates win variable which records the winning strategy for that event
          # also records when k4 is the winning strategy for the remaining trial i.e., when learning onset occurs
            
              for (event in 1:length(beta_map)){ # for each trial
              win <- max(which(beta_maps[1:nrow(beta_maps),event] == max(beta_maps[1:nrow(beta_maps),event]))) #index the winning strategy (strategy with most evidence) for that event
              if (sum(beta_maps[1:nrow(beta_maps),event])==0){win <- NA} #if there is no evidence for any winning strategy, make win = NA
              tmp <- data.frame(sid, ses, context, transfer, train_type, event, k1 = beta_maps[1,event], k2 = beta_maps[2,event], k3 = beta_maps[3,event], k4 = beta_maps[4,event], win)
              data <- rbind(data,tmp)
            }
            
            last_strategy_change <- max(which(diff(data$win)!=0))+1 # get the first event after strategy has changed
            data <- data %>% 
              mutate(stable_k4 = case_when(event < last_strategy_change ~ 0, # stable k4 = 0 for trials before last strategy change and 1 for trials after last strategy change and when winning strategy is k4 (know 4 doors)
                                           event %in% intersect(which(event >= last_strategy_change), which(win == 4)) ~ 1, .default = NA))
            
          group_data <- rbind(group_data,data)
        }
      }
    }
  }

  
  # threshold ---------------------------------------------------------------
results <- group_data %>% group_by(sid,ses,context,train_type,transfer) %>% 
  summarise(nsamples = n(),
            n_learned = tail(win, n = 1),
            k4_onset = min(which(stable_k4 == 1)))


write.csv(group_data,file.path('res',paste(paste(exp,'maggi-map',sep='_'),'csv', sep='.')), row.names = FALSE)
write.csv(results,file.path('res',paste(paste(exp,'maggi-k4',sep='_'),'csv', sep='.')), row.names = FALSE)



