# lydia barnes, may 2024
# edited by emily chung, september 2025 
# applies maggi algorithm. algorithm estimates probability of using a given strategy, weighting by recency.

# E.C. this code only runs for 'sub-01', sub-03" and 'sub-04' from the pilot-ver2 files on teams :(
# this error comes up when running all other participants 
# Error in rbind(deparse.level, ...) : 
#   numbers of columns of arguments do not match

library(zeallot) #unpack/destructure with %<-%
library(tidyverse)

source(file.path(getwd(), "src-learn", "get_maggi.R"))
source(file.path(getwd(), "src-learn", "format_data_for_maggi.R"))
source(file.path(getwd(), "src", "get_subs.R"))

set.seed(17)
simulation <- FALSE

if (simulation){ #if False, 
  # synthesise data
  ps <- c(.1, .3, .6, .9) #reasonable steps in the probability of successes over runs of trials
  n <- 25 #trials per run
  data <- unlist(lapply(ps, rbinom, n=n, size=1)) # n * length(ps) trials drawn from a binomial distribution
  c(alphas,betas,beta_map,beta_variance) %<-% get_maggi(data)
  
  # view the final beta distribution
  increments <- seq(0,1,by=.01)
  plot(increments,dbeta(increments,alphas[length(data)],betas[length(data)]),type="l",col="darkgreen") 
  
}else{
  # read real data
  
  project_path <- getwd()
  
  # settings ----------------------------------------------------------------
  version <- "data_sandpit"
  exp <- "exp_lt" # experiment: 'exp_ts' (task-switching) or 'exp_lt' (learning transfer)
  sess <- c(1,3) # session: 1 = 'ses-learn', 2 = 'ses-train', 3 = 'ses-test'.
  colours <- c("darkgreen","limegreen","gold","orange")
  save_plots <- FALSE
  specific_doors = TRUE
  
  subs <- get_subs(version)
  events <- read.csv(file.path('res',paste(paste(exp, "evt", sep='_'), ".csv", sep=''))) 
  group_data <- data.frame(
    sub = integer(), ses = integer(), context = integer(), train_type = integer(), transfer = integer(), event = integer(), 
    door1 = numeric(), door2 = numeric(), door3 = numeric(), door4 = numeric(), n_doors_learned = integer(), win = integer(), stable_k4 = integer()
  )
  session_names <- c('ses-learn','ses-test') 
  for (subject in subs){
    
    print(subject)
    sid <- as.numeric(substring(subject,5,7))

    train_type <- events %>% filter(sub==sid, ses==2) %>% pull(train_type)
    train_type <- train_type[[1]]
    
    for (ses in sess){
      
      #if(exp=="exp_lt" && ses==1 && subject=="sub-64"){
      #  print("skipping missing data")
      #}else{
        
        if (ses <3) {
          conditions <- c(1,2) # 1: context 1, 2: context 2
        } else{
          conditions <- c(1, 2, 3) # 1: E.C. novel, 2: partial, 3: complete transfer
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
          strategies <- format_data_for_maggi(exp,nsub=sid,nses=ses,ncontext=condition,method="by_trial",specific_doors=TRUE,competitive=FALSE,evaluate_all=FALSE)
          
          # empty figure ------------------------------------------------------------
          if (save_plots){
            fnl <- file.path(project_path,'fig',paste(paste(exp, subject, session_names[ses], condition_names[condition], "maggi", sep = "_"), ".png", sep = ""))
            png(file = fnl)
            plot(1:nrow(strategies),rep(0,1,nrow(strategies)),type="l",col="black",ylim=c(0,1))
          }
          
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
            
            if (save_plots){
              # view alphas and betas over time
              points(1:length(strategy),beta_map,type="l",col=colours[i])
            }
            
          }
          
          if (save_plots){
            dev.off()
          }
          
          # format the data
          
          # if specific_doors = TRUE
          # E.C. creates a variable which counts the number of doors learned
          # creates a stable K4 variable which identifies the trials where all 4 doors are learned and remains the stable strategy for remaining trials
          data <- data.frame(sub = integer(), ses = integer(), context = integer(), train_type = integer(), transfer = integer(), event = integer(), k1 = numeric(), k2 = numeric(), k3 = numeric(), k4 = numeric(), win = integer())
          if (specific_doors) {
            learn_threshold <- 0.75 # participants are considered to have learned a door when beta_map = 0.75
            for (event in 1:length(beta_map)){
              n_doors_learned <- sum(beta_maps[,event]>= learn_threshold)# count the number of doors exceeding learn threshold
              tmp <- data.frame(sid, ses, context, transfer, train_type, event, k1 = beta_maps[1,event], k2 = beta_maps[2,event], k3 = beta_maps[3,event], k4 = beta_maps[4,event], n_doors_learned)
              data <- rbind(data,tmp)
            }
            
            data <- data %>% mutate(stable_k4 = 0)
            if (n_doors_learned > 0) { 
              strategy_changes <- c(NA, diff(data$n_doors_learned))# idxs trials when there is a change in the number of doors learned vice versa
              last_strategy_change <- tail(which(strategy_changes != 0), 1) # get the first trial after evidence for 4 doors
              if(strategy_changes[last_strategy_change] > 0 & data$n_doors_learned[last_strategy_change] == 4) { 
                data <- data %>% 
                  mutate(stable_k4 = case_when(event < last_strategy_change ~ 0, # stable k4 = 0 for trials before last strategy change and 1 for trials after last strategy change and when winning strategy is k4 (know 4 doors)
                                               event %in% intersect(which(event >= last_strategy_change), which(n_doors_learned == 4)) ~ 1))
              } 
            } 
          }
  
          # if specific_doors = FALSE
          # creates win variable which records the winning strategy for that event
          # also records when k4 is the winning strategy for the remaining trial i.e., when learning onset occurs
            else {
              for (event in 1:length(beta_map)){ # for each trial
              win <- which(beta_maps[1:nrow(beta_maps),event] == max(beta_maps[1:nrow(beta_maps),event])) #index the winning strategy (strategy with most evidence) for that event
              if (sum(beta_maps[1:nrow(beta_maps),event])==0){win <- NA} #if there is no evidence for any winning strategy, make win = NA
              tmp <- data.frame(sid, ses, context, transfer, train_type, event, k1 = beta_maps[1,event], k2 = beta_maps[2,event], k3 = beta_maps[3,event], k4 = beta_maps[4,event], win)
              data <- rbind(data,tmp)
            }
            
            last_strategy_change <- max(which(diff(data$win)!=0))+1 # get the first event after strategy has changed
            data <- data %>% 
              mutate(stable_k4 = case_when(event < last_strategy_change ~ 0, # stable k4 = 0 for trials before last strategy change and 1 for trials after last strategy change and when winning strategy is k4 (know 4 doors)
                                           event %in% intersect(which(event >= last_strategy_change), which(win == 4)) ~ 1, .default = NA))
            }
          group_data <- rbind(group_data,data)
        }
      }
    }
  }
  
  # threshold ---------------------------------------------------------------
results <- group_data %>% group_by(sid,ses,context,train_type,transfer) %>% summarise(nsamples = n(),k4_onset = min(which(stable_k4==1)))
  #E.C. add n doors learned for each participant


write.csv(group_data,file.path('res',paste(paste(exp,'maggi-map',sep='_'),'csv', sep='.')))
write.csv(results,file.path('res',paste(paste(exp,'maggi-k4',sep='_'),'csv', sep='.')))



