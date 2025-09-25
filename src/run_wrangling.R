# lydia barnes, march 2024 this script extracts, formats, and summarises data from the 'doors'
# project.
# updated by E. Chung

### sources
library(tidyverse)
library(zeallot) #unpack/destructure with %<-%

source(file.path("src", "get_subs.R"))
source(file.path("src", "get_switch.R"))
source(file.path("src", "get_data.R"))
source(file.path("src","get_setting_stability.R"))
source(file.path("src","get_transition_probabilities.R"))
source(file.path("src","get_learned_doors.R"))

### settings

# !you will want to update these settings a lot during piloting, when the task code or the way you
# test changes, or when you test participants on different subsets of the task phases
version <- "data_sandpit" # pilot-data-00 (train and test), pilot-data-01 (learn and train), pilot-data-02 (learn and train, learn phase split into two parts)
exp <- "exp_lt" # experiment: 'exp_ts' (task-switching) or 'exp_lt' (learning transfer)
sess <- c("ses-learn","ses-train","ses-test") # session: 'ses-learn','ses-train','ses-test'. can select one (e.g. ses <- c('ses-learn')) or multiple (e.g. ses <- c('ses-train','ses-test'))

### paths

# !if you open the project thru doors.Rproj, your working directory will automatically be the
# project path
project_path <- getwd()
if (!dir.exists(file.path(project_path, "res"))) {
  # check that the results directory exists. if it doesn't, create it.
  dir.create(file.path(project_path, "res"))
}

# !you will need to change the data path to match the location of OneDrive on your personal
# computer
#data_path <- file.path("/Users/emilychung/Library/CloudStorage/OneDrive-SharedLibraries-UNSW/Learning and Attention Group - Emily PhD project - Emily PhD project/exp1_data", version)
data_path <- file.path("C:/Users/echung/UNSW/Learning and Attention Group - Emily PhD project - exp1_data", version)
if (!dir.exists(data_path)) {
  stop(paste0(data_path, " does not exist"))
}

### load an up-to-date list of participants
subs <- get_subs(version)

### extract events from the raw data

# make an empty data frame with all the variables (columns) that we will want
grp_data <- data.frame(
  sub = integer(), ses = integer(), subses = integer(), t = integer(), context = integer(), door = integer(),
  door_cc = integer(), door_oc = integer(), on = numeric(), off = numeric(), 
  switch = integer(), train_type = integer(), transfer = integer(), full_transfer_first = integer(),
  original_house = integer()
)

# for each subject and session, use the function 'get_data' to load their raw data and attach it to
# our 'grp_data' data frame with one measurement (row) per event (click or hover)
for (sub in subs) {
  print(sub)
  
  sid <- as.numeric(substring(sub,5,7))
  for (ses in sess) {
    train_type <- NA
    context_one_doors <- NA
    
    if (exp=="exp_lt" && sub=="sub-64" && ses=="ses-learn"){
     print("skipping missing data") 
    }else{
      
      if (ses == "ses-test") {
        train_type <- grp_data %>%
          filter(sub == sid, ses == 2) %>%
          select(train_type) %>% 
          unique() %>% 
          pull()
        train_doors <- grp_data %>% 
          filter(sub==sid,ses==ses,door_cc==1) %>% 
          select(ses, door,context) %>% 
          unique()
      }
      
      data <- get_data(data_path, sub, ses, train_type, train_doors) # load and format raw data
      grp_data <- rbind(grp_data, data) # add to the 'grp_data' data frame so we end up with all subjects and sessions in one spreadsheet
      
    }
  }
}

# track whether context-incorrect clicks in the test phase land on doors that were learned in the learn phase
door_lc <- get_learned_doors(grp_data)
grp_data <- grp_data %>% add_column(door_lc, .after="door_oc")


grp_data <- get_setting_stability(grp_data) # track when they changed context into the correct or other context's door set
grp_data <- grp_data %>% mutate(door_nc = case_when(door_cc==1 ~ 0, door_oc == 1 ~ 0, .default=1), .after="door_oc")

# save the formatted data
fnl <- file.path(project_path, "res", paste(paste(exp, "evt", sep = "_"), ".csv", sep = ""))
write_csv(grp_data, fnl)

### extract accuracy and response time averages from event data

# by trial
res <- grp_data %>%
  group_by(sub, ses, t, context, train_type, transfer, order, original_house) %>%
  summarise(
    switch = max(switch), n_clicks = n(), n_cc = sum(door_cc), n_oc = sum(door_oc), n_lc = sum(door_lc), n_nc = sum(door_nc),
    #setting_sticks = select_oc[1],
    #setting_slips = max(select_oc_late),
    context_changes = sum(select_cc)+sum(select_oc),
    accuracy = n_cc / n_clicks,
    #setting_errors = n_oc / n_clicks,
    general_errors = n_nc / n_clicks,
    learned_setting_errors = n_lc / n_clicks
)

# re-label exp_lt test phase "switch" trials as stay trials
res <- res %>% 
  mutate(switch = case_when(switch==1 & ses==3 ~ 0, .default = switch))


# calculate context change rates  
res$context_changes[intersect(which(res$switch==1),which(res$ses==2))] <- res$context_changes[intersect(which(res$switch==1),which(res$ses==2))]-1

fnl <- file.path(project_path, "res", paste(paste(exp, "trl", sep = "_"), ".csv", sep = ""))
write_csv(res, fnl)

