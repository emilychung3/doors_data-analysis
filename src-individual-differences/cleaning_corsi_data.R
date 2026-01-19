# E. Chung, 2025
# Two separate python scripts were used to run the forward and backwards version of the
# corsi block tapping task for sub-04 to sub-18. This was later combined into the
# same script to facilitate with ease of testing.
# This script is used to combine the separate data files generated for the forward
# and backwards version of the corsi block tapping task for those participants who
# had separate data files for each version of the task.
# I am also removing demographics recorded for all participants because these
# have been stored elsewhere.


library(tidyverse)

# relevant subject numbers
subs_data_separated <- c('sub-04', 'sub-05', 'sub-06', 'sub-07', 'sub-08', 'sub-10',
                         'sub-11','sub-12', 'sub-13', 'sub-14', 'sub-15', 'sub-16',
                         'sub-17', 'sub-18')
subs_data_combined <- c('sub-01', 'sub-02', 'sub-03', 'sub-09', 'sub-19', 'sub-20',
                        'sub-21', 'sub-22', 'sub-23', 'sub-24', 'sub-25', 'sub-26', 
                        'sub-27', 'sub-28', 'sub-29', 'sub-30', 'sub-31', 'sub-32', 
                        'sub-33', 'sub-34', 'sub-35', 'sub-36', 'sub-37', 'sub-38', # data lost for'sub-38', 
                        'sub-39', 'sub-40', 'sub-41', 'sub-42', 'sub-43', 'sub-44',
                        'sub-45', 'sub-46', 'sub-47', 'sub-48', 'sub-49', 'sub-50',
                        'sub-51', 'sub-52', 'sub-53', 'sub-54', 'sub-55', 'sub-56',
                        'sub-57', 'sub-58', 'sub-59', 'sub-60', 'sub-61', 'sub-62', 
                        'sub-63', 'sub-64', 'sub-65', 'sub-66', 'sub-67', 'sub-68',
                        'sub-69', 'sub-70', 'sub-71', 'sub-72', 'sub-73', 'sub-74',
                        'sub-75', 'sub-76', 'sub-77', 'sub-78', 'sub-79', 'sub-80',
                        'sub-81', 'sub-82', 'sub-83', 'sub-84', 'sub-85', 'sub-86', 
                        'sub-87','sub-88', 'sub-89', 'sub-90', 'sub-91', 'sub-92', 
                        'sub-93','sub-94', 'sub-95', 'sub-96')


# data path for raw data 
data_path <- file.path("C:/Users/echung/UNSW/Learning and Attention Group - Emily PhD project - exp1_data/spatialWM")


# cleaning data files for subjects where fwd and bwd data are separated ----------------------------------------
for (sub in subs_data_separated) {
  fwd_data <- read.csv(file.path(data_path, "data_sandpit", sub, paste("fwd-corsi_and_demographics", 
                                                  sub, "beh.csv", sep = "_")), header = TRUE)
  bwd_data <- read.csv(file.path(data_path, "data_sandpit", sub, paste("bwd-corsi_and_demographics", 
                                                sub, "beh.csv", sep = "_")), header = TRUE)
  
  fwd_data <- fwd_data %>% 
    mutate(version = 'fwd') %>% 
    select(-age, -gender, -Monitor.Name, -X)
  bwd_data <- bwd_data %>% 
    mutate(version = 'bwd') %>% 
    select(-Monitor.Name, -X)
  
  combined_data <- rbind(fwd_data, bwd_data)
  combined_data <- combined_data[, c('sub', 'version', 'seq_len', 'trial', 'seq', 'selected',
                                     'correct_seq')]
  
  # writing to a new data folder for this participant containing cleaned data
  participant_folder <- file.path(data_path, "clean_data_sandpit", sub)
  if (!dir.exists(participant_folder)) {
    dir.create(participant_folder, recursive = TRUE)
  }
  
  clean_file <- file.path(participant_folder, paste(sub, "corsi.csv", sep = "_"))
  write_csv(combined_data, clean_file)
}

# cleaning data files for subjects with a single data file for both versions of the task ---------------------------
for (sub in subs_data_combined){
  if (sub == 'sub-38') {
    print("skipping missing data")
  }
  combined_data <- read.csv(file.path(data_path, "data_sandpit", sub, paste("corsi_and_demographics", 
                                                                       sub, "beh.csv", sep = "_")), header = TRUE)
  # removing demographics
  combined_data <- combined_data %>% 
    select(-Monitor.Name, -age, -gender, -X)
  
  # rearranging columns
  combined_data <- combined_data[, c('sub', 'version', 'seq_len', 'trial', 'seq', 'selected',
                                     'correct_seq')]
  
  # writing to a new data folder for this participant containing cleaned data
  participant_folder <- file.path(data_path, "clean_data_sandpit", sub)
  if (!dir.exists(participant_folder)) {
    dir.create(participant_folder, recursive = TRUE)
  }
  
  clean_file <- file.path(participant_folder, paste(sub, "corsi.csv", sep = "_"))
  write_csv(combined_data, clean_file)
  
}