# E.Chung, 2025
# In this script, I am getting the longest sequence length that can be recalled 
# for each participant in the forward and backward version of the Corsi Block 
# Tapping Task. In this task, participants recall the order in which a sequence 
# of blocks flash on a computer screen.

library(tidyverse)

source("src/get_subs.R")

version <- 'spatialWM/clean_data_sandpit'
corsi_ver <- c('fwd', 'bwd')

data_path <- file.path("C:/Users/echung/UNSW/Learning and Attention Group - Emily PhD project - exp1_data", version)
subs <- get_subs(version) 

group_corsi <- data.frame(sub = integer(), version = integer(), 
                          max_seq_length = integer())

for (sub in subs){
  sid <- as.numeric(substring(sub,5,7))
  
  if (sub == 'sub-38') {
    print('skipping missing data')
  } 
  else {
  for (ver in corsi_ver){
    corsi_data <- read.csv(file.path(data_path, sub, paste(sub, "corsi.csv", sep = "_")), header = TRUE)
    ver_data <- corsi_data %>% filter(version == ver)
    
    correct_seqlen <- ver_data$seq_len[which(ver_data$correct_seq == 'True')]
    max_seq_len <- max(correct_seqlen)
    
    scores <- data.frame(sub = sid, version = ver, max_seq_len = max_seq_len)
    group_corsi <- rbind(group_corsi, scores)
    }
  }
}

# now change to wide format
group_corsi <- group_corsi %>% 
  pivot_wider(names_from = version, values_from = max_seq_len) %>% 
  rename('max_fwd_corsi' = 'fwd',
         'max_bwd_corsi' = 'bwd')

write.csv(group_corsi, file.path("res/corsi_scores.csv"), row.names = FALSE)
