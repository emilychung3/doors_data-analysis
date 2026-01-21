# E.Chung, 2025
# This script gets the digit span scores for each participant from a combined 
# spreadsheet used to score all participants on each sequence. Here, we extract 
# the maximum sequence length that each participant was able to correctly recall
# for the forward and backwards version of the task.

library(tidyverse)
source(file.path("src", "get_subs.R"))

version <- "individual_differences"

data_path <- file.path("C:/Users/echung/UNSW/Learning and Attention Group - Emily PhD project - exp1_data/individual_differences")
digit_span <- read.csv(file.path(data_path, "digit_span_data.csv"))

subs <- get_subs(version)

group_data <- data.frame(sub = integer(), max_fw_len = integer(), max_bw_len = integer())

for (sub in subs){
   sid <- as.numeric(substring(sub,5,7))
   sub_data <- digit_span %>% filter(sub == sid) # remove capitals from variable names...
   
   correct_fw_idx <- which(sub_data$fw_score == 1)
   max_fw_seqLen <- max(sub_data$fw_seqlength[correct_fw_idx])
    
   correct_bw_idx <- which(sub_data$bw_score == 1)
   max_bw_seqLen <- max(sub_data$bw_seqlength[correct_bw_idx])
   
   sub_digit_span <- data.frame(sub = sid, 
                                max_fw_digits = max_fw_seqLen, 
                                max_bw_digits = max_bw_seqLen)
   
   group_data <- rbind(group_data, sub_digit_span) %>% 
     arrange(sub)
   
}

write.csv(group_data, file.path("res/digit_span_scores.csv"), row.names = FALSE)