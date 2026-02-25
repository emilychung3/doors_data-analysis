# E. Chung, Feb 2026
# In this script, I am getting the amount of time spent in the training stage.

library(tidyverse)

version <- "data_sandpit"
ses <- "ses-train"

data_path <- file.path("/Users/emilychung/Library/CloudStorage/OneDrive-SharedLibraries-UNSW/Learning and Attention Group - Emily PhD project - Emily PhD project/exp1_data", version)
subs <- get_subs(version)

group_data <- data.frame(sub = integer(), train_time = integer())
for (sub in subs) {
    print(sub)
    sid <- as.numeric(substring(sub,5,7))
    
    resps <- read.table(file.path(data_path, sub, ses, "beh", paste(sub, ses, "task-mforage_beh.tsv",
                                                                  sep = "_"
    )), header = TRUE)
    
    train_time <- resps$onset[nrow(resps)] - resps$onset[1]
    
    sub_data <- data.frame(sub = sid, train_time = train_time)
    group_data <- rbind(group_data, sub_data)
}

train_type <- rep(c("Stable", "Variable"), times = length(subs)/2) # hardcoding group membership

group_data <- cbind(group_data, train_type) %>% 
  select(sub, train_type, train_time)

write.csv(group_data, "res/train_time.csv", row.names=FALSE)
