# lydia barnes, may 2024 counts, clusters, and/or traces stereotypical behaviour during training to
# explain performance at test

# NB: people can get an imperfect match score for the optimal path but an overshoot of zero if they double-click.
#--------------------------------------------------------------------------------------------------
# sources
library(tidyverse)
library(gtools)
source(file.path(getwd(), "src-stereo", "count_stereo.R"))

# settings 
exp <- "exp_ts" # experiment: 'exp_ts' (task-switching) or 'exp_lt' (learning transfer)

# paths
project_path <- getwd()
data_path <- file.path(project_path, "res")
if (!dir.exists(data_path)) {
  stop(paste0(data_path, " does not exist"))
}

#--------------------------------------------------------------------------------------------------
# load event data
fnl <- file.path(data_path, paste(paste(exp, "evt", sep = "_"), ".csv", sep = ""))
data <- read_csv(fnl, show_col_types = FALSE)
data <- data %>% filter(ses == 2)

# load shortest path data
fnl <- file.path(data_path, paste(paste(exp, "opt-path", sep = "_"), ".csv", sep = ""))
opt <- read_csv(fnl, show_col_types = FALSE)

# load graph of distances between doors
fnl <- file.path(project_path, "src-stereo", "graph.csv")
graph <- unname(data.matrix(read_csv(fnl, col_names = FALSE, show_col_types = FALSE)))

#--------------------------------------------------------------------------------------------------
# extract stereotypy metrics
stereo <- count_stereo(exp, data, opt, graph)

# save to file
fnl <- file.path(project_path, "res", paste(paste(exp, "stereotypy", sep = "_"), ".csv", sep = ""))
write_csv(stereo, fnl)
