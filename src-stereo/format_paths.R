# lydia barnes, april 2024 this script formats optimal search path data for the doors project


### sources
library(tidyverse)
library(tidyjson)
source(file.path(getwd(), "src", "get_subs.R"))

# settings
exp <- "exp_lt" # experiment: 'exp_ts' (task-switching) or 'exp_lt' (learning transfer)

# paths
project_path <- getwd()

### format the optimal path data

# make an empty data frame
optimal <- data.frame(
  sub = integer(), algorithm = character(), solution = integer(), context = integer(),
  door = integer(), x = integer(), y = integer(), path_weight = double()
)

# map door IDs to x and y positions
xloc <- c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4)
yloc <- c(4, 3, 2, 1, 4, 3, 2, 1, 4, 3, 2, 1, 4, 3, 2, 1)

# extract optimal paths for each algorithm, subject, and context
algs <- c("hamiltonian", "travelling") # shortest path method: 'hp' (shortest hamiltonian path) and 'tsp' (travelling salesperson)
contexts <- c(1, 2) # context: 1 or 2
for (alg in algs) {
  # read the optimal path data
  fnl <- file.path(project_path, "src-stereo", paste(paste(alg, "solutions", sep = "_"), ".json", sep = ""))
  tmp <- read_json(fnl)
  tmp <- tmp$..JSON
  tmp <- tmp[[1]]

  for (subject in seq_along(1:length(tmp))) {
    for (ctx in contexts) {
      # find the relevant optimal paths
      opt <- tmp[[subject]][[ctx]]

      for (i in seq_along(1:length(opt[[1]]))) {
        door <- unlist(opt[[1]][[i]])
        sub <- rep(subject, length(door))
        algorithm <- rep(alg, length(door))
        solution <- rep(i, length(door))
        context <- rep(ctx, length(door))
        x <- xloc[door]
        y <- yloc[door]
        path_weight <- rep(opt[[2]], length(door))

        optimal <- rbind(optimal, data.frame(sub, algorithm, solution, context, door, x, y, path_weight))
      }
    }
  }
}

### save the formatted path data
fnl <- file.path(project_path, "res", paste(paste(exp, "opt-path", sep = "_"), ".csv",
  sep = ""
))
write_csv(optimal, fnl)
