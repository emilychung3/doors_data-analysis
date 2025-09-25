# lydia barnes, may 2024 takes event data (clicks or hovers), identifies when contexts change, and
# marks all events in a switch trial as switch events

get_switch <- function(data) {
  data <- data %>%
    mutate(switch = c(0, case_when(diff(context) != 0 ~ 1, .default = 0))) # in event data, note when switches happen
  tmp <- data %>%
    group_by(t) %>%
    summarise(switch = max(switch)) # if a switch event has happened in a trial, that is a switch trial
  for (s in 1:nrow(data)) {
    # for each event,
    t <- data$t[s] # find the trial number
    i <- which(sapply(tmp$t, FUN = function(x) t %in% x)) # find where this trial number is in tmp
    if (tmp$switch[i
    ] == 1) {
      # if it's a switch trial
      data$switch[s
      ] <- 1 # label this event as a switch
    }
  }
  return(data)
}
