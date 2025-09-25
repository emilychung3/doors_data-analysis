### sources
library(tidyverse)
library(ggthemes)
library(ggExtra)

### settings
exp <- "exp_lt" # experiment: 'exp_ts' (task-switching) or 'exp_lt' (learning transfer)
label_sz <- 30

### paths
project_path <- getwd()
data_path <- file.path(project_path, "res")
if (!dir.exists(data_path)) {
  stop(paste0(data_path, " does not exist"))
}

### data
# stereotypy measures from train phase
fnl <- file.path(project_path, "res", paste(paste(exp, "stereotypy", sep = "_"), ".csv", sep = ""))
stereo <- read_csv(fnl, show_col_types = FALSE)

# general accuracy and response times for all phases
fnl <- file.path(project_path, "res", paste(paste(exp, "avg", sep = "_"), ".csv", sep = ""))
avg <- read_csv(fnl)

# add test phase accuracy to stereo data frame
stereo_performance <- stereo %>% group_by(sub, ses, context) %>% summarise_all(mean)
#   train phase performance
stereo_performance$accuracy <- avg %>% filter(ses==2,switch==0) %>% pull(accuracy)
stereo_performance$context_changes <- avg %>% filter(ses==2,switch==0) %>% pull(context_changes)
stereo_performance$train_type <- avg %>% filter(ses==2,switch==0) %>% mutate(train_type = case_when(train_type==1~"Low Switch",train_type==2~"High Switch",.default=NA)) %>% pull(train_type)
#   test phase performance
stereo_performance$accuracy_test <- avg %>% filter(ses == 3, switch == 0) %>% pull(accuracy) # add test accuracy to stereo results
stereo_performance$context_test <- avg %>% filter(ses==3, switch==0) %>% pull(context)
#   collapsing over subsessions
tmp <- stereo_performance %>% group_by(sub,ses) %>% summarise_all(mean)
reclicks_avg <- tmp %>% pull(reclicks)
overshoot_avg <- tmp %>% pull(travelling_overshoot)
a <- stereo_performance %>% filter(ses == 2,context_test==3) %>% ungroup() %>% 
  select(sub,context,travelling_overshoot,reclicks,accuracy,context_changes,train_type,accuracy_test,context_test)
a$reclicks <- reclicks_avg
a$travelling_overshoot <- overshoot_avg
b <- stereo_performance %>% filter(ses==2, context_test==4) %>% ungroup() %>% 
  select(sub,context,travelling_overshoot,reclicks,accuracy,context_changes,train_type,accuracy_test,context_test)
b$reclicks <- reclicks_avg
b$travelling_overshoot <- overshoot_avg
stereo_performance <- bind_rows(a,b)

test_labels <- c("Full Transfer Accuracy", "Partial Transfer Accuracy")

for (ctx in 1:2) {
  p <- stereo %>% filter(ses == 2, context == ctx) %>% 
    ggplot() +
    geom_point(
      aes(
        x = entropy,
        y = reclicks,
        colour = accuracy,
      ),
      alpha = 1,
      size = 10,
    ) +
    theme_minimal(base_size = label_sz, base_family = "Roboto") +
    labs(
      x = "Entropy",
      y = "Re-Clicks",
      colour = "Accuracy",
    ) +
    theme(
      panel.background = element_rect(fill = "white", colour = "white"),
      plot.background = element_rect(fill = "white", colour = "white")
    )
  fnl <- file.path(project_path, "fig", paste(paste(exp, "stereotypy", paste("context", ctx, sep = "-"), sep = "_"), ".png", sep = ""))
  ggsave(fnl, plot = p, width = 14, height = 14)
  
  
  ### transitions
  p <- stereo %>%
    filter(ses == 2, context == ctx) %>%
    ggplot() +
    geom_point(
      aes(
        x = transition_counts,
        y = accuracy,
        colour = factor(subses),
      ),
      alpha = 0.8,
      size = 15
    ) +
    geom_text(
      aes(
        x = transition_counts,
        y = accuracy, label = sub
      ),
      alpha = 0.8,
      size = 8,
      position = position_jitter(width = 0.1, height = 0.05)
    ) +
    theme_minimal(base_size = label_sz, base_family = "Roboto") +
    ylim(0, 1.1) +
    #xlim(0.5, 4.5) +
    labs(
      title = "Transitions by accuracy",
      x = "Transitions",
      y = "Accuracy",
      colour = "Half of session"
    ) +
    scale_colour_brewer(
      palette = "Greens",
      labels = unique(stereo$subses)
    ) +
    theme(
      panel.background = element_rect(fill = "white", colour = "white"),
      plot.background = element_rect(fill = "white", colour = "white")
    )
  fnl <- file.path(project_path, "fig", paste(paste(exp, "transitions", paste("context", ctx, sep = "-"), sep = "_"),".png",sep = ""))
  ggsave(fnl, plot = p, width = 14, height = 7)
}


#===============================================================================
# analyse by test phase context (1 = full transfer, 2 = partial transfer)

# reclicks
p <- stereo_performance %>%
  ggplot() +
  geom_point(
    aes(
      x = reclicks,
      y = accuracy_test,
      colour = factor(context_test)
    ),
    alpha = 0.8,
    size = 15
  ) +
  geom_text(
    aes(
      x = reclicks,
      y = accuracy_test, 
      label = sub
    ),
    alpha = 0.8,
    size = 8,
    position = position_jitter(width = 0.1, height = 0.05)
  ) +
  theme_minimal(base_size = label_sz, base_family = "Roboto") +
  ylim(0.5, 1.1) +
  xlim(0, 8.5) +
  labs(
    title = "Transfer Accuracy by Train Re-Clicks",
    x = "Re-Clicks",
    y = "Accuracy",
    colour = "Test Type"
  ) +
  scale_colour_brewer(
    palette = "Greens",
    labels = c("Full Transfer","Partial Transfer")
  ) +
  theme(
    panel.background = element_rect(fill = "white", colour = "white"),
    plot.background = element_rect(fill = "white", colour = "white")
  )
  
fnl <-
  file.path(project_path, "fig", paste(paste(exp, "reclicks", sep = "_"),".png",sep = ""))
ggsave(fnl, plot = p, width = 14, height = 7)

  
# shortest path overshoot
  p <- stereo_performance %>%
    ggplot() +
    geom_point(aes(x = overshoot_tsp, y = accuracy_test,
                   colour = factor(context_test)),
      alpha = 0.8,
      size = 15
    ) +
    geom_text(
      aes(
        x = overshoot_tsp,
        y = accuracy_test, label = sub
      ),
      alpha = 0.8,
      size = 8,
      position = position_jitter(width = 0.1, height = 0.05)
    ) +
    theme_minimal(base_size = label_sz, base_family = "Roboto") +
    ylim(0.5, 1.1) +
    xlim(-0.1, 2.1) +
    labs(
      title = "Train Overshoot by Test Accuracy",
      x = "Path Overshoot", y = "Accuracy", colour = "Test Type"
    ) +
    scale_colour_brewer(
      palette = "Greens",
      labels = c("Full Transfer","Partial Transfer")
    ) +
    theme(
      panel.background = element_rect(fill = "white", colour = "white"),
      plot.background = element_rect(fill = "white", colour = "white")
    )

  fnl <- file.path(project_path, "fig", paste(paste(exp, "overshoot", sep = "_"), ".png", sep = ""))
  ggsave(fnl, plot = p, width = 14, height = 7)
  
