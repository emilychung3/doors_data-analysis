# lydia barnes, august 2024
# compares four types of accuracy
#   "accuracy": number of current-context clicks / total number of clicks
#   "setting_errors": number of other-context clicks / total number of clicks
#   "win": whether they reached the target in four clicks or fewer
#   "context_changes": how many times they moved between contexts

library(tidyverse)
library(ggforce)
library(ggsci)
library(ggpubr)

# settings
exp <- "exp_ts" # experiment: 'exp_ts' (task-conding) or 'exp_lt' (learning transfer)
label_sz <- 20
mk_sz <- 2

# data
project_path <- getwd()
fnl <- file.path(project_path, "res", paste(paste(exp, "avg", sep = "_"), ".csv", sep = ""))
res <- read.csv(fnl)
res <- res %>% 
  filter(ses==2) %>% 
  select(c(sub,ses,context,switch,accuracy,setting_errors,win,context_changes)) %>% 
  mutate(context_changes = (context_changes-mean(context_changes))/sd(context_changes)) %>% 
  mutate(accuracy = (accuracy-mean(accuracy))/sd(accuracy)) %>% 
  mutate(setting_errors = (setting_errors-mean(setting_errors))/sd(setting_errors)) %>% 
  mutate(win = (win-mean(win))/sd(win)) %>% 
  pivot_longer(!c(sub,ses,context,switch), names_to = "accuracy_type", values_to = "score")

# plot
res %>% 
  ggplot(aes(x = accuracy_type, y = score, colour = factor(switch))) +
  geom_violin() +
  stat_summary(fun.data = "mean_cl_normal",geom = "pointrange", position = position_dodge(width = .9), linewidth = 1, size = mk_sz/2) +
  stat_summary(fun = "mean", geom = "line", position = position_dodge(width = 0.9), linewidth = 1, alpha = 1) +
  theme_minimal() +
  scale_color_lancet(
    name = "Trial Type",
    labels = c("Non-Switch","Switch")) + 
  scale_x_discrete(labels = c("Accuracy", "Context Changes", "Setting Errors", "Win")) +
  labs(title = "", x = "Accuracy Type", y = "Z-Score") +
  theme(
    plot.title = element_text(size = label_sz),
    axis.text.x = element_text(size = label_sz), axis.text.y = element_text(size = label_sz), legend.text = element_text(size = label_sz),
    axis.title.x = element_text(size = label_sz), axis.title.y = element_text(size = label_sz), legend.title = element_text(size = label_sz)
  )

fnl <- file.path(project_path, "fig", paste(paste(exp, "accuracy-types", sep = "_"), ".pdf", sep = ""))
ggsave(fnl, plot = last_plot(), width=12, height=8)
  