# lydia barnes, august 2024 

### sources
library(tidyverse)
library(ggforce)
library(ggsci)
library(ggpubr)

# essentials
project_path <- getwd()

# settings
exp <- "exp_ts" # experiment: 'exp_ts' (task-switching) or 'exp_lt' (learning transfer)
label_sz <- 20
mk_sz <- 2

### plot the data!
fnl <- file.path(project_path, "res", paste(paste(exp, "avg", sep = "_"), ".csv", sep = ""))
res <- read.csv(fnl)
res <- res %>%
  mutate(switch = case_when(switch == 0 ~ "Stay", switch == 1 ~ "Switch")) %>%
  mutate(train_type = as.character(train_type)) %>% 
  mutate(transfer_sequence = case_when(full_transfer_first == 0 ~ "Partial-Full", full_transfer_first == 1 ~ "Full-Partial", .default = NA)) %>% 
  mutate(transfer = case_when(transfer == 1 ~ "Full", transfer == 2 ~ "Partial", .default = NA))

# for train and test phases, group by training type (low / high switch) and trial type (switch / stay)
pl=list()
pl[[1]] <- res %>% filter(ses==3) %>%
  ggplot() +
  
  # show each person's score by training type (low switch/high switch) and trial type (switch/stay)
  geom_violin(aes(x = train_type, y = setting_sticks, colour = switch)) +
  stat_summary(
    aes(x = train_type, y = setting_sticks, color = switch),
    fun.data = "mean_cl_normal",geom = "pointrange", position = position_dodge(width = .9), linewidth = 1, size = mk_sz/2) +
  stat_summary(
    aes(x = train_type, y = setting_sticks, colour = switch),
    fun = "mean", geom = "line", position = position_dodge(width = 0.9), linewidth = 1, alpha = 1
  ) +
  
  # tidy
  theme_minimal() +
  scale_color_lancet(
    name = "Switch",
  ) +
  scale_x_discrete(labels = c("Low Switch", "High Switch")) +
  labs(title = "Start-of-Trial Errors", x = "Training Group", y = "Setting Errors") +
  theme(
    plot.title = element_text(size = label_sz),
    axis.text.x = element_text(size = label_sz), axis.text.y = element_text(size = label_sz), legend.text = element_text(size = label_sz),
    axis.title.x = element_text(size = label_sz), axis.title.y = element_text(size = label_sz), legend.title = element_text(size = label_sz)
  )

pl[[2]] <- res %>% filter(ses==3) %>%
  ggplot() +
  
  # show each person's score by training type (low switch/high switch) and trial type (switch/stay)
  geom_violin(aes(x = train_type, y = setting_slips, colour = switch)) +
  stat_summary(
    aes(x = train_type, y = setting_slips, color = switch),
    fun.data = "mean_cl_normal",geom = "pointrange", position = position_dodge(width = .9), linewidth = 1, size = mk_sz/2) +
  stat_summary(
    aes(x = train_type, y = setting_slips, colour = switch),
    fun = "mean", geom = "line", position = position_dodge(width = 0.9), linewidth = 1, alpha = 1
  ) +
  
  # tidy
  theme_minimal() +
  scale_color_lancet(
    name = "Switch",
  ) +
  scale_x_discrete(labels = c("Low Switch", "High Switch")) +
  labs(title = "Mid-Trial Errors", x = "Training Group", y = "Setting Errors") +
  theme(
    plot.title = element_text(size = label_sz),
    axis.text.x = element_text(size = label_sz), axis.text.y = element_text(size = label_sz), legend.text = element_text(size = label_sz),
    axis.title.x = element_text(size = label_sz), axis.title.y = element_text(size = label_sz), legend.title = element_text(size = label_sz)
  )
ggarrange(plotlist=pl,nrow=1,ncol=2)

fnl <- file.path(project_path, "fig", paste(paste(exp, "sticks-and-slips", sep = "_"), ".pdf",sep = ""))
ggsave(fnl, plot = last_plot(),width = 15, height = 8, limitsize = FALSE)
