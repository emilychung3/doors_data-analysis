# lydia barnes, august 2024
# views k4 onset in learn and test phase

library(tidyverse)
library(ggforce)
library(ggsci)
library(ggpubr)

project_path <- getwd()

# settings
exp <- "exp_lt" # experiment: 'exp_ts' (task-conding) or 'exp_lt' (learning transfer)
label_sz <- 10

# data
fnl <- file.path(project_path, "res", paste(paste(exp, "maggi-k4", sep = "_"), ".csv", sep = ""))
res <- read.csv(fnl)
res <- res %>% mutate(context = case_when(context < 3 ~ context, context==3~1, context==4~2))

pl <- list()
pl[[1]] <- res %>% filter(ses==1) %>% 
  ggplot() +
  geom_density(aes(x=k4_onset, colour = factor(context)), linewidth = 1) + 
  guides(linetype=FALSE) +
  theme_minimal() +
  scale_color_brewer(
    name = "Context",
    labels = c("1","2")
  ) + 
  labs(title = "Learn Phase", x = "K4 Onset", y = "Density") +
  theme(
    plot.title = element_text(size = label_sz),
    axis.text.x = element_text(size = label_sz), axis.text.y = element_text(size = label_sz), legend.text = element_text(size = label_sz),
    axis.title.x = element_text(size = label_sz), axis.title.y = element_text(size = label_sz), legend.title = element_text(size = label_sz)
  )
pl[[2]] <- res %>% filter(ses==3) %>% 
  ggplot() +
  geom_density(aes(x=k4_onset, colour = factor(transfer)), linewidth = 1) + 
  guides(linetype=FALSE) +
  theme_minimal() +
  scale_color_brewer(
    name = "Transfer",
    labels = c("Full","Partial")
  ) + 
  labs(title = "Test Phase", x = "K4 Onset", y = "Density") +
  theme(
    plot.title = element_text(size = label_sz),
    axis.text.x = element_text(size = label_sz), axis.text.y = element_text(size = label_sz), legend.text = element_text(size = label_sz),
    axis.title.x = element_text(size = label_sz), axis.title.y = element_text(size = label_sz), legend.title = element_text(size = label_sz)
  )

ggarrange(plotlist=pl,nrow=1,ncol=2)
fnl <- file.path(project_path, "fig", paste(paste(exp, "k4-distributions", sep = "_"), ".pdf", sep = ""))
ggsave(fnl, plot = last_plot(), width = 15, height = 8, limitsize = FALSE)
