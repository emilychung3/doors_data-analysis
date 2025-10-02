# lydia barnes, august 2024 

### sources
library(tidyverse)
library(ggforce)
library(ggsci)
library(ggpubr)

# essentials
project_path <- getwd()

# settings
exp <- "exp_lt"
label_sz <- 20
mk_sz <- 4

fnl <- file.path(project_path, "res", paste(paste(exp, "maggi-k4", sep = "_"), ".csv", sep = ""))
res <- read.csv(fnl)
res <- res %>% 
  mutate(context = case_when(context==1~1,context==2~2,transfer==1~1,transfer==2~2)) %>% 
  mutate(session_name = case_when(ses==1~"learn",ses==3~"test")) %>% 
  select(!c(X,ses,transfer,nclicks)) %>% 
  pivot_wider(names_from = session_name, values_from = k4_onset) %>% 
  mutate(context_name = case_when(context==1~"A",context==2~"B")) %>% 
  select(!context) %>% 
  pivot_wider(names_from = context_name, values_from = c(learn,test))
res <- res %>% filter(if_all(learn_A:test_B,is.finite))

# scatter plot
pl = list()
pl[[1]] <- res %>% 
  ggplot(aes(x=rowMeans(cbind(learn_A,learn_B)),y=test_A,colour=factor(train_type))) +
  geom_point(size=mk_sz) +
  theme_minimal() +
  scale_colour_brewer(
    palette = "Set1",
    name = "Training Group",
    labels = c("Low Switch","High Switch")
  ) +
  labs(
    title = "Full Transfer", x = "K4 Learn", y = "K4 Test"
  ) +
  theme(
    plot.title = element_text(size = label_sz),
    axis.text.x = element_text(size = label_sz), axis.text.y = element_text(size = label_sz), legend.text = element_text(size = label_sz),
    axis.title.x = element_text(size = label_sz), axis.title.y = element_text(size = label_sz), legend.title = element_text(size = label_sz)
  )
pl[[2]] <- res %>% 
  ggplot(aes(x=rowMeans(cbind(learn_A,learn_B)),y=test_B,colour=factor(train_type))) +
  geom_point(size=mk_sz) +
  theme_minimal() +
  scale_colour_brewer(
    palette = "Set1",
    name = "Training Group",
    labels = c("Low Switch","High Switch")
  ) +
  labs(
    title = "Partial Transfer", x = "K4 Learn", y = "K4 Test"
  ) +
  theme(
    plot.title = element_text(size = label_sz),
    axis.text.x = element_text(size = label_sz), axis.text.y = element_text(size = label_sz), legend.text = element_text(size = label_sz),
    axis.title.x = element_text(size = label_sz), axis.title.y = element_text(size = label_sz), legend.title = element_text(size = label_sz)
  )
ggarrange(plotlist=pl,nrow=1,ncol=2)
fnl <- file.path(project_path, "fig", paste(paste(exp, "k4", sep = "_"), ".pdf",sep = ""))
ggsave(fnl, plot = last_plot(),width=16,height=7)

# check whether people in one training group experienced a bigger improvement from learn to test than the other
res <- res %>% 
  mutate(learn = rowMeans(cbind(learn_A,learn_B))) %>% 
  mutate(change = learn-test_B)
#res <- res %>% pivot_longer(cols = learn_A:test_B, names_to = c("phase","context"), names_sep = "_", values_to = "k4")

res %>% 
  ggplot(aes(x = factor(train_type), y = change)) +
  geom_violin() +
  stat_summary(
    fun.data = "mean_cl_normal",geom = "pointrange", position = position_dodge(width = .9), linewidth = 1, size = mk_sz/2) +
  stat_summary(
    fun = "mean", geom = "line", position = position_dodge(width = 0.9), linewidth = 1, alpha = 1
  ) +
  theme_minimal() +
  scale_x_discrete(
    name = "Training Group",
    labels = c("Low Switch", "High Switch")
    ) +
  labs(
    title = "", x = "Training Group", y = "K4 Onset Change"
    ) +
  theme(
    plot.title = element_text(size = label_sz),
    axis.text.x = element_text(size = label_sz), axis.text.y = element_text(size = label_sz), legend.text = element_text(size = label_sz),
    axis.title.x = element_text(size = label_sz), axis.title.y = element_text(size = label_sz), legend.title = element_text(size = label_sz)
  )

fnl <- file.path(project_path, "fig", paste(paste(exp, "k4-change", sep = "_"), ".pdf",sep = ""))
ggsave(fnl, plot = last_plot(),width=10,height=7)

