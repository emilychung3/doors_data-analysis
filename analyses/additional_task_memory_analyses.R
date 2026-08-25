# E.Chung, 2026
# additional target memory analyses

rm(list = ls())

# load libraries 
library(tidyverse)
library(ggplot2) # libraries for plotting data
library(afex) # libraries for performing planned contrast analyses
library(emmeans)
afex_options(emmeans_model = "multivariate")

# target memory ===============================================================
target_memory <- read.csv("res/task_memory_summary.csv")

target_memory <- target_memory %>% 
  select(sub, train_type, mean_locs_correct) %>% 
  mutate(proportion_correct = mean_locs_correct/4)
mean(target_memory$proportion_correct)

target_memory_stable <- target_memory %>% 
 filter(train_type == "Stable") %>% 
  select(-mean_locs_correct)
target_memory_variable <- target_memory %>% 
  filter(train_type == "Variable") %>% 
  select(-mean_locs_correct)

#plot 
target_memory_plt <- 
  ggplot(data = target_memory, 
       mapping = aes(x = train_type, 
                     y = proportion_correct, 
                     colour = train_type,
                     fill = train_type)) +
  geom_point(alpha = 0.1, 
             size = 2,
             position = position_dodge(width = 0.3)) +

  stat_summary(fun = mean, geom = "point", shape = 21, size = 2.5, 
               colour = "grey40", fill = "white") +
  #scale_y_continuous(breaks = seq(0, 2, by = 1)) +
  labs(x = 'Group', 
       y = 'Proportion locations recalled') +
  theme_classic(base_size = 13) +
  scale_fill_manual(values = c(  "#ff914d", "#004aad")) +
  scale_colour_manual(values = c( "#ff914d", "#004aad")) +
  theme(
    legend.position = "none",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 10),
    strip.background = element_rect(colour = NA),
    strip.text = element_text(face = "bold", size = 12),
    axis.line.x = element_line(color = "grey80", linewidth = 0.4),
    axis.line.y = element_line(color = "grey80", linewidth = 0.4),
    axis.ticks = element_line(color = "grey40", linewidth = 0.4),
    #axis.text.x = element_blank(),
    axis.text.y = element_text(color = "grey20", size = 12),
    axis.text = element_text(color = "grey20", size = 12),
    axis.title = element_text(face = 'bold', color = "grey20", size = 12),
    axis.title.x = element_blank(),
    axis.title.y = element_text(margin = margin(r = 10)) ,
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(20, 20, 20, 20) 
  )
target_memory_plt


target_memory_bar <- 
  ggplot(data = target_memory, 
         mapping = aes(x = train_type, 
                       y = proportion_correct, 
                       fill = train_type)) +
  stat_summary(fun = mean, geom = "col", 
               colour = "grey40", 
               width = 0.6) +
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = 0.2, 
               colour = "grey40") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
  labs(x = 'Group', 
       y = 'Proportion locations recalled') +
  theme_classic(base_size = 13) +
  scale_fill_manual(values = c("#ff914d", "#004aad")) +
  theme(
    legend.position = "none",
    strip.background = element_rect(colour = NA),
    strip.text = element_text(face = "bold", size = 12),
    axis.line.x = element_line(color = "grey80", linewidth = 0.4),
    axis.line.y = element_line(color = "grey80", linewidth = 0.4),
    axis.ticks = element_line(color = "grey40", linewidth = 0.4),
    axis.text = element_text(color = "grey20", size = 12),
    axis.title = element_text(face = 'bold', color = "grey20", size = 12),
    axis.title.x = element_blank(),
    axis.title.y = element_text(margin = margin(r = 10)),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(20, 20, 20, 20)
  )
target_memory_bar

# ttest comparing mean locations recalled with 0.2 i.e., chance performance ====
t.test(target_memory$proportion_correct, mu = 0.2, conf.level = 0.95)

# one-tailed ttest comparing mean against 1 i.e., perfect performance ==========
t.test(target_memory$proportion_correct, mu = 1, alternative = "less", conf.level = 0.95)

# ttest comparing mean locations recalled with 0.2 performance =================
t.test(target_memory_stable$proportion_correct, target_memory_variable$proportion_correct,
       conf.level = 0.95, var.equal = TRUE)


