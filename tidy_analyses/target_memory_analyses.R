# E. Chung, 2026
# Target memory analyses

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
  select(-mean_locs_correct, -proportion_setErr1, -proportion_setErr2, - proportion_genErr1,
         - proportion_genErr2) %>% 
  mutate(comp_and_part = case_when(comp_and_part == 0 ~ 0,
                                    comp_and_part == 0.5 ~ 1,
                                    comp_and_part == 1.0 ~ 2),
         complete_only = case_when(complete_only == 0 ~ 0,
                                   complete_only == 0.5 ~ 1,
                                   complete_only == 1.0 ~ 2),
         partial_only = case_when(partial_only == 0 ~ 0,
                                   partial_only == 0.5 ~ 1,
                                   partial_only == 1.0 ~ 2),
         never_transferred = case_when(never_transferred == 0 ~ 0,
                                  never_transferred == 0.5 ~ 1,
                                  never_transferred == 1.0 ~ 2))
# pivoting the data into long form
target_memory <- target_memory %>% 
  pivot_longer(cols = c(comp_and_part, complete_only, partial_only, never_transferred),
               names_to = 'transferred_to',
               values_to = 'ncorrect')

# descriptives ================================================================
target_memory_desc <- target_memory %>% 
  group_by(train_type, transferred_to) %>% 
  summarise(mean = mean(ncorrect),
            var = var(ncorrect),
            sd = sd(ncorrect))
print(target_memory_desc)
capture.output(print(target_memory_desc), 
               file = "res/output/target_memory_desc.txt")

# anova =======================================================================
target_memory_anova <- aov_ez('sub',
                              'ncorrect', 
                              target_memory, 
                              between = 'train_type',
                              within = 'transferred_to')
print(target_memory_anova)
capture.output(print(target_memory_anova),
               file = "res/output/target_memory_anova.txt")

# mutating new order conditions ==============================================
transfer_data <- read.csv("res/all_transfer_data.csv")

# here, I am coding whether participants performed the complete condition before
# or after partial transfer in the transfer stage, AND when they performed in 
# complete transfer condition in the transfer stage (i.e., 1st, 2nd or 3rd task). 
# WARNING: this section is hardcoded.
new_order_conditions <- transfer_data %>% 
  select(sub, train_type, order_id) %>% 
  mutate(comp_vs_part_order = case_when(order_id %in% c(2, 3, 4) ~ "Complete before Partial",
                                        order_id %in% c(1, 5, 6)    ~ "Complete after Partial"),
         complete_pos = case_when(order_id %in% c(3, 4) ~ "1st",
                                  order_id %in% c(2, 5) ~ "2nd",
                                  order_id %in% c(1, 6) ~ "3rd")) %>% 
  unique()

target_memory <- inner_join(target_memory, new_order_conditions, by = c('sub', 'train_type'))


# more analyses =================================================================
## comparing participants who completes complete BEFORE partial transfer with those
## who performed complete transfer AFTER partial transfer.
target_memoryxorder_desc <- target_memory %>% 
  group_by(transferred_to, comp_vs_part_order) %>% 
  summarise(mean = mean(ncorrect),
            var = var(ncorrect), 
            sd = sd(ncorrect))
print(target_memoryxorder_desc)
capture.output(print(target_memoryxorder_desc),
               file = "res/output/target_memory_by_order_desc.txt")

target_memoryxorder_anova <- aov_ez('sub',
                                 'ncorrect', 
                                 target_memory, 
                                 between = 'comp_vs_part_order',
                                 within = 'transferred_to')
print(target_memoryxorder_anova) 
capture.output(print(target_memoryxorder_anova),
               file = "res/output/target_memory_by_order_anova.txt")

# checking for a recency effect of performing complete transfer as the 1st, 2nd 
# or 3rd condition in the transfer test on target memory.
# There are some participants who fail to remember the locations of the task 
# transferred to the complete condition. We want to know whether this is due to 
# a recency effect of performing complete transfer 1st, 2nd or 3rd during the 
# transfer test phase. If there is a recency effect, we should see poorer memory
# of the complete task in participants who performed complete transfer first and 
# the best memory in those who performed complete transfer 3rd.

target_memory_recency_desc <- target_memory %>% 
  group_by(train_type, complete_pos) %>% 
  summarise(mean = mean(all_complete),
            var = var(all_complete), 
            sd = sd(all_complete))
print(target_memory_recency_desc)
capture.output(print(target_memory_recency_desc),
               file = "res/output/target_memory_recency_desc.txt")

complete_recency_anova <- aov_ez('sub',
                                 'all_complete', 
                                 target_memory, 
                                 between = c('complete_pos', 'train_type'))
print(complete_recency_anova)
capture.output(print(complete_recency_anova),
               file = "res/output/target_memory_recency_anova.txt")

# plots =====================================================================

# renaming conditions
target_memory <- target_memory %>%
  mutate(transferred_to = case_when(
                                 transferred_to == "comp_and_part" ~ "Both",
                                 transferred_to == "complete_only" ~ "Complete only",
                                 transferred_to == "partial_only" ~ "Partial only",
                                 transferred_to == "never_transferred" ~ "Never"))

target_memory$comp_vs_part_order <- factor(target_memory$comp_vs_part_order,
                                           levels("Complete before Partial", "Complete after Partial"))


target_memory_plt <- 
  ggplot(data = target_memory, 
         mapping = aes(x = transferred_to, 
                       y = ncorrect, 
                       colour = transferred_to, 
                       fill = transferred_to)) +
  geom_point(aes(group = sub),
             alpha = 0.1, 
             size = 2,
             position = position_dodge(width = 0.3)) +
  stat_summary(fun = mean, geom = "line", 
               aes(group = 1), 
               colour = 'grey40', 
               linewidth = 0.4)+
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2.5, 
               colour = "grey40", fill = "white") +
  scale_y_continuous(breaks = seq(0, 2, by = 1)) +
  labs(x = 'Locations by transfer condition', 
       y = 'Number of locations recalled',
       fill = 'Locations by transfer condition',
       colour = 'Locations by transfer condition') +
  theme_classic(base_size = 13) +
  scale_fill_manual(values = c( "#00bf63", "#ff66c4", "#ff914d", "#004aad")) +
  scale_colour_manual(values = c("#00bf63", "#ff66c4", "#ff914d", "#004aad")) +
  facet_wrap(~ train_type) +
  theme(
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 10),
    strip.background = element_rect(colour = NA),
    strip.text = element_text(face = "bold", size = 12),
    axis.line.x = element_line(color = "grey80", linewidth = 0.4),
    axis.line.y = element_line(color = "grey80", linewidth = 0.4),
    axis.ticks = element_line(color = "grey40", linewidth = 0.4),
    axis.text.x = element_blank(),
    axis.text.y = element_text(color = "grey20", size = 12),
    axis.text = element_text(color = "grey20", size = 12),
    axis.title = element_text(face = 'bold', color = "grey20", size = 12),
    axis.title.x = element_blank(),
    axis.title.y = element_text(margin = margin(r = 10)) ,
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(20, 20, 20, 20) 
  )
print(target_memory_plt)
ggsave(here("res/plots/task_memory_by_trf.png"), 
       plot = target_memory_plt, 
       width = 8, height = 4)

# reordering variables
target_memory$comp_vs_part_order <- factor(target_memory$comp_vs_part_order,
                                                levels = c("Complete before Partial", "Complete after Partial"))
transfer_order_plot <- 
  ggplot(data = target_memory, 
         mapping = aes(x = transferred_to, 
                       y = ncorrect, 
                       colour = transferred_to, 
                       fill = transferred_to)) +
  geom_point(aes(group = sub),
             alpha = 0.3, 
             size = 2,
             position = position_dodge(width = 0.3)) +
  stat_summary(fun = mean, geom = "line", 
               aes(group = 1), 
               colour = 'grey40', 
               linewidth = 0.3 )+
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2.5, 
               colour = "grey40", fill = "white") +
  scale_y_continuous(breaks = seq(0, 2, by = 1)) +
  labs(x = "Locations by transfer condition", 
       y = "Number of locations recalled", 
       fill ="Locations by transfer condition", 
       colour = "Locations by transfer condition") +
  theme_classic(base_size = 13) +
  scale_fill_manual(values = c("#00bf63", "#ff66c4", "#ff914d", "#004aad")) +
  scale_colour_manual(values = c("#00bf63", "#ff66c4", "#ff914d", "#004aad")) +
  facet_wrap(~ comp_vs_part_order) +
  theme(
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 10),
    strip.background = element_rect(colour = NA),
    strip.text = element_text(face = "bold", size = 10),
    axis.line.x = element_line(color = "grey80", linewidth = 0.4),
    axis.line.y = element_line(color = "grey80", linewidth = 0.4),
    axis.ticks = element_line(color = "grey40", linewidth = 0.4),
    axis.text.x = element_blank(),
    axis.text.y = element_text(color = "grey20", size = 12),
    axis.text = element_text(color = "grey20", size = 12),
    axis.title = element_text(face = 'bold', color = "grey20", size = 12),
    axis.title.x = element_blank(),
    axis.title.y = element_text(margin = margin(r = 10)) ,
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(20, 20, 20, 20) 
  )
print(transfer_order_plot)

ggsave(here("res/plots/task_memory_by_trf_order.png"), 
       plot = transfer_order_plot, 
       width = 8, 
       height = 3)

