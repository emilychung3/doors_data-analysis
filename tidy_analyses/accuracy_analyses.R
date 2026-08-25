# E.Chung, 2026
# accuracy analyses
rm(list = ls())

# load libraries 
library(tidyverse)
library(ggplot2) # libraries for plotting data
library(gghalves)
library(afex) # libraries for performing planned contrast analyses
library(emmeans)
afex_options(emmeans_model = "multivariate")

# accuracy data ====================================================================
accuracy <- read.csv("res/all_transfer_data.csv")

# descriptives =======================================================
acc_desc <- accuracy %>%  # descriptives by group x transfer type
  group_by(train_type, transfer) %>% 
  summarise(mean = mean(mean_acc),
            var = var(mean_acc),
            sd = sd(mean_acc))
print(acc_desc)
capture.output(print(acc_desc), 
               file = "res/output/accuracy_desc.txt")

acc_main_desc <- accuracy %>%   # descriptives by transfer type for main effect
  group_by(transfer) %>% 
  summarise(mean = mean(mean_acc),
            var = var(mean_acc),
            sd = sd(mean_acc))
print(acc_main_desc)
capture.output(print(acc_main_desc), 
               file = "res/output/accuracy_desc_main_effect.txt")


# accuracy contrast analyses =========================================================
# fit the data to an ANOVA model
accuracy_anova <- aov_ez('sub', 
                         'mean_acc', 
                         accuracy, 
                         between = 'train_type',
                         within = 'transfer')

# get estimated marginal means
accuracy_emm <- emmeans(accuracy_anova, c("train_type", "transfer"))
print(accuracy_emm)

# specify contrasts and run analyses
# main effect of transfer
novel_vs_part <- c(1, 1, -1, -1, 0, 0)  # within family, (k = 3)
part_vs_comp <- c(0, 0, 1, 1, -1, -1) 
novel_vs_comp <- c(1, 1, 0, 0, -1, -1) 
within_main <- list("novelvs.part" = novel_vs_part,
                    "partvs.comp" = part_vs_comp,
                    "novelvs.comp" = novel_vs_comp)

transfer_main_acc <- contrast(accuracy_emm, within_main, adjust = "bonferroni") # main effect of transfer
print(transfer_main_acc) 
capture.output(print(transfer_main_acc), 
               file = "res/output/accuracy_transfer_main_effect.txt")

transfer_main_CI <- confint(transfer_main_acc, level = 0.95) # and get confidence intervals
print(transfer_main_CI) 
capture.output(print(transfer_main_CI), 
               file = "res/output/accuracy_transfer_main_effect_CI.txt")


## interactions
group_main <- c(1, -1, 1, -1, 1, -1) 
novel_part_int <- group_main*novel_vs_part # two interaction contrasts
part_comp_int <- group_main*part_vs_comp
interactions <- list("groupxnovelvs.part" = novel_part_int, 
                     "groupxpartvs.comp" = part_comp_int)

interactions_acc <- contrast(accuracy_emm, interactions, adjust = "bonferroni")
print(interactions_acc)
capture.output(print(interactions_acc), 
               file = "res/output/accuracy_interactions.txt")

interactions_acc_CI <- confint(interactions_acc, level = 0.95)
print(interactions_acc_CI)
capture.output(print(interactions_acc_CI),
                file = "res/output/accuracy_interactions_CI.txt")

# accuracy contrasts controlling for order  ==================================
accuracy_residuals <- resid(lm(mean_acc ~ order_id, data = accuracy))
accuracy <- cbind(accuracy, accuracy_residuals)

# fit the data to an ANOVA model
acc_resid_anova <- aov_ez('sub',
                          'accuracy_residuals', 
                          accuracy, 
                          between = 'train_type',
                          within = 'transfer')

# get estimated marginal means
acc_resid_emm <- emmeans(acc_resid_anova, c("train_type", "transfer"))
print(acc_resid_emm)

# specify contrasts and run analyses
# transfer main effects
novel_vs_part <- c(1, 1, -1, -1, 0, 0) 
part_vs_comp <- c(0, 0, 1, 1, -1, -1) 
novel_vs_comp <- c(1, 1, 0, 0, -1, -1) 
within_main <- list("novelvs.part" = novel_vs_part,
                    "partvs.comp" = part_vs_comp,
                    "novelvs.comp" = novel_vs_comp)

transfer_main_acc_resid <- contrast(acc_resid_emm, within_main, adjust = "bonferroni")
print(transfer_main_acc_resid) # novel versus comp comparison significant
capture.output(print(transfer_main_acc_resid),
               file = "res/output/accuracyresids_transfer_main_effect.txt")

transfer_main_acc_resid_CI <- confint(transfer_main_acc_resid, level = 0.95)
print(transfer_main_acc_resid)
capture.output(print(transfer_main_acc_resid_CI),
               file = "res/output/accuracyresids_transfer_main_effect_CI.txt")


## interactions 
group_main <- c(1, -1, 1, -1, 1, -1)
novel_part_int <- group_main*novel_vs_part
part_comp_int <- group_main*part_vs_comp
interactions <- list("groupxnovelvs.part" = novel_part_int, 
                     "groupxpartvs.comp" = part_comp_int)

interactions_acc_resid <- contrast(acc_resid_emm, interactions, adjust = "bonferroni")
print(interactions_acc) 
capture.output(print(interactions_acc_resid),
               file = "res/output/accuracyresids_interactions.txt")

# plot acc ====================================================================
# specifying the order that I want my transfer conditions to appear
accuracy <- accuracy %>% 
  mutate(train_type = case_when(train_type == "Stable" ~ "Rare",
                                train_type == "Variable" ~ "Frequent"),
         transfer = case_when(transfer == "Novel" ~ "New",
                              transfer == "Complete" ~ "Complete",
                              transfer == "Partial" ~ "Partial"))

accuracy$transfer <- factor(
  accuracy$transfer,
  levels = c("Complete", "Partial", "New")
)
accuracy$train_type<- factor(
  accuracy$train_type,
  levels = c("Rare", "Frequent")
)

# plotting now
jitter_pos <- position_dodge(width = 0.2) 
accuracy_plt <- ggplot(data = accuracy, 
                       mapping = aes(x = transfer, y = mean_acc, colour = transfer, fill = transfer)) +
  geom_point(aes(group = sub), 
             alpha = 0.3, 
             size = 2,
             position = jitter_pos) +
  geom_line(aes(group = sub), 
            alpha = 0.3, 
            colour = "grey",
            position = jitter_pos) +
  stat_summary(aes(group = transfer),
               fun.data = "mean_sdl", 
               fun.args = list(mult = 1),
               geom = "errorbar", 
               width = 0.1,  
               colour = "grey40") +
  stat_summary(aes(group = transfer),
               fun = mean, geom = "point", shape = 21, size = 2.5, 
               colour = "grey40", fill = "white", stroke = 0.6) +
  labs(x = "Transfer", y = "Accuracy") +
  theme_classic(base_size = 13) +
  scale_fill_manual(values = c("#00bf63", "#ff66c4", "#ff914d")) +
  scale_colour_manual(values = c("#00bf63", "#ff66c4", "#ff914d")) +
  facet_wrap(~train_type) +
  theme(
    legend.position = "none",
    strip.background = element_rect(colour = NA),
    strip.text = element_text(face = "bold", size = 12),
    axis.line.x = element_line(color = "grey80", linewidth = 0.4),
    axis.line.y = element_line(color = "grey80", linewidth = 0.4),
    axis.ticks = element_line(color = "grey40", linewidth = 0.4),
    axis.text = element_text(color = "grey20", size = 12),
    axis.title = element_text(face = 'bold', color = "grey20", size = 12),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)) ,
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(20, 20, 20, 20) 
  )

print(accuracy_plt) 
ggsave("res/plots/accuracy.png", plot = accuracy_plt, 
       width = 6, height = 4)

