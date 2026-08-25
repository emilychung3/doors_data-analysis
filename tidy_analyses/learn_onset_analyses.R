# E. Chung, 2026
# Learning onset analyses
rm(list = ls())

# load libraries 
library(tidyverse)
library(ggplot2) # libraries for plotting data
library(gghalves)
library(afex) # libraries for performing planned contrast analyses
library(emmeans)
afex_options(emmeans_model = "multivariate")

# learning onset =============================================================
learn_onset <- read.csv("res/post_exclusion_transfer_data.csv")

# descriptives =============================================================
learn_onset_desc <- learn_onset %>%  # descriptives by group x transfer type
  group_by(train_type, transfer) %>% 
  summarise(mean = mean(k4_onset),
            var = var(k4_onset),
            sd = sd(k4_onset))
print(learn_onset_desc)
capture.output(print(learn_onset_desc), 
               file = "res/output/learn_onset_desc.txt")

learn_onset_main_desc <- learn_onset %>%   # descriptives by transfer type for main effect
  group_by(transfer) %>% 
  summarise(mean = mean(k4_onset),
            var = var(k4_onset),
            sd = sd(k4_onset))
print(learn_onset_main_desc)
capture.output(print(learn_onset_main_desc), 
               file = "res/output/learn_onset_desc_main_effect.txt")


# contrast analyses =========================================================
# fit the data to an ANOVA model
learn_onset_anova <- aov_ez('sub',
                            'k4_onset', 
                            learn_onset, 
                            between = 'train_type',
                            within = 'transfer')

# get estimated marginal means
learn_onset_emm <- emmeans(learn_onset_anova, c("train_type", "transfer"))
print(learn_onset_emm)

# specify contrasts and run analyses
## transfer main effect
novel_vs_part <- c(1, 1, -1, -1, 0, 0) 
part_vs_comp <- c(0, 0, 1, 1, -1, -1) 
novel_vs_comp <- c(1, 1, 0, 0, -1, -1) 
within_main <- list("novelvs.part" = novel_vs_part,
                    "partvs.comp" = part_vs_comp,
                    "novelvs.comp" = novel_vs_comp)

transfer_main_learn_onset <- contrast(learn_onset_emm, within_main, adjust = "bonferroni")
print(transfer_main_learn_onset)
capture.output(print(transfer_main_learn_onset), 
               file = "res/output/learn_onset_transfer_main_effect.txt")

transfer_main_learn_onset_CI <- confint(transfer_main_learn_onset, level = 0.95)
print(transfer_main_learn_onset_CI)
capture.output(print(transfer_main_learn_onset_CI),
               file = "res/output/learn_onset_transfer_main_effect_CI.txt")

## interaction family, (k = 2)
group_main <- c(1, -1, 1, -1, 1, -1)
novel_part_int <- group_main*novel_vs_part
part_comp_int <- group_main*part_vs_comp
interactions <- list("groupxnovelvs.part" = novel_part_int, 
                     "groupxpartvs.comp" = part_comp_int)

interactions_learn_onset <- contrast(learn_onset_emm, interactions, adjust = "bonferroni")
print(interactions_learn_onset)
capture.output(print(interactions_learn_onset),
               file = "res/output/learn_onset_interactions.txt")

# learn onset contrasts controlling for order ==================================
# regress order onto acc and get residuals
onset_residuals <- resid(lm(k4_onset ~ order_id, data = learn_onset))

# add residuals to subject data 
learn_onset <- cbind(learn_onset, onset_residuals)

# fit the data to an ANOVA model
onset_resid_anova <- aov_ez('sub',
                            'onset_residuals', 
                            learn_onset, 
                            between = 'train_type',
                            within = 'transfer')

# get estimated marginal means
onset_resid_emm <- emmeans(onset_resid_anova, c("train_type", "transfer"))
print(onset_resid_emm)

# specify contrasts and run analyses
## transfer main effects
novel_vs_part <- c(1, 1, -1, -1, 0, 0) 
part_vs_comp <- c(0, 0, 1, 1, -1, -1) 
novel_vs_comp <- c(1, 1, 0, 0, -1, -1) 
within_main <- list("novelvs.part" = novel_vs_part,
                    "partvs.comp" = part_vs_comp,
                    "novelvs.comp" = novel_vs_comp)
transfer_main_learn_onset_resid <- contrast(onset_resid_emm, within_main, adjust = "bonferroni")
print(transfer_main_learn_onset_resid) 
capture.output(print(transfer_main_learn_onset_resid),
               file = "res/output/learn_onsetresids_transfer_main_effect.txt")


## interaction effects
group_main <- c(1, -1, 1, -1, 1, -1)
novel_part_int <- group_main*novel_vs_part
part_comp_int <- group_main*part_vs_comp
interactions <- list("groupxnovelvs.part" = novel_part_int, 
                     "groupxpartvs.comp" = part_comp_int)
interactions_learn_onset_resid <- contrast(onset_resid_emm, interactions, adjust = "bonferroni")
print(interactions_learn_onset_resid) # no significant interactions
capture.output(print(interactions_learn_onset_resid),
               file = "res/output/learn_onsetresids_interactions.txt")


# plot learn onset ============================================================
# specifying the order that I want my transfer conditions to appear
learn_onset <- learn_onset %>% 
  mutate(train_type = case_when(train_type == "Stable" ~ "Rare",
                                train_type == "Variable" ~ "Frequent"),
         transfer = case_when(transfer == "Novel" ~ "New",
                              transfer == "Complete" ~ "Complete",
                              transfer == "Partial" ~ "Partial"))

learn_onset$transfer <- factor(
  learn_onset$transfer,
  levels = c("Complete", "Partial", "New")
)
learn_onset$train_type<- factor(
  learn_onset$train_type,
  levels = c("Rare", "Frequent")
)

# plotting now
jitter_pos <- position_dodge(width = 0.2) 
learn_onset_plt <- ggplot(data = learn_onset, 
                       mapping = aes(x = transfer, y = k4_onset, colour = transfer, fill = transfer)) +
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
  labs(x = "Transfer", y = "Learning Onset") +
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

print(learn_onset_plt) 
ggsave("res/plots/learn_onset.png", plot = learn_onset_plt, 
       width = 6, height = 4)
