# E.Chung, 2026
# general_error_analyses

rm(list = ls())

# load libraries
library(tidyverse)
library(ggplot2) # libraries for plotting data
library(gghalves)
library(afex) # libraries for performing planned contrast analyses
library(emmeans)
afex_options(emmeans_model = "multivariate")

# reading general error data ==================================================
general_error <- read.csv("res/training_jumps_gen-error.csv")
general_error <- general_error %>% 
  mutate(train_type = factor(
    case_when(
      train_type == "Stable"   ~ "Rare",
      train_type == "Variable" ~ "Frequent"
    ),
    levels = c("Rare", "Frequent")
  ))

# plotting general error ======================================================
general_error_plt <- 
  ggplot(data = general_error, 
         mapping = aes(x = switch, y = mean_gen_error, colour = switch, fill = switch)) +
  geom_half_violin(side = "r", scale = "width", 
                   width = 0.6,
                   alpha = 0.3, linewidth = 0.3, 
                   trim = FALSE) +
  geom_half_point(side = "l", alpha = 0.5, size = 2,
                  width = 0.6,
                  position = position_jitter(width = 0.06)) +
  geom_boxplot(width = 0.1, alpha = 0.8, linewidth = 0.3, colour = "grey20", 
               outlier.shape = NA) +
  scale_y_continuous(limits = c(0, 0.45), breaks = seq(0, 45, by = 0.1)) +
  scale_fill_manual(values = c("#004aad", "#ffbd59")) +
  scale_colour_manual(values = c("#004aad", "#ffbd59")) +
  labs(x = 'Trial Type', y = 'General Error') + 
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2.5, 
               colour = "grey20", fill = "white") +
  facet_wrap(~train_type) +
  theme_classic(base_size = 13) +
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
print(general_error_plt)   
ggsave(filename = 'res/plots/gen_error_plt.png', 
       plot = general_error_plt,
       height = 4,
       width = 6)

# get general error descriptives ==============================================
descriptives <- general_error %>% 
  group_by(train_type, switch) %>% 
  summarise(mean = mean(mean_gen_error),
            var = var(mean_gen_error),
            sd = sd(mean_gen_error)) %>% 
  ungroup()
capture.output(print(descriptives),
              file = "res/output/gen_error_descriptives.txt")

# general error analyses ======================================================
# fit the data to an ANOVA model
gen_error_anova <- aov_ez('sub',
                          'mean_gen_error', 
                          general_error, 
                          between = 'train_type',
                          within = 'switch')
print(gen_error_anova)
capture.output(print(gen_error_anova),
               file = "res/output/gen_error_anova.txt")

# get estimated marginal means
gen_error_emm <- emmeans(gen_error_anova, c("train_type", "switch"))
print(gen_error_emm)

# perform simple effects contrast analyses
# simple effect contrast to probe difference between groups for each trial type 
# and difference in trial type of each group separately
simple_effects <- list('stay_btwn_group' = c(1, -1, 0, 0),
                       'switch_btwn_group' = c(0, 0, 1, -1), 
                       'stable_trial_type' = c(1, 0, -1, 0),
                       'variable_trial_type' = c(0, 1, 0, -1)
)
gen_error_contrasts <- contrast(gen_error_emm, simple_effects, adjust = "bonferroni")
capture.output(print(gen_error_contrasts),
               file = "res/output/gen_error_contrasts.txt")

# qqplots general errors ======================================================
qqnorm(general_error$mean_gen_error, col = "blue", pch = 19) # data is severely skewed
qqline(general_error$mean_gen_error, col = "red", lwd = 2)

# so lets log transform the data
transformed_gen_error <- general_error %>%  
  mutate(log_error = log(mean_gen_error + 0.000001),
         sqrt_error = sqrt(mean_gen_error))

# and revisualise
# extreme outliers and zeros making the transformation ineffective
# so, let's run analyses without outliers and see if we get the same result
qqnorm(transformed_gen_error$sqrt_error, col = "blue", pch = 19) 
qqline(transformed_gen_error$sqrt_error, col = "red", lwd = 2)

# getting outliers
descriptives <- descriptives %>% 
  mutate(upper_limit = (mean + 2.5*sd))
general_error <- left_join(general_error, descriptives, 
                                        by = c("train_type", "switch"))

outliers_summary <- general_error %>% 
  filter(mean_gen_error > upper_limit)
capture.output(print(outliers_summary), 
               file = "res/output/gen_error_outlier_summary.txt")

n_outliers_per_cond <- outliers_summary %>% 
  group_by(train_type, switch) %>% 
  summarise(n = n())
outlier_ids_gen_err <- outliers_summary %>% pull(sub) %>% unique()

# now, excluding outliers from dataset
gen_error_post_excl <- general_error %>% 
  filter(!sub %in% outlier_ids_gen_err)

# and perform analyses with new dataset again - RESULT: same pattern of results
# fit the data to an ANOVA model
gen_error_anova <- aov_ez('sub',
                          'mean_gen_error', 
                          gen_error_post_excl, 
                          between = 'train_type',
                          within = 'switch')
print(gen_error_anova)
# capture.output(print(gen_error_anova),
#                file = "res/output/gen_error_anova.txt")

# get estimated marginal means
gen_error_emm <- emmeans(gen_error_anova, c("train_type", "switch"))
print(gen_error_emm)

# perform simple effects contrast analyses
# simple effect contrast to probe difference between groups for each trial type 
# and difference in trial type of each group separately
simple_effects <- list('stay_btwn_group' = c(1, -1, 0, 0),
                       'switch_btwn_group' = c(0, 0, 1, -1), 
                       'stable_trial_type' = c(1, 0, -1, 0),
                       'variable_trial_type' = c(0, 1, 0, -1)
)
gen_error_contrasts <- contrast(gen_error_emm, simple_effects, adjust = "bonferroni")
# capture.output(print(gen_error_contrasts),
#                file = "res/output/gen_error_contrasts.txt")

