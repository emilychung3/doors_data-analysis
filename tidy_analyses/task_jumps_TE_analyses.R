# E.Chung, 2026
rm(list = ls())

# load libraries 
library(here) # setting working directory to the project directory
library(tidyverse)
library(ggplot2) # libraries for plotting data
library(gghalves)
library(gridExtra)
library(afex) # libraries for performing planned contrast analyses
library(emmeans)
afex_options(emmeans_model = "multivariate")

# task jumps ================================================================
task_jumps <- read.csv("rerun_taskjumps_TE/output/task_jumps.csv")
task_jumps <- task_jumps %>% select(-X)
task_jumps$train_type <- as.factor(task_jumps$train_type)
levels(task_jumps$train_type) <- c("Rare", "Frequent")
task_jumps$switch <- as.factor(task_jumps$switch)
levels(task_jumps$switch) <- c("Stay", "Switch")

# now run anova for task_jumps
task_jumps_anova <- aov_ez('sub',
                          'njumps', 
                          task_jumps, 
                          between = 'train_type',
                          within = 'switch')
task_jumps_anova
capture.output(print(task_jumps_anova), file = "output/task_jumps_anova.txt")

# get estimated marginal means
task_jumps_emm <- emmeans(task_jumps_anova, c("train_type", "switch"))
task_jumps_emm

# simple effects
simple_effects <- list('stay_btwn_group' = c(1, -1, 0, 0),
                       'switch_btwn_group' = c(0, 0, 1, -1), 
                       'stable_trial_type' = c(1, 0, -1, 0),
                       'variable_trial_type' = c(0, 1, 0, -1)
)

task_jumps_contrasts <- 
  contrast(task_jumps_emm, simple_effects, adjust = "bonferroni")
capture.output(print(task_jumps_contrasts), 
               file = "output/task_jumps_simple_effects.txt")

# qqplot task jumps ===================================================
# let's check the data for skewness
qqnorm(task_jumps$njumps, col = "blue", pch = 19) # data is severely skewed
qqline(task_jumps$njumps, col = "red", lwd = 2)

# so lets log transform the data
transformed_task_jumps <- task_jumps %>%  
  mutate(log_jumps = log(njumps + 0.000001),
         sqrt_jumps = sqrt(njumps))


# and revisualise
# too many zeros in the data for a log or sqrt transform to be effective.
qqnorm(transformed_task_jumps$sqrt_jumps, col = "blue", pch = 19) 
qqline(transformed_task_jumps$sqrt_jumps, col = "red", lwd = 2)

# lets try remove the outliers for task jumps - DO WE GET THE SAME RESULT?
task_jumps_descriptives <- task_jumps %>%  
  group_by(train_type, switch) %>% 
  summarise(mean = mean(njumps),
            var = var(njumps),
            sd = sd(njumps))
capture.output(print(task_jumps_descriptives,
                     file = "rerun_taskjumps_TE/task_jumps_descriptives.txt"))

outlier_lims_jumps <- task_jumps_descriptives %>% 
  mutate(upper_limit = (mean + 2.5*sd),
         lower_limit = (mean - 2.5*sd))

task_jumps <- left_join(task_jumps, outlier_lims_jumps, by = c("train_type", "switch"))

# outlier summary dat
outliers_summ_jumps <- task_jumps %>% 
  filter(njumps > upper_limit | njumps < lower_limit) 
n_outliers_per_cond_jumps <- outliers_summ_jumps %>% 
  group_by(train_type, switch) %>% 
  summarise(n = n())
outlier_ids_jumps <- outliers_summ_jumps %>% pull(sub) %>% unique()

# now, excluding outliers
task_jumps_post_excl <- task_jumps %>% 
  filter(!sub %in% outlier_ids_jumps)

# now, rerun those analyses - RESULT: same pattern of effects
task_jumps_anova <- aov_ez('sub',
                           'njumps', 
                           task_jumps_post_excl, 
                           between = 'train_type',
                           within = 'switch')
task_jumps_anova
capture.output(print(task_jumps_anova), file = "output/task_jumps_anova.txt")

# get estimated marginal means
task_jumps_emm <- emmeans(task_jumps_anova, c("train_type", "switch"))
task_jumps_emm

# simple effects
simple_effects <- list('stay_btwn_group' = c(1, -1, 0, 0),
                       'switch_btwn_group' = c(0, 0, 1, -1), 
                       'stable_trial_type' = c(1, 0, -1, 0),
                       'variable_trial_type' = c(0, 1, 0, -1)
                       )

task_jumps_contrasts <- contrast(task_jumps_emm, simple_effects, adjust = "bonferroni")

# plot task jumps ==============================================================
task_jumps_plot <- 
  ggplot(data = task_jumps, 
         mapping = aes(x = switch, y = njumps, colour = switch, fill = switch)) +
  geom_half_violin(side = "r", scale = "width", 
                   width = 0.6,
                   alpha = 0.3, linewidth = 0.3, 
                   trim = FALSE) +
  geom_half_point(side = "l", alpha = 0.5, size = 2,
                  width = 0.6,
                  position = position_jitter(width = 0.06)) +
  geom_boxplot(width = 0.1, alpha = 0.8, linewidth = 0.3, colour = "grey20", 
                 outlier.shape = NA) +
  scale_y_continuous(limits = c(0, 3), breaks = seq(0, 3, by = 1)) +
  scale_fill_manual(values = c("#004aad", "#ffbd59")) +
  scale_colour_manual(values = c("#004aad", "#ffbd59")) +
  labs(x = 'Trial Type', y = 'Task Jumps') + 
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
task_jumps_plot
ggsave(filename = 'res/output/plots/task_jumps_plt.png', 
       plot = task_jumps_plot,
       height = 5,
       width = 6)


# weighted transition entropy ==================================================
raw_wTE <- read.csv("rerun_taskjumps_TE/output/exp_lt_weighted_entropy.csv")
raw_wTE_switch <- read.csv("res/exp_lt_weighted_entropy_switch_only.csv")
raw_wTE_stay <- read.csv("res/exp_lt_weighted_entropy_stay_only.csv")

wTE <- raw_wTE %>% 
  group_by(sid, train_type) %>% 
  summarise(TE = mean(TE),
            weightedTE = mean(weightedTE))
  
wTE$train_type <- as.factor(wTE$train_type)
levels(wTE$train_type) <- c("Rare", "Frequent")

wTE_plot <- ggplot(data = wTE, 
                  mapping = aes(x = train_type, y = weightedTE, colour = train_type, fill = train_type)) +
  geom_half_violin(side = "r", scale = "width", 
                   width = 0.4,
                   alpha = 0.3, linewidth = 0.3, 
                   trim = FALSE) +
  geom_half_point(side = "l", alpha = 0.5, size = 2,
                  width = 0.4,
                  position = position_jitter(width = 0.05)) +
  geom_boxplot(width = 0.05, alpha = 0.8, linewidth = 0.3, 
                outlier.shape = NA, colour = "grey20") +
  scale_y_continuous(limits = c(0, 3), breaks = seq(0, 3, by = 1)) +
  scale_fill_manual(values = c("#004aad", "#ffbd59")) +
  scale_colour_manual(values = c("#004aad", "#ffbd59")) +
  labs(x = 'Group', y = 'Weighted TE') + # just use 'TE'
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2.5, 
               colour = "grey20", fill = "white") +
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
    axis.title.y = element_text(margin = margin(r = 10)),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(20, 20, 20, 20)
  )
wTE_plot
ggsave(filename = 'res/output/plots/weightedTE_plt.png', 
       plot = wTE_plot,
       height = 5,
       width = 6)

# get descriptives
wTE_desc <- wTE %>% 
  group_by(train_type) %>% 
  summarise(mean = mean(weightedTE),
            var = var(weightedTE),
            sd = sd(weightedTE))
wTE_desc
capture.output(print(wTE_desc), 
               file = "rerun_taskjumps_TE/output/TE_descriptives.txt")

# now run t test for weightedTE
TE_ttest <- t.test(weightedTE ~ train_type, var.equal = TRUE, data = wTE) # var.equal = TRUE?
TE_ttest
capture.output(print(TE_ttest), 
               file = "rerun_taskjumps_TE/output/TE_ttest.txt")


## qqplots for wTE ===========================================================
qqnorm(wTE$weightedTE, col = "blue", pch = 19) # data is a bit skewed
qqline(wTE$weightedTE, col = "red", lwd = 2)

# so let's transform the data
transformed_wTE <- wTE %>%  
  mutate(log_wTE = log(weightedTE + 0.000001),
         sqrt_wTE = sqrt(weightedTE))

# and revisualise
qqnorm(transformed_wTE$sqrt_wTE, col = "blue", pch = 19) # transformation made it worse
qqline(transformed_wTE$sqrt_wTE, col = "red", lwd = 2)

# let's try run without outliers
outlier_lims_wTE <- wTE_desc %>% 
  mutate(upper_limit = (mean + 2.5*sd),
         lower_limit = (mean - 2.5*sd))

wTE <- left_join(wTE, outlier_lims_wTE, by = c("train_type"))

# outlier summary dat - **no outliers for TE**
outliers_summ_wTE <- wTE %>% 
  filter(weightedTE > upper_limit | weightedTE < lower_limit) 
n_outliers_per_cond_wTE <- outliers_summ_wTE %>% 
  group_by(train_type) %>% 
  summarise(n = n())
outlier_ids_wTE <- outliers_summ_wTE %>% pull(sid) %>% unique()


# running analyses on wTE for switch and stay trials ===========================

wTE_switch <- raw_wTE_switch %>% 
  group_by(sid, train_type) %>% 
  summarise(TE = mean(TE),
            weightedTE = mean(weightedTE)) %>% 
  mutate(trial_type = "Switch")
wTE_switch$train_type <- as.factor(wTE_switch$train_type)
levels(wTE_switch$train_type) <- c("Rare", "Frequent")

wTE_stay <- raw_wTE_stay %>% 
  group_by(sid, train_type) %>% 
  summarise(TE = mean(TE),
            weightedTE = mean(weightedTE)) %>% 
  mutate(trial_type = "Stay")
wTE_stay$train_type <- as.factor(wTE_stay$train_type)
levels(wTE_stay$train_type) <- c("Rare", "Frequent")

wTE_trial_type <- rbind(wTE_switch, wTE_stay) %>% 
  arrange(sid)

# get descriptives
wTE_desc <- wTE_trial_type %>% 
  group_by(train_type, trial_type) %>% 
  summarise(mean = mean(weightedTE),
            var = var(weightedTE),
            sd = sd(weightedTE))
wTE_desc

# now visualise
wTE_trial_type_plot <- 
  ggplot(data = wTE_trial_type, 
         mapping = aes(x = trial_type, y = weightedTE, colour = trial_type, fill = trial_type)) +
  geom_half_violin(side = "r", scale = "width", 
                   width = 0.6,
                   alpha = 0.3, linewidth = 0.3, 
                   trim = FALSE) +
  geom_half_point(side = "l", alpha = 0.5, size = 2,
                  width = 0.6,
                  position = position_jitter(width = 0.06)) +
  geom_boxplot(width = 0.1, alpha = 0.8, linewidth = 0.3, colour = "grey20", 
               outlier.shape = NA) +
  scale_y_continuous(limits = c(0, 3), breaks = seq(0, 3, by = 1)) +
  scale_fill_manual(values = c("#004aad", "#ffbd59")) +
  scale_colour_manual(values = c("#004aad", "#ffbd59")) +
  labs(x = 'Trial Type', y = 'Transition Entropy') + 
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
wTE_trial_type_plot
ggsave(filename = 'res/plots/wTE_by_trialtype_plt.png', 
       plot = wTE_trial_type_plot,
       height = 5,
       width = 6)

# now run interaction
wTE_anova <- aov_ez('sid',
                     'weightedTE', 
                      wTE_trial_type, 
                      between = 'train_type',
                      within = 'trial_type')
wTE_anova
capture.output(print(wTE_anova), file = "res/output/wTE_anova.txt")

# get estimated marginal means
wTE_emm <- emmeans(wTE_anova, c("train_type", "trial_type"))
wTE_emm

# simple effects
simple_effects <- list('rare_vs_freq_switch' = c(1, -1, 0, 0),
                       'rare_vs_freq_stay' = c(0, 0, 1, -1), 
                       'switch_vs_stay_rare' = c(1, 0, -1, 0),
                       'switch_vs_stay_frequent' = c(0, 1, 0, -1)
)

wTE_contrasts <- contrast(wTE_emm, simple_effects, adjust = "bonferroni")
wTE_contrasts
capture.output(print(wTE_contrasts), 
               file = "res/output/wTE_simple_effects.txt")


# regression analyses =========================================================

# regress group * WM scores on TE, task jumps and rule learning ratings--------
# first load the individual differences dataframe
ind_diffs <- read.csv("res/all_individual_differences.csv") # P.S do not use mean_locs_correct from this dataframe

# now, i am going to have to calculate the average number of task jumps for each
# participant across all trials
task_jumps_trl <- read.csv("rerun_taskjumps_TE/output/task_jumps_trl.csv")
task_jumps_trl <- task_jumps_trl %>% select(-X)
task_jumps_trl$train_type <- as.factor(task_jumps_trl$train_type)
levels(task_jumps_trl$train_type) <- c("Stay", "Frequent")
task_jumps_trl$switch <- as.factor(task_jumps_trl$switch)
levels(task_jumps_trl$switch) <- c("Stay", "Switch")

task_jumps_trl <- task_jumps_trl %>% 
  group_by(sub, train_type) %>% 
  summarise(task_jumps = mean(njumps))

# now, i am going to join the individual diffs data with task jumps and TE data
regression_data <- inner_join(ind_diffs, task_jumps_trl, by = c('sub', 'train_type'))

wTE <- wTE %>% rename("sub" = "sid")
regression_data <- inner_join(regression_data, wTE, by = c('sub', 'train_type'))
regression_data <- regression_data %>% 
  select(sub, train_type, max_fwd_corsi, max_bwd_corsi, max_fw_digits, max_bw_digits,
         acq_rule_score, task_jumps, weightedTE)

# filter participants with missing data
regression_data <- regression_data %>% 
  filter(is.finite(max_fwd_corsi) & is.finite(max_bwd_corsi))

# let's see if the variables are correlated
## highest correlation is between task jumps x TE and max_bwd_digits x max_fwd_digits
## these correlations are around 0.5.
corr_mat <- cor(regression_data %>% select(-sub, -train_type))
print(corr_mat) 

# multivariate multiple regression
## only the model with task_jumps came out significant
mlm1 <- lm(cbind(weightedTE, task_jumps) ~ train_type + max_fwd_corsi +
             max_bwd_corsi + max_fw_digits + max_bw_digits, 
           data = regression_data)
summary(mlm1) # RESULT = only the model for Task Jumps is significant
capture.output(summary(mlm1), 
               file = "rerun_taskjumps_TE/output/summary_MR.txt")
Anova(mlm1)
capture.output(Anova(mlm1), 
               file = "rerun_taskjumps_TE/output/multivariateMR.txt")

# follow up analyses of individual predictors in task jumps model
lm_task_jumps <- lm(formula = task_jumps ~ train_type + max_fwd_corsi + max_bwd_corsi + 
                      max_fw_digits + max_bw_digits, data = regression_data)
individual_p <- summary(lm_task_jumps)$coefficients[1, 4]
fdr_adjustedp <- p.adjust(individual_p, method = "fdr") 
fdr_adjustedp # only train type and max_bwd digits is significant predictors


# partial regression plot ======================================================
## this is the amount of variance in task jumps that can't be explained by predictor vars
task_jumps_resid <- resid(lm(formula = task_jumps ~ train_type + max_fwd_corsi + 
                               max_bwd_corsi + max_fw_digits, data = regression_data))
## this is the amount of variance in bwd digit span that cant be explained by the predictor vars
bwd_digitspan_resid <- resid(lm(formula = max_bw_digits ~ train_type + max_fwd_corsi + 
                                  max_bwd_corsi + max_fw_digits, data = regression_data))
cor(bwd_digitspan_resid, task_jumps_resid) # this is the partial correlation 
residuals_dat <- data.frame(task_jumps = task_jumps_resid, bwd_digitspan = bwd_digitspan_resid)

# now, plot
partial_reg_plot <- ggplot(data = residuals_dat,
                           mapping = aes(x = bwd_digitspan, y = task_jumps)) +
  geom_point(colour = "#004aad", alpha = 0.6, size = 2.5) +
  geom_smooth(method = "lm", colour = "#004aad", fill = "#004aad",
              alpha = 0.12, linewidth = 0.8, se = TRUE) +
  labs(x = "Backward Digit Span Residuals",
       y = "Task Jump Residuals") +
  theme_classic(base_size = 13) +
  theme(
    axis.line = element_line(color = "grey80", linewidth = 0.4),
    axis.ticks = element_line(color = "grey80", linewidth = 0.4),
    axis.text = element_text(color = "grey40", size = 10),
    axis.title = element_text(color = "grey20", size = 12, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(20, 20, 20, 20)
  )

print(partial_reg_plot)

ggsave("rerun_taskjumps_TE/output/plots/task_jumps_partialreg.png", 
       plot = partial_reg_plot, 
       height = 5,
       width = 6)

grid.arrange(task_jumps_plot, partial_reg_plot, ncol = 2)

# regressing TE with transfer bias scores ======================================
bias <- read.csv("res/transfer_bias_scores.csv") # contains accuracy bias scores

transfer_data <- read.csv("res/all_transfer_data.csv") # mutating learn onset bias scores
bias_k4 <- transfer_data %>% 
  select(sub, transfer, k4_onset) %>% 
  pivot_wider(names_from = transfer, values_from = k4_onset) %>% 
  mutate('k4bias_ovr_complete' = Partial/(Partial+Complete),
         'k4bias_ovr_novel' = Partial/(Partial+Novel)) %>% 
  select(-Novel, -Complete, -Partial)
write.csv(bias_k4, file = "res/transfer_k4_bias_scores.csv")


# inspect QQ-plots
qq_novel_bias <- ggplot(data = bias_k4, aes(sample = k4bias_ovr_novel)) + 
  geom_qq(colour = 'blue') +
  geom_qq_line(color = "grey")+
  labs(title = "Q-Q plot for bias scores partial/(partial + novel)",
       x = 'theoretical',
       y = 'sample') +
  theme_classic() +
  theme(
    axis.line.x = element_line(color = "grey20", linewidth = 0.5),
    axis.line.y = element_line(color = "grey20", linewidth = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)))
print(qq_novel_bias)

qq_complete_bias <- ggplot(data = bias_k4, aes(sample = k4bias_ovr_complete)) + 
  geom_qq(colour = 'blue') +
  geom_qq_line(color = "grey")+
  labs(title = "Q-Q plot for bias scores partial/(partial + complete)",
       x = 'theoretical',
       y = 'sample') +
  theme_classic() +
  theme(
    axis.line.x = element_line(color = "grey20", linewidth = 0.5),
    axis.line.y = element_line(color = "grey20", linewidth = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)))
print(qq_complete_bias)

# log transform bias accuracy scores (k4 scores are ok)
bias <- bias %>% 
  mutate(log_bias_comp = log(bias_ovr_complete + 0.0001),
         log_bias_novel = log(bias_ovr_novel + 0.0001),
         sqrt_bias_comp = sqrt(bias_ovr_complete),
         sqrt_bias_novel = sqrt(bias_ovr_novel)
         )

# reinspect the data after transformation
qq_novel_bias_transformed <- 
  ggplot(data = bias, aes(sample = sqrt_bias_novel)) + 
  geom_qq(colour = 'blue') +
  geom_qq_line(color = "grey")+
  labs(title = "Q-Q plot for transformed bias scores partial/(partial + novel)",
       x = 'theoretical',
       y = 'sample') +
  theme_classic() +
  theme(
    axis.line.x = element_line(color = "grey20", linewidth = 0.5),
    axis.line.y = element_line(color = "grey20", linewidth = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)))
print(qq_novel_bias_transformed)
# transformations did not help

# TE ~ bias scores 
wTE <- wTE %>% 
  rename("sub" = "sid")
bias <- inner_join(bias, wTE, by = "sub") %>%  
  select(-train_type.y) %>% 
  rename("train_type" = "train_type.x")
bias <- inner_join(bias, bias_k4, by = "sub")

# TE ~ accuracy bias scores
# (n = 96)
lm1 <- lm(cbind(bias_ovr_complete, bias_ovr_novel) ~ weightedTE, data = bias)
summary(lm1)
capture.output(summary(lm1),
               file = "res/output/lm_TE_accuracy_bias_scores.txt")
Anova(lm1)
# TE ~ group + accuracy bias scores
lm2 <- lm(cbind(bias_ovr_complete, bias_ovr_novel) ~ train_type + weightedTE, data = bias)
summary(lm2)
capture.output(summary(lm2),
               file = "res/output/lm_TExgroupaccuracy_bias_scores.txt")
Anova(lm2)

# TE ~ k4 bias scores
# (n = 87)
nonlearners <- read.csv("res/nonlearners.csv")
nonlearners <- unique(nonlearners$sub)
bias_k4_post_exclusion <- bias %>% 
  filter(!sub %in% nonlearners)

lm3 <- lm(cbind(k4bias_ovr_complete, k4bias_ovr_novel) ~ weightedTE, 
          data = bias_k4_post_exclusion)
summary(lm3)
capture.output(summary(lm3),
               file = "res/output/lm_TExk4_bias_scores.txt")
Anova(lm3)
# TE ~ group + k4 bias scores
lm4 <- lm(cbind(bias_ovr_complete, bias_ovr_novel) ~ train_type + weightedTE, 
          data = bias_k4_post_exclusion)
summary(lm4)
capture.output(summary(lm4),
               file = "res/output/lm_TExgroupk4_bias_scores.txt")
Anova(lm4)






