# E. Chung, 2026
# Individual differences analyses
# Here, I am performing the multiple regression analyses to look at the
# the relationship between working memory and performance in the doors task.

rm(list = ls())
library(tidyverse)
library(afex)
library(emmeans)
afex_options(emmeans_model = "multivariate")

# get wrangled data ============================================================
# compute average number of task jumps for each participant, averaged across all
# trials. 
raw_task_jumps <- read.csv("rerun_taskjumps_TE/output/task_jumps_trl.csv")
task_jumps <- raw_task_jumps %>% 
  group_by(sub, train_type) %>% 
  summarise(jumps = mean(njumps))

# compute the wTE, averaged across task contexts
raw_wTE <- read.csv("rerun_taskjumps_TE/output/exp_lt_weighted_entropy.csv")
wTE <- raw_wTE %>% 
  group_by(sid, train_type) %>% 
  summarise(wTE = mean(weightedTE)) %>% 
  rename('sub' = 'sid')

# join task jumps and TE data to working memory data 
working_memory <- read.csv("res/all_individual_differences.csv")
working_memory <- working_memory %>% 
  select(sub, max_fwd_corsi, max_bwd_corsi, max_fw_digits, max_bw_digits, acq_rule_score)

regression_data <- inner_join(working_memory, task_jumps, by = 'sub')
regression_data <- inner_join(regression_data, wTE, by = 'sub') %>% 
  select(-train_type.y) %>% 
  rename('train_type' = 'train_type.x') %>% 
  mutate(train_type = as.factor(
    case_when(train_type == 1 ~ "Rare",
              train_type == 2 ~ "Frequent")
    )
  )

# convert accuracy and learn onset data to wide form and join to working mem data
transfer_data <- read.csv("res/all_transfer_data.csv")
acc_wide <- transfer_data %>% 
  select(sub, train_type, transfer, mean_acc) %>% 
  pivot_wider(names_from = transfer, values_from = mean_acc) %>% 
  rename("novel_acc" = "Novel",
         "comp_acc" = "Complete",
         "partial_acc" = "Partial")
learn_onset_wide <- transfer_data %>% 
  select(sub, train_type, transfer, k4_onset) %>% 
  pivot_wider(names_from = transfer, values_from = k4_onset) %>% 
  rename("novel_k4" = "Novel",
         "comp_k4" = "Complete",
         "partial_k4" = "Partial")

regression_data <- inner_join(regression_data, acc_wide, by = 'sub')
regression_data <- inner_join(regression_data, learn_onset_wide, by = 'sub') %>% 
  select(-train_type.y, - train_type) %>% # just removing duplicate columns
  rename("train_type" = 'train_type.x') %>% 
  filter(is.finite(max_fwd_corsi) & is.finite(max_bwd_corsi))

# multivariate regression ======================================================
# jumps, wTE ~ group and wm measure
# n = 90
mlm1 <- lm(cbind(jumps, wTE) 
  ~ train_type + max_fwd_corsi + max_bwd_corsi + max_fw_digits + max_bw_digits, 
  data = regression_data)
summary(mlm1)
capture.output(summary(mlm1),
               file = ("res/output/mlm_jumps_TE_by_wm.txt"))
Anova(mlm1)
capture.output(Anova(mlm1),
               file = ("res/output/MANOVA_jumps_TE_by_wm.txt"))

# accuracy (novel, complete, partial) ~ group and wm measure
# n = 90
mlm2 <- lm(cbind(novel_acc, 
                 comp_acc, 
                 partial_acc) 
           ~ train_type + max_fwd_corsi + max_bwd_corsi + max_fw_digits + max_bw_digits, 
           data = regression_data)
summary(mlm2)
capture.output(summary(mlm2),
               file = ("res/output/mlm_acc_by_wm.txt"))

Anova(mlm2)
capture.output(Anova(mlm2),
               file = ("res/output/MANOVA_acc_by_wm.txt"))


# learn onset (novel, complete, partial) ~ group and wm measure
# need to exclude those who never learned all four locations in at least one condition
# first, get nonlearners
nonlearners <- read.csv("res/nonlearners.csv") 
nonlearners <- unique(nonlearners$sub)
exclude_nonlearners <- regression_data %>% 
  filter(!sub %in% nonlearners)

# n = 90
mlm3 <- lm(cbind(novel_k4, 
                 comp_k4, 
                 partial_k4) 
           ~ train_type + max_fwd_corsi + max_bwd_corsi + max_fw_digits + max_bw_digits, 
           data = exclude_nonlearners)
summary(mlm3)
capture.output(summary(mlm3),
      file = "res/output/mlm_learn_onset_by_wm.txt")
Anova(mlm3)
capture.output(Anova(mlm3),
               file = ("res/output/MANOVA_learn_onset_by_wm.txt"))


# more multivaritate regression with WM and bias scores ======================
# get accuracy bias scores data
# (n = 90)
bias_acc <- read.csv("res/transfer_bias_scores.csv")
bias_acc <- bias_acc %>% 
  filter(!sub %in% c(7, 37, 38, 76, 88, 93)) # missing WM data from these subs
regression_data <- inner_join(regression_data, bias_acc, by = 'sub')
regression_data <- regression_data %>% 
  select(-train_type.y) %>% 
  rename("train_type" = "train_type.x")

# bias scores ~ group, all WM scores, weightedTE, acq rule scores
mlm4 <- lm(cbind(bias_ovr_novel, bias_ovr_complete) ~
             train_type + max_fwd_corsi + max_bwd_corsi + max_fw_digits + max_bw_digits +
             wTE + acq_rule_score, 
           data = regression_data)
summary(mlm4)
capture.output(summary(mlm4),
               file = "res/output/mlm_accuracybias_scores_WM.txt")
Anova(mlm4)
capture.output(Anova(mlm4),
               file = "res/output/MANOVA_accuracybias_scores_WM.txt")


# get k4 bias scores data
# (n = 81)
bias_k4 <- read.csv("res/transfer_k4_bias_scores.csv")
bias_k4 <- bias_k4 %>% 
  select(-X) %>% 
  filter(!sub %in% c(7, 37, 38, 76, 88, 93)) %>% 
  filter(!sub %in% nonlearners) 
exclude_nonlearners <- inner_join(exclude_nonlearners, bias_k4, by = 'sub')
exclude_nonlearners <- exclude_nonlearners %>% 
  select(-train_type.y) %>% 
  rename("train_type" = "train_type.x")

mlm5 <- lm(cbind(k4bias_ovr_novel, k4bias_ovr_complete) ~
             train_type + max_fwd_corsi + max_bwd_corsi + max_fw_digits + max_bw_digits +
           wTE + acq_rule_score, 
           data = exclude_nonlearners)
summary(mlm5)
capture.output(summary(mlm5),
               file = "res/output/mlm_k4bias_scores_WM.txt")
Anova(mlm5)
capture.output(Anova(mlm5),
               file = "res/output/MANOVA_k4bias_scores_WM.txt")


# exploratory analyses ========================================================
# I am going to compute new bias scores here.
# bias1 = complete - partial/complete + partial (positive value = better complete than partial)
# bias2 = partial - novel/partial + novel (positive value = better partial better than novel)

regression_data <- regression_data %>% 
  mutate(bias1_acc = (comp_acc-partial_acc)/(comp_acc+partial_acc),
         bias2_acc = (partial_acc-novel_acc)/(partial_acc+novel_acc),
         bias1_k4 = (comp_k4-partial_k4)/(comp_k4+partial_k4),
         bias2_k4 = (partial_k4-novel_k4)/(partial_k4+novel_k4)
  )

regression_data_exclude_nonlearners <- regression_data %>% 
  filter(!sub %in% nonlearners)

# now, run analyses
# accuracy bias1, bias2 ~ wTE + Jumps, + Group + WM measure
# n = 90
mlm6 <- lm(cbind(bias1_acc, bias2_acc) ~ wTE + jumps + train_type + max_fwd_corsi
           + max_bwd_corsi + max_fw_digits + max_bw_digits,
           data = regression_data)
summary(mlm6)
capture.output(summary(mlm6),
               file = "output/mlm_ver2_accbias_scores_WM.txt")
anova(mlm6)
capture.output(anova(mlm6),
               file = "res/output/MANOVA_ver2_accbias_scores_WM.txt")


# learn onset bias1, bias2 ~ wTE + Jumps, + Group + WM measure
# n = 81
mlm7 <- lm(cbind(bias1_k4, bias2_k4) ~ wTE + jumps + train_type + max_fwd_corsi
           + max_bwd_corsi + max_fw_digits + max_bw_digits,
           data = regression_data_exclude_nonlearners)
summary(mlm7)
capture.output(summary(mlm7),
               file = "output/mlm_ver2_k4bias_scores_WM.txt")
anova(mlm7)
capture.output(anova(mlm7),
               file = "res/output/MANOVA_ver2_k4bias_scores_WM.txt")
