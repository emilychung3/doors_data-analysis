# lydia barnes, march 2024 
# generates figures from 'doors' project RT and accuracy outputs

# NB: you will need to filter out switch trials whenever you're not looking at switch effects

### sources
library(tidyverse)
library(ggforce)
library(ggsci)

# essentials
project_path <- getwd()

# settings
exp <- "exp_ts" # experiment: 'exp_ts' (task-switching) or 'exp_lt' (learning transfer)
ses <- "ses-train" # session: 'ses-learn','ses-train','ses-test'
title_sz = 40
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
if (ses == "ses-learn") {
  res <- res %>%
    filter(ses == 1)
} else if (ses == "ses-train") {
  res <- res %>%
    filter(ses == 2)
} else if (ses == "ses-test") {
  res <- res %>%
    filter(ses == 3)
}

if (ses == "ses-learn") {
  
  # for the learn phase, we don't group people by training type (low or high switch) or split their data by trial type (stay or switch)
  # BUT we can view the overall accuracy in each context to check that people acquired both OK
  
  # first, relabel context so it's easier to comprehend
  # drop the few switch trials during the learn phase, as they are likely to be outliers
  res <- res %>%
    mutate(context = case_when(context == 1 ~ "Context A", context == 2 ~ "Context B")) %>%
    filter(switch != "Switch") 

  # pass the data frame to ggplot  
  res %>%
    ggplot() +
    
    # show a horizontal line at chance (.25 as 1 in 4 doors belong to the current context)
    geom_hline(yintercept = 0.25, linetype = "solid", linewidth = 1, alpha = 1, color = "black") +
    
    # create one scatter point per participant per context, jittered so that they are spread around and not on top of each other
    # set scatter point transparency (alpha) and size
    geom_jitter(aes(x = context, y = accuracy), alpha = 0.5, size = mk_sz) +
    
    # show the mean and 95% confidence interval for each context
    stat_summary(
      aes(x = context, y = accuracy),
      fun.data = "mean_cl_normal", geom = "pointrange", linewidth = 2, size = mk_sz / 2,
      alpha = 1
    ) +
    stat_summary(aes(x = context, y = accuracy),
      fun = "mean", geom = "line", linewidth = 1,
      alpha = 1
    ) +
    
    # tidy up
    theme_minimal() +
    scale_x_discrete(labels = c("Context A", "Context B")) +
    labs(
      title = "",
      x = "Context", y = "Accuracy (%)"
    ) +
    theme(
      plot.title = element_text(size = label_sz), 
      axis.text.x = element_text(size = label_sz),
      axis.text.y = element_text(size = label_sz), 
      legend.text = element_text(size = label_sz), 
      axis.title.x = element_text(size = label_sz), 
      axis.title.y = element_text(size = label_sz), 
      legend.title = element_text(size = label_sz)
    )
  
} else {
  
  # for train and test phases, group by training type (low / high switch) and trial type (switch / stay)
  res %>% 
    ggplot() +
    
    # show the chance level
    #geom_hline(yintercept = 0.25, linetype = "solid", linewidth = 1, alpha = 1, color = "black") +
    
    # show each person's score by training type (low switch/high switch) and trial type (switch/stay)
    geom_violin(aes(x = train_type, y = context_changes, color = switch, fill = switch),
                position = position_dodge(width = .7), alpha = .5, linewidth = .4) +
    
    # opt 1: add a boxplot
    geom_boxplot(aes(x = train_type, y = context_changes, fill = switch),
                 position = position_dodge(width = .7), width = .05, linewidth = .7,
                 outlier.alpha = 1,outlier.shape = 21,outlier.size = 2.5,outlier.stroke = NA) +
    
    # opt 2: add a 95% confidence interval and mean
    #stat_summary(aes(x = train_type, y = context_changes, color = switch),
    #             fun.data = "mean_cl_normal",geom = "pointrange", position = position_dodge(width = .7), linewidth = 1, size = mk_sz/2) +
    #stat_summary(aes(x = train_type, y = context_changes, color = switch),
    #             fun = "mean", geom = "line", position = position_dodge(width = .7), linewidth = 1, alpha = 1) +
    
    # tidy
    theme_classic() +
    scale_colour_manual(values = c("#F8CF71","#ABEBC6"),
                        name = "Trial Type",
                        labels = c("Non-Switch","Switch")) +
    scale_fill_manual(values = c("#F8CF71","#ABEBC6"),
                      name = "Trial Type",
                      labels = c("Non-Switch","Switch")) +
    scale_x_discrete(labels = c("Low", "High")) +
    labs(title = "", x = "Training Group", y = "Switch Rate") +
    theme(
      plot.title = element_text(size = label_sz),
      axis.text.x = element_text(size = label_sz), axis.text.y = element_text(size = label_sz), legend.text = element_text(size = label_sz),
      axis.title.x = element_text(size = label_sz), axis.title.y = element_text(size = label_sz), legend.title = element_text(size = label_sz)
    )
}
fnl <- file.path(project_path, "fig", paste(paste(exp, ses, "avg", sep = "_"), ".pdf",
  sep = ""
))
ggsave(fnl, plot = last_plot(), unit = "cm", width = 20, height = 17, limitsize = FALSE)


