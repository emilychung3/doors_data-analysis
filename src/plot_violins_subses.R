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
subses <- c(1,2)
title_sz = 40
label_sz <- 20
mk_sz <- 2

### plot the data!
fnl <- file.path(project_path, "res", paste(paste(exp, "avg-ss", sep = "_"), ".csv", sep = ""))
res <- read.csv(fnl)
res <- res %>%
  mutate(switch = case_when(switch == 0 ~ "Stay", switch == 1 ~ "Switch")) %>%
  mutate(train_type = as.character(train_type)) %>% 
  mutate(transfer_sequence = case_when(full_transfer_first == 0 ~ "Partial-Full", full_transfer_first == 1 ~ "Full-Partial", .default = NA)) %>% 
  mutate(transfer = case_when(transfer == 1 ~ "Full", transfer == 2 ~ "Partial", .default = NA))
if (ses == "ses-train") {
  res <- res %>%
    filter(ses == 2)
} else if (ses == "ses-test") {
  res <- res %>%
    filter(ses == 3)
}
  
# for train and test phases, group by training type (low / high switch) and trial type (switch / stay)
pl <- list()
for (ss in subses){
pl[[ss]] <- res %>% 
  ggplot() +
  # show each person's score by training type (low switch/high switch) and trial type (switch/stay)
  geom_violin(aes(x = train_type, y = context_changes, color = switch, fill = switch),
              position = position_dodge(width = .7), alpha = .5, linewidth = .4) +
  # add a boxplot
  geom_boxplot(aes(x = train_type, y = context_changes, fill = switch),
               position = position_dodge(width = .7), width = .05, linewidth = .7,
               outlier.alpha = 1,outlier.shape = 21,outlier.size = 2.5,outlier.stroke = NA) +
  # tidy
  theme_classic() +
  scale_colour_manual(values = c("#F8CF71","#ABEBC6"),
                      name = "Trial Type",
                      labels = c("Non-Switch","Switch")) +
  scale_fill_manual(values = c("#F8CF71","#ABEBC6"),
                    name = "Trial Type",
                    labels = c("Non-Switch","Switch")) +
  scale_x_discrete(labels = c("Low", "High")) +
  labs(title = sprintf("Sub-Session %d",ss), x = "Training Group", y = "Switch Rate") +
  theme(
    plot.title = element_text(size = label_sz),
    axis.text.x = element_text(size = label_sz), axis.text.y = element_text(size = label_sz), legend.text = element_text(size = label_sz),
    axis.title.x = element_text(size = label_sz), axis.title.y = element_text(size = label_sz), legend.title = element_text(size = label_sz)
  )
}
ggarrange(plotlist = pl,ncol=2,nrow=1)
fnl <- file.path(project_path, "fig", paste(paste(exp, ses, "avg-ss", sep = "_"), ".pdf",sep = ""))
ggsave(fnl, plot = last_plot(), unit = "cm", width = 40, height = 17, limitsize = FALSE)


