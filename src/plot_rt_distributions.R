# lydia barnes, august 2024
# visualises RT distributions per subject and condition

library(tidyverse)
library(ggforce)
library(ggsci)
library(ggpubr)

project_path <- getwd()

# settings
exp <- "exp_ts" # experiment: 'exp_ts' (task-conding) or 'exp_lt' (learning transfer)
ses <- "ses-test" # session: 'ses-learn','ses-train','ses-test'
label_sz <- 10

# data
fnl <- file.path(project_path, "res", paste(paste(exp, "trl", sep = "_"), ".csv", sep = ""))
res <- read.csv(fnl)

# trim high RTs for each condition
#res <- res %>% group_by(sub,ses,switch) %>% filter(rt<(mean(rt)+3*sd(rt)))

pl <- list()
for (subject in unique(res$sub)){
  tmp <- res %>% filter(sub==subject)
  pl[[subject]] <- tmp %>% 
    ggplot() +
    geom_density(aes(x=rt, colour = factor(ses), linetype = factor(switch)), linewidth = 1) + 
    guides(linetype=FALSE) +
    theme_minimal() +
    scale_color_brewer(
      name = "Phase",
      labels = c("Learn","Train","Test")
    ) + 
    labs(title = paste("Subject",subject,sep=" "), x = "RT", y = "Density") +
    theme(
      plot.title = element_text(size = label_sz),
      axis.text.x = element_text(size = label_sz), axis.text.y = element_text(size = label_sz), legend.text = element_text(size = label_sz),
      axis.title.x = element_text(size = label_sz), axis.title.y = element_text(size = label_sz), legend.title = element_text(size = label_sz)
    )
}
ggarrange(plotlist=pl,nrow=25,ncol=4)
fnl <- file.path(project_path, "fig", paste(paste(exp, "rt-distributions", sep = "_"), ".png", sep = ""))
ggsave(fnl, plot = last_plot(), width = 12, height = 45, limitsize = FALSE)
  