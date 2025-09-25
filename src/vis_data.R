# lydia barnes, december 2024
# taking essential plotting commands from markdown docs for easy tweaking

library(tidyverse)
library(wesanderson)
source(file.path(getwd(),"src","get_wrangled_data.R"))
source(file.path(getwd(),"src","theme_doors.R"))

exp <- "exp_lt"
project_path <- getwd()
width <- 16 #set this to the manuscript text width (usually 21 cm - 2*2.5 cm margins = 16 cm)

# -------------------------------------------------------------------------
# format the data
data <- get_wrangled_data(project_path, exp)
if(exp=="exp_lt"){
  condition_label <- "Transfer Type"
  condition_levels <- c("Identity","Mixed")
  data <- data %>% rename(condition = transfer)
  data <- left_join(
    data, data %>% 
      select(sub,condition,accuracy, setting_errors, rt, k4) %>% 
      pivot_wider(names_from = condition, values_from = c(accuracy, setting_errors, rt, k4)) %>% 
      mutate(accuracy_difference = accuracy_partial - accuracy_complete,
             setting_errors_difference = setting_errors_partial - setting_errors_complete,
             rt_difference = rt_partial - rt_complete,
             k4_difference = k4_partial - k4_complete), 
    join_by(sub))
}else{
  condition_label <- "Trial Type"
  condition_levels <- c("Stay","Switch")
  data <- data %>% rename(condition = switch)
  data <- left_join(
    data, data %>% 
      select(sub,condition,accuracy, setting_errors, rt) %>% 
      pivot_wider(names_from = condition, values_from = c(accuracy, setting_errors, rt)) %>% 
      mutate(accuracy_difference = accuracy_switch - accuracy_stay,
             setting_errors_difference = setting_errors_switch - setting_errors_stay,
             rt_difference = rt_switch - rt_stay), 
    join_by(sub))
}
log_data <- data %>% 
  mutate(
    accuracy = log(accuracy),
    rt = log(rt),
    setting_errors = log(setting_errors+.0001),
    exploration = log(exploration+.0001)
  )
if(exp=="exp_lt"){
  log_data <- log_data %>% 
    mutate(
      k4 = log(k4),
      perseveration = log(perseveration+.0001))
}else{
    log_data <- log_data %>% 
      mutate(perseveration = log(perseveration))
  }

# -------------------------------------------------------------------------
# how does training group predict test performance?

### accuracy
data %>% 
  ggplot(aes(x = condition, y = accuracy, fill = train_type)) +
  geom_violin(alpha=.5, width = .6) +
  geom_boxplot(position = position_dodge(width = .6), width = .05, linewidth = .7, outlier.alpha = 1,outlier.shape = 21,outlier.size = 2.5,outlier.stroke = NA) +
  scale_fill_manual(values = wes_palette("IsleofDogs1"), labels = c("Stable Context", "Variable Context")) +
  scale_x_discrete(labels = condition_levels) +
  labs(x = condition_label, y = "Accuracy", fill = "Training Group") +
  theme_minimal() +
  theme_doors()
fnl <- file.path(project_path, "fig", paste(exp,"train-type-accuracy.pdf",sep="_"))
ggsave(fnl, plot = last_plot(), unit = "cm", width = width, height = width*.5, limitsize = FALSE)

#guide = "none"
data %>% 
  ggplot(aes(x = train_type, y = accuracy_difference, fill = train_type)) +
  geom_violin(alpha=.5, width = .6) +
  geom_boxplot(position = position_dodge(width = .6), width = .05, linewidth = .7, outlier.alpha = 1,outlier.shape = 21,outlier.size = 2.5,outlier.stroke = NA) +
  scale_fill_manual(values = wes_palette("IsleofDogs1"), labels = c("Stable Context", "Variable Context")) +
  scale_x_discrete(labels = c("Stable Context", "Variable Context")) +
  labs(x = "Training Group", y = "Accuracy Difference", fill = "Training Group") +
  theme_minimal() +
  theme_doors()
fnl <- file.path(project_path, "fig", paste(exp,"train-type-accuracy-difference.pdf",sep="_"))
ggsave(fnl, plot = last_plot(), unit = "cm", width = width, height = width*.5, limitsize = FALSE)

### setting errors
data %>% 
  ggplot(aes(x = condition, y = setting_errors, fill = train_type)) +
  geom_violin(alpha=.5, width = .6) +
  geom_boxplot(position = position_dodge(width = .6), width = .05, linewidth = .7, outlier.alpha = 1,outlier.shape = 21,outlier.size = 2.5,outlier.stroke = NA) +
  scale_fill_manual(values = wes_palette("IsleofDogs1"), labels = c("Stable Context", "Variable Context")) +
  scale_x_discrete(labels = condition_levels) +
  labs(x = condition_label, y = "Setting Errors", fill = "Training Group") +
  theme_minimal() +
  theme_doors()
fnl <- file.path(project_path, "fig", paste(exp,"train-type-setting-errors.pdf",sep="_"))
ggsave(fnl, plot = last_plot(), unit = "cm", width = width, height = width*.5, limitsize = FALSE)

data %>% 
  ggplot(aes(x = train_type, y = setting_errors_difference, fill = train_type)) +
  geom_violin(alpha=.5, width = .6) +
  geom_boxplot(position = position_dodge(width = .6), width = .05, linewidth = .7, outlier.alpha = 1,outlier.shape = 21,outlier.size = 2.5,outlier.stroke = NA) +
  scale_fill_manual(values = wes_palette("IsleofDogs1"), labels = c("Stable Context", "Variable Context")) +
  scale_x_discrete(labels = c("Stable Context", "Variable Context")) +
  labs(x = "Training Group", y = "Setting Errors Difference", fill = "Training Group") +
  theme_minimal() +
  theme_doors()
fnl <- file.path(project_path, "fig", paste(exp,"train-type-setting-errors-difference.pdf",sep="_"))
ggsave(fnl, plot = last_plot(), unit = "cm", width = width, height = width*.5, limitsize = FALSE)

### rt
data %>% 
  ggplot(aes(x = condition, y = rt, fill = train_type)) +
  geom_violin(alpha=.5, width = .6) +
  geom_boxplot(position = position_dodge(width = .6), width = .05, linewidth = .7, outlier.alpha = 1,outlier.shape = 21,outlier.size = 2.5,outlier.stroke = NA) +
  scale_fill_manual(values = wes_palette("IsleofDogs1"), labels = c("Stable Context", "Variable Context")) +
  scale_x_discrete(labels = condition_levels) +
  labs(x = condition_label, y = "Response Time", fill = "Training Group") +
  theme_minimal() +
  theme_doors()
fnl <- file.path(project_path, "fig", paste(exp,"train-type-rt.pdf",sep="_"))
ggsave(fnl, plot = last_plot(), unit = "cm", width = width, height = width*.5, limitsize = FALSE)

data %>% 
  ggplot(aes(x = train_type, y = rt_difference, fill = train_type)) +
  geom_violin(alpha=.5, width = .6) +
  geom_boxplot(position = position_dodge(width = .6), width = .05, linewidth = .7, outlier.alpha = 1,outlier.shape = 21,outlier.size = 2.5,outlier.stroke = NA) +
  scale_fill_manual(values = wes_palette("IsleofDogs1"), labels = c("Stable Context", "Variable Context")) +
  scale_x_discrete(labels = c("Stable Context", "Variable Context")) +
  labs(x = "Training Group", y = "Response Time Difference", fill = "Training Group") +
  theme_minimal() +
  theme_doors()
fnl <- file.path(project_path, "fig", paste(exp,"train-type-rt-difference.pdf",sep="_"))
ggsave(fnl, plot = last_plot(), unit = "cm", width = width, height = width*.5, limitsize = FALSE)


### k4
if(exp=="exp_lt"){
  data %>% 
    ggplot(aes(x = condition, y = k4, fill = train_type)) +
    geom_violin(alpha=.5, width = .6) +
    geom_boxplot(position = position_dodge(width = .6), width = .05, linewidth = .7, outlier.alpha = 1,outlier.shape = 21,outlier.size = 2.5,outlier.stroke = NA) +
    scale_fill_manual(values = wes_palette("IsleofDogs1"), labels = c("Stable Context", "Variable Context")) +
    scale_x_discrete(labels = condition_levels) +
    labs(x = condition_label, y = "Learning Onset", fill = "Training Group") +
    theme_minimal() +
    theme_doors()
  fnl <- file.path(project_path, "fig", paste(exp,"train-type-k4.pdf",sep="_"))
  ggsave(fnl, plot = last_plot(), unit = "cm", width = width, height = width*.5, limitsize = FALSE)
  
  data %>% 
    ggplot(aes(x = train_type, y = k4_difference, fill = train_type)) +
    geom_violin(alpha=.5, width = .6) +
    geom_boxplot(position = position_dodge(width = .6), width = .05, linewidth = .7, outlier.alpha = 1,outlier.shape = 21,outlier.size = 2.5,outlier.stroke = NA) +
    scale_fill_manual(values = wes_palette("IsleofDogs1"), labels = c("Stable Context", "Variable Context")) +
    scale_x_discrete(labels = c("Stable Context", "Variable Context")) +
    labs(x = "Training Group", y = "Learning Onset Difference", fill = "Training Group") +
    theme_minimal() +
    theme_doors()
  fnl <- file.path(project_path, "fig", paste(exp,"train-type-k4-difference.pdf",sep="_"))
  ggsave(fnl, plot = last_plot(), unit = "cm", width = width, height = width*.5, limitsize = FALSE)
}

# -------------------------------------------------------------------------
# how does training group affect exploration and perseveration?
data %>% 
  ggplot(aes(x = train_type, y = exploration, fill = train_type)) +
  geom_violin(alpha=.5, width = .6) +
  geom_boxplot(position = position_dodge(width = .6), width = .02, linewidth = .7, outlier.alpha = 1,outlier.shape = 21,outlier.size = 2.5,outlier.stroke = NA) +
  scale_fill_manual(values = wes_palette("IsleofDogs1"), labels = c("Stable Context", "Variable Context")) +
  labs(x = "", y = "Exploration", fill = "Training Group") +
  guides(x = "none") +
  theme_minimal() +
  theme_doors()
fnl <- file.path(project_path, "fig", paste(exp,"train-type-exploration.pdf",sep="_"))
ggsave(fnl, plot = last_plot(), unit = "cm", width = width, height = width*.5, limitsize = FALSE)

data %>% 
  ggplot(aes(x = train_type, y = perseveration, fill = train_type)) +
  geom_violin(alpha=.5, width = .6) +
  geom_boxplot(position = position_dodge(width = .6), width = .02, linewidth = .7, outlier.alpha = 1,outlier.shape = 21,outlier.size = 2.5,outlier.stroke = NA) +
  scale_fill_manual(values = wes_palette("IsleofDogs1"), labels = c("Stable Context", "Variable Context")) +
  labs(x = "", y = "Perseveration", fill = "Training Group") +
  guides(x = "none") +
  theme_minimal() +
  theme_doors()
fnl <- file.path(project_path, "fig", paste(exp,"train-type-perseveration.pdf",sep="_"))
ggsave(fnl, plot = last_plot(), unit = "cm", width = width, height = width*.5, limitsize = FALSE)

# -------------------------------------------------------------------------
# how are perseveration and exploration distributed?
data %>% 
  ggplot(aes(x = perseveration)) +
  geom_density(alpha = .7, fill = "#9986A5") +
  theme_minimal() +
  theme_doors() +
  labs(x = "Perseveration", y = "Density")
fnl <- file.path(project_path, "fig", paste(exp,"perseveration.pdf",sep="_"))
ggsave(fnl, plot = last_plot(), unit = "cm", width = width*.5, height = width*.5, limitsize = FALSE)

data %>% 
  ggplot(aes(x = exploration)) +
  geom_density(alpha = .7, fill = "#9986A5") +
  theme_minimal() +
  labs(x = "Exploration", y = "Density") +
  theme_doors()
fnl <- file.path(project_path, "fig", paste(exp,"exploration.pdf",sep="_"))
ggsave(fnl, plot = last_plot(), unit = "cm", width = width*.5, height = width*.5, limitsize = FALSE)


# -------------------------------------------------------------------------
# how do perseveration and exploration relate to test performance?
### accuracy
log_data %>% 
  ggplot(aes(x = perseveration, y = accuracy, colour = condition, fill = condition)) +
  geom_point(colour="black", size = 2.5, alpha = .7, shape=21, stroke = 1) +
  geom_smooth(method="lm", se=TRUE) +
  scale_colour_manual(values = wes_palette("IsleofDogs1"), labels = condition_levels) +
  scale_fill_manual(values = wes_palette("IsleofDogs1"), labels = condition_levels) +
  labs(x = "Perseveration", y = "Accuracy", fill = condition_label, colour = condition_label) +
  guides(fill="none",colour="none") +
  theme_minimal() +
  theme_doors()
fnl <- file.path(project_path, "fig", paste(exp,"perseveration-accuracy.pdf",sep="_"))
ggsave(fnl, plot = last_plot(), unit = "cm", width = width*.5, height = width*.5, limitsize = FALSE)

log_data %>% 
  ggplot(aes(x = exploration, y = accuracy, colour = condition, fill = condition)) +
  geom_point(colour="black", size = 2.5, alpha = .7, shape=21, stroke = 1) +
  geom_smooth(method="lm", se=TRUE) +
  scale_colour_manual(values = wes_palette("IsleofDogs1"), labels = condition_levels) +
  scale_fill_manual(values = wes_palette("IsleofDogs1"), labels = condition_levels) +
  labs(x = "Exploration", y = "Accuracy", fill = condition_label, colour = condition_label) +
  guides(fill="none",colour="none") +
  theme_minimal() +
  theme_doors()
fnl <- file.path(project_path, "fig", paste(exp,"exploration-accuracy.pdf",sep="_"))
ggsave(fnl, plot = last_plot(), unit = "cm", width = width*.5, height = width*.5, limitsize = FALSE)

### setting errors
log_data %>% 
  ggplot(aes(x = perseveration, y = setting_errors, colour = condition, fill = condition)) +
  geom_point(colour="black", size = 2.5, alpha = .7, shape=21, stroke = 1) +
  geom_smooth(method="lm", se=TRUE) +
  scale_colour_manual(values = wes_palette("IsleofDogs1"), labels = condition_levels) +
  scale_fill_manual(values = wes_palette("IsleofDogs1"), labels = condition_levels) +
  labs(x = "Perseveration", y = "Setting Errors", fill = condition_label, colour = condition_label) +
  guides(fill="none",colour="none") +
  theme_minimal() +
  theme_doors()
fnl <- file.path(project_path, "fig", paste(exp,"perseveration-setting-errors.pdf",sep="_"))
ggsave(fnl, plot = last_plot(), unit = "cm", width = width*.5, height = width*.5, limitsize = FALSE)

log_data %>% 
  ggplot(aes(x = exploration, y = setting_errors, colour = condition, fill = condition)) +
  geom_point(colour="black", size = 2.5, alpha = .7, shape=21, stroke = 1) +
  geom_smooth(method="lm", se=TRUE) +
  scale_colour_manual(values = wes_palette("IsleofDogs1"), labels = condition_levels) +
  scale_fill_manual(values = wes_palette("IsleofDogs1"), labels = condition_levels) +
  labs(x = "Exploration", y = "Setting Errors", fill = condition_label, colour = condition_label) +
  guides(fill="none",colour="none") +
  theme_minimal() +
  theme_doors()
fnl <- file.path(project_path, "fig", paste(exp,"exploration-setting-errors.pdf",sep="_"))
ggsave(fnl, plot = last_plot(), unit = "cm", width = width*.5, height = width*.5, limitsize = FALSE)

### rt
log_data %>% 
  ggplot(aes(x = perseveration, y = rt, colour = condition, fill = condition)) +
  geom_point(colour="black", size = 2.5, alpha = .7, shape=21, stroke = 1) +
  geom_smooth(method="lm", se=TRUE) +
  scale_colour_manual(values = wes_palette("IsleofDogs1"), labels = condition_levels) +
  scale_fill_manual(values = wes_palette("IsleofDogs1"), labels = condition_levels) +
  theme_minimal() +
  labs(x = "Perseveration", y = "Response Time", fill = condition_label, colour = condition_label) +
  theme_doors()
fnl <- file.path(project_path, "fig", paste(exp,"perseveration-rt.pdf",sep="_"))
ggsave(fnl, plot = last_plot(), unit = "cm", width = width*.68, height = width*.5, limitsize = FALSE)

log_data %>% 
  ggplot(aes(x = exploration, y = rt, colour = condition, fill = condition)) +
  geom_point(colour="black", size = 2.5, alpha = .7, shape=21, stroke = 1) +
  geom_smooth(method="lm", se=TRUE) +
  scale_colour_manual(values = wes_palette("IsleofDogs1"), labels = condition_levels) +
  scale_fill_manual(values = wes_palette("IsleofDogs1"), labels = condition_levels) +
  theme_minimal() +
  labs(x = "Exploration", y = "Response Time", fill = condition_label, colour = condition_label) +
  theme_doors()
fnl <- file.path(project_path, "fig", paste(exp,"exploration-rt.pdf",sep="_"))
ggsave(fnl, plot = last_plot(), unit = "cm", width = width*.68, height = width*.5, limitsize = FALSE)

### k4
if(exp=="exp_lt"){
  log_data %>% 
    ggplot(aes(x = perseveration, y = k4, colour = condition, fill = condition)) +
    geom_point(colour="black", size = 2.5, alpha = .7, shape=21, stroke = 1) +
    geom_smooth(method="lm", se=TRUE) +
    scale_colour_manual(values = wes_palette("IsleofDogs1"), labels = condition_levels) +
    scale_fill_manual(values = wes_palette("IsleofDogs1"), labels = condition_levels) +
    labs(x = "Perseveration", y = "Learning Onset", fill = condition_label, colour = condition_label) +
    theme_minimal() +
    theme_doors()
  fnl <- file.path(project_path, "fig", paste(exp,"perseveration-k4.pdf",sep="_"))
  ggsave(fnl, plot = last_plot(), unit = "cm", width = width*.68, height = width*.5, limitsize = FALSE)
  
  log_data %>% 
    ggplot(aes(x = exploration, y = k4, colour = condition, fill = condition)) +
    geom_point(colour="black", size = 2.5, alpha = .7, shape=21, stroke = 1) +
    geom_smooth(method="lm", se=TRUE) +
    scale_colour_manual(values = wes_palette("IsleofDogs1"), labels = condition_levels) +
    scale_fill_manual(values = wes_palette("IsleofDogs1"), labels = condition_levels) +
    labs(x = "Exploration", y = "Learning Onset", fill = condition_label, colour = condition_label) +
    theme_minimal() +
    theme_doors()
  fnl <- file.path(project_path, "fig", paste(exp,"exploration-k4.pdf",sep="_"))
  ggsave(fnl, plot = last_plot(), unit = "cm", width = width*.68, height = width*.5, limitsize = FALSE)
  
}
