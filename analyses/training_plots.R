# E. Chung, 2025
# This function can be used to create violin and boxplots to show group comparisons
# for training measures - task-jumps, general errors and transition entropy.

training_plots <- function(data){
  plot <- ggplot(data = data, 
                 mapping = aes(x = iv, y = dv, colour = iv, fill = iv)) +
    geom_half_violin(side = "r", scale = "width", 
                     width = 0.6,
                     alpha = 0.3, linewidth = 0.3, 
                     trim = FALSE) +
    geom_half_point(side = "l", alpha = 0.5, size = 1.5,
                    width = 0.6,
                    position = position_jitter(width = 0.05)) +
    geom_boxplot(width = 0.1, alpha = 0.9, linewidth = 0.3, colour = "grey20", 
                 outlier.shape = NA) +
    scale_y_continuous(limits = c(0, max(dv)), breaks = seq(0, 40, by = 10)) +
    scale_fill_manual(values = these_cols) +
    scale_colour_manual(values = these_cols) +
    labs(x = x_axis_lab, y = y_axis_lab) + 
    stat_summary(fun = mean, geom = "point", shape = 23, size = 2, 
                colour = "grey20", fill = "grey20") +
    # facet_wrap(~train_type, labeller = labeller(train_type = c( # no facet wrap for transition entropy plot
    #   "stable" = "Stable",
    #   "variable" = "Variable"
    # ))) +
    theme_classic(base_size = 16) +
    theme(
      legend.position = "none",
      strip.background = element_rect(fill = "#f0e6da", colour = NA),
      strip.text = element_text(face = "bold", size = 14),
      axis.line.x = element_line(color = "grey20", linewidth = 0.5),
      axis.line.y = element_line(color = "grey20", linewidth = 0.5),
      axis.ticks = element_line(color = "grey20", linewidth = 0.5),
      axis.text = element_text(color = "grey20", size = 14),
      axis.title = element_text(face = 'bold', color = "grey20", size = 16),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10)) 
    )
    
 plot   
}

