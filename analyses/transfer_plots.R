# E. Chung, 2025
# This function can be used to create violin and boxplots to show within subject comparisons
# for transfer measures - accuracy, setting_errors and learning onset.

transfer_plots <- function(data){
  plot <- ggplot(data = data, 
                  mapping = aes(x = iv, y = dv, colour = iv, fill = iv)) +
    geom_point(alpha = 0.5, size = 1.5,
               position = position_jitter(width = 0.2)) +
    geom_line(aes(group = sub), alpha = 0.5, colour = "grey") +
    geom_boxplot(width = 0.15, alpha = 0.8, linewidth = 0.4, colour = 'grey20', fill = "grey",
                 outlier.shape = NA) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 2.5) +
    labs(x = x_axis_lab, y = y_axis_lab) +
    theme_classic(base_size = 16)+
    scale_fill_manual(values = these_cols) +
    scale_colour_manual(values = these_cols) +
    facet_wrap(~train_type, 
               labeller = labeller(train_type = c("stable" = "Stable",
                                       "variable" = "Variable"))) +
    theme(
      legend.position = "none",
      strip.background = element_rect(fill = "#D9D9D9", colour = NA),
      strip.text = element_text(face = "bold", size = 14),
      axis.line.x = element_line(color = "grey20", linewidth = 0.5),
      axis.line.y = element_line(color = "grey20", linewidth = 0.5),
      axis.ticks = element_line(color = "grey20", linewidth = 0.5),
      axis.text = element_text(color = "grey20", size = 14),
      axis.title = element_text(color = "grey20", size = 16)
    )
  
  plot
}