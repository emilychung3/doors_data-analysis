# E. Chung, 2025
# This function can be used to create violin and boxplots to show within subject comparisons
# for transfer measures - accuracy, setting_errors and learning onset.

transfer_plots <- function(data){
  jitter_pos <- position_jitterdodge(jitter.width = 0.01) # can't change width?
  
  plot <- ggplot(data = data, 
                  mapping = aes(x = iv, y = dv, colour = iv, fill = iv)) +
    geom_point(aes(group = sid), # 'sub' for acc and 'sid' for maggi
               alpha = 0.3, 
               size = 1.5,
               position = jitter_pos) +
    geom_line(aes(group = sid), # as above
              alpha = 0.3, 
              colour = "grey",
              position = jitter_pos) +
    geom_boxplot(width = 0.2, 
                 alpha = 0.8, 
                 linewidth = 0.3, 
                 colour = 'grey20', 
                outlier.shape = NA) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 2, 
                 colour = "grey20", fill = "grey20") +
    scale_y_continuous(limits = c(0, max(dv)), breaks = seq(0, 0, by = 10)) +
    labs(x = x_axis_lab, y = y_axis_lab) +
    theme_classic(base_size = 16)+
    scale_fill_manual(values = these_cols) +
    scale_colour_manual(values = these_cols) +
    facet_wrap(~train_type) +
    theme(
      legend.position = "none",
      strip.background = element_rect(fill = "#f0e6da", colour = "NA"),
      strip.text = element_text(face = "bold", size = 14),
      axis.line.x = element_line(color = "grey20", linewidth = 0.5),
      axis.line.y = element_line(color = "grey20", linewidth = 0.5),
      axis.ticks = element_line(color = "grey20", linewidth = 0.5),
      axis.text = element_text(color = "grey20", size = 14),
      axis.title = element_text(face = "bold", color = "grey20", size = 16),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10)) 
    )
  
  plot
}