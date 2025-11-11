# E. Chung, 2025
# This function can be used to create scatter plots to show individual differences 
# data.

scatterplot <- function(data) {
  plot <- ggplot(data, mapping = aes(x = iv, y = dv)) +
    geom_point(color = '#6E00B3', fill = "#6E00B3", 
               alpha = 0.7, 
               size = 2) +
    geom_smooth(method = 'lm', 
                col = 'grey', 
                linetype = 'dashed', 
                linewidth = 0.5,
                se = FALSE) +
    labs(x = x_axis_lab, y = y_axis_lab) + 
    theme_classic() 
  
  plot
}