theme_doors <- function(){
  
  title_sz <- 12
  label_sz <- 12
  theme(
    plot.title = element_text(size = title_sz), 
    axis.text.x = element_text(size = label_sz), 
    axis.text.y = element_text(size = label_sz), 
    legend.text = element_text(size = label_sz), 
    axis.title.x = element_text(size = label_sz), 
    axis.title.y = element_text(size = label_sz), 
    legend.title = element_text(size = label_sz),
    strip.text.x = element_text(size = label_sz)
  )
  
}