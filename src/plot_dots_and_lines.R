# lydia barnes, july 2024
# example of how to plot repeated measurements as scatter joined by lines

library(tidyverse)

# get some example data from the "iris" dataset
data(iris)

# re-label the conditions, so we have 2 groups of 2 (flower_segment = "Sepal", "Petal"; orientation = "Width", "Length")
iris <- iris %>% 
  mutate(flower = c(1:nrow(iris))) %>% 
  filter(Species != "setosa") %>% 
  pivot_longer(cols = Sepal.Length:Petal.Width, names_to = "measurement_type",values_to = "mm") %>% 
  mutate(flower_segment = case_when(measurement_type == "Sepal.Width" ~ 1, measurement_type == "Sepal.Length" ~ 1, .default = 0)) %>% 
  mutate(orientation = factor(case_when(measurement_type == "Sepal.Width" ~ 1, measurement_type == "Petal.Width" ~ 1, .default = 0)))
  
# get some summary statistics
res <- iris %>% 
  group_by(measurement_type) %>% 
  summarise(mean = mean(mm),
            sd = sd(mm))

#plot!
ggplot() + 
  geom_line(data = iris, 
            aes(x = flower_segment, y = mm, group = (interaction(flower,orientation))), color = "grey",
  ) +
  geom_point(data = iris, aes(x = flower_segment, y = mm, color = orientation, group = flower_segment),
             alpha = 0.6, size = 2.5, 
             position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0)
  ) +
  geom_errorbar(
    aes(x = c(0,0,1,1), 
        ymin = res$mean - 1.96 * (res$sd / sqrt(length(unique(iris$flower)))), 
        ymax = res$mean + 1.96 * (res$sd / sqrt(length(unique(iris$flower)))), 
        width = 0.1)
  ) + 
  theme_minimal() + 
  labs(title = "Iris Sizes",
       x = "Flower Segment",
       y = "Size (mm)",
       color = "Orientation",
 ) +
  scale_color_manual(values = c("0" = "orange", "1" = "blue"),
                     labels = c("Width", "Length")) +
  scale_x_continuous(breaks = c(0,1),
                     labels = c("Sepal","Petal"))
