# E.Chung

# here I am plotting the figures for the multiple regression analyses run in 
# res_summary.Rmd
rm(list = ls())
library(tidyverse)
library(ggplot2)

# this is the data
regression_data <- read.csv("res/regression_data.csv")

# For the multiple regression analysis with TE ~ group * WM measures, I am creating 
# a composite score for each individual and plotting it against TE. I will also 
# draw a regression line through it.

# To do this, I need to reproduce the model and get the same coefficients as those
# in res_summary.Rmd.

# I am just changing the Group to a numerical variable so that I am able to include
# it in the composite score
regression_data <- regression_data %>% 
  mutate(train_type = case_when(train_type == "Stable" ~ 1,
                      train_type == "Variable" ~ 2))

composite_score <- regression_data %>% 
  mutate(composite_score = TE_coefs[[1]] + TE_coefs[[2]]*train_type + TE_coefs[[3]]*max_fwd_corsi
+ TE_coefs[[4]]*max_bwd_corsi + TE_coefs[[5]]*max_fw_digits + TE_coefs[[6]]*max_bw_digits) %>% 
  select(sub, composite_score, mean_entropy)

cor_value <- cor(composite_score$composite_score, composite_score$mean_entropy)

composite_score_plot <- 
  ggplot(data = composite_score,
       mapping = aes(composite_score, mean_entropy))+
  geom_point(fill = "#004aad", color = "#004aad") +
  geom_smooth(linewidth = 0.7, linetype = "dashed", color = "grey",
              method = "lm",
              se = FALSE) +
  labs(x = "38.5 + 0.59X1 - 1.80X2 - 1.64X3 + 0.79X4 - 1.31X5",
       y = "Transition Entropy") +
  annotate("text",
           x = 6, y = 30,
           label = paste ("r = ", round(cor_value, 2))
           ) +
  theme_classic() +
  theme(
    legend.position = "none",
    strip.background = element_rect(colour = "NA"),
    strip.text = element_text(margin = margin(b = 10),
                              face = "bold", 
                              size = 11),
    axis.line.x = element_line(color = "grey20", linewidth = 0.5),
    axis.line.y = element_line(color = "grey20", linewidth = 0.5),
    axis.ticks = element_line(color = "grey20", linewidth = 0.5),
    axis.text = element_text(color = "grey20", size = 10),
    axis.title = element_text(face = "bold", color = "grey20", size = 12),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_blank()) 
  

ggsave("res/plots/composite_score_plot.png", plot = composite_score_plot, 
       width = 4, height = 4)
  


# For the multiple regression analysis with log Task Jumps ~ group*WM measures,
# I am creating a model without backwards_digit_span and group, getting the 
# residuals from this model and plotting it against log task jumps.

model <- lm(log_jumps ~ max_fwd_corsi + max_bwd_corsi + max_fw_digits, 
            data = regression_data)
summary(model)
model$residuals

residuals_data <- data.frame(residuals = model$residuals, log_jumps = regression_data$log_jumps)
cor_value <- cor(residuals_data$residuals, residuals_data$log_jumps)

residuals_plot <- 
  ggplot(data = residuals_data,
         mapping = aes(residuals, log_jumps))+
  geom_point(fill = "#004aad", color = "#004aad") +
  geom_smooth(linewidth = 0.7, linetype = "dashed", color = "grey",
              method = "lm",
              se = FALSE) +
  scale_y_continuous(breaks = seq(-5, 2, by = 2)) +
  labs(x = "residuals",
       y = "log(Task Jumps)") +
  annotate("text",
           x = -1, y = -0.5,
           label = paste ("r = ", round(cor_value, 2))
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    strip.background = element_rect(colour = "NA"),
    strip.text = element_text(margin = margin(b = 10),
                              face = "bold", 
                              size = 11),
    axis.line.x = element_line(color = "grey20", linewidth = 0.5),
    axis.line.y = element_line(color = "grey20", linewidth = 0.5),
    axis.ticks = element_line(color = "grey20", linewidth = 0.5),
    axis.text = element_text(color = "grey20", size = 10),
    axis.title = element_text(face = "bold", color = "grey20", size = 12),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
    )
residuals_plot

ggsave("res/plots/residuals_plot.png", plot = residuals_plot, 
       width = 4, height = 4)
