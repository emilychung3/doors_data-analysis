# lydia barnes, september 2024
# plots path data for two example subjects who both have high accuracy but differ in their path consistency

### sources
library(tidyverse)
library(tidyjson)
library(ggpubr)

#=========================================================================================================
# settings (data)
version <- "study-01" 
exp <- "exp_lt" 
sess <- c("ses-train"=2) 
subs <- c("sub-74","sub-73")
algs <- c("hamiltonian") 
contexts <- c(1)

# settings (figures)
title_sz <- 6
label_sz <- 6
ln_sz <- .4

#=========================================================================================================
# read the optimal path data
project_path <- getwd()
fnl <- file.path(project_path, "res", paste(paste(exp, "opt-path", sep = "_"), ".csv",
                                            sep = ""
))
optimal <- read_csv(fnl, show_col_types = FALSE)

# read the observed path data read the data
fnl <- file.path(project_path, "res", paste(paste(exp, "evt", sep = "_"), ".csv", sep = ""))
observed <- read_csv(fnl, show_col_types = FALSE)

# re-code door ID as x and y grid positions
xloc <- c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4)
yloc <- c(4, 3, 2, 1, 4, 3, 2, 1, 4, 3, 2, 1, 4, 3, 2, 1)
id <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)
count <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
doors <- data.frame(xloc, yloc, id, count)
observed <- observed %>%
  mutate(x = xloc[door], y = yloc[door])


# assign path colours for context
context_colour <- c("#79402E") #ISLE OF DOGS


# -------------------------------------------------------------------------
# stereotypy measures from train phase
fnl <-
  file.path(project_path, "res", paste(paste(exp, "stereotypy", sep = "_"), ".csv",
                                       sep = ""
  ))
stereo <- read_csv(fnl, show_col_types = FALSE)
stereo <- stereo %>% group_by(sub,ses,context) %>% summarise_all(mean) %>% ungroup() %>% select(!c(ses))


# -------------------------------------------------------------------------
# plot
for (ss in sess){
  for (ctx in contexts){
    for (alg in algs){
      
      pl <- list()
      i <- 0
      for (sub in subs){
        sid <- as.numeric(substring(sub,5,7))
        i <- i+1
        
        # filter to just this subject, session,  context, and algorithm
        opt <- optimal %>%
          filter(sub == sid, algorithm == alg, context == ctx) %>%
          mutate(solution_factor = factor(solution))
        obs <- observed %>%
          filter(sub == sid, ses == ss, context == ctx, switch == 0)
        
        trials <- unique(obs$t)
        obs <- obs %>% 
          mutate(t_factor = factor(t))
        
        # set grid colours to match
        colours <- rep("lightgrey", 16)
        
        # make the figure
        pl[[sub]] <- ggplot() +
          geom_tile(
            data = doors, aes(x = xloc, y = yloc, fill = id, colour = "white"), show.legend = FALSE,
            width = 0.8, height = 0.8, alpha = 1, col = "lightgrey"
          ) +
          #geom_path(
          #  data = opt, aes(x = x, y = y, group = solution_factor), linewidth = ln_sz, linejoin = "mitre",
          #  lineend = "butt", position = position_jitter(width = 0.1, height = 0.1), 
          #  alpha = 0.8, 
          #  arrow = arrow(angle = 15, type = "closed", length = unit(.1,"cm"))
          #) +
          geom_path(
            data = obs, aes(x = x, y = y, group = t_factor),
            linewidth = ln_sz, linejoin = "mitre", lineend = "butt", position = position_jitter(width = 0.1, height = 0.1),
            alpha = 0.1, 
            arrow = arrow(angle = 15, type = "closed", length = unit(.1,"cm")), 
            col = context_colour[ctx]
          ) +
          theme_minimal() +
          ylim(0.5, 4.5) +
          xlim(0.5,4.5) +
          scale_fill_gradientn(colours = colours, guide = "none") +
          labs(title = sprintf("Subject %d",i)) +
          theme(
            plot.title=element_text(size=title_sz), 
            axis.text.x = element_blank(), axis.text.y = element_blank(), 
            axis.title.x = element_blank(), axis.title.y = element_blank()
          )
      }
      
      ggarrange(plotlist = pl, nrow = 1, ncol = 2)
      
      # save it
      fnl <- file.path(project_path, "fig", paste(paste(exp, names(sess[sess==ss]), "opt-path-example",paste("context", ctx,sep = "-"), sep = "_"), ".png", sep = ""))
      ggsave(fnl, plot = last_plot(), unit = "cm", width = 5, height = 3, limitsize = FALSE)
      
    }    
  }
}



