(function(){
  rm(list = ls())
  # Start with a module... graphic of the change in wood density 
  analysis <- source("./src/analysis/nursery-experiment/final-wood-density.analysis.R")$value
  themed <- source('./src/utils/theme.R')$value
  str(analysis$preds_treat)
  
  # loading ggplot2 
  library(ggplot2)
  
  # model prediction
  pred_line <- geom_line()
  
  # raw data points as box plot 
  raw_points <- geom_boxplot(data = analysis$mode@frame, 
                             aes(x = treatment, 
                                 y = final_wood_density),
                             color = themed$selectBlue(), 
                             width = 0.5)
  
  # confidence intervals 95% n = 5000
  conf_int <-   geom_ribbon(aes(ymin = CI025, ymax = CI975), fill = themed$selectMedGrey()) 
  
  # mean species line 
  base_density_data <- read.csv("./src/data/nursery-experiment/wood-density-nursery-base.raw.csv")
  
  # baseline for the density
  base_density_line <- geom_abline(slope = 0, 
                           intercept = mean(base_density_data, na.rm = T), 
                           linetype = 2, 
                           color = themed$selectRed())
  
  # compose the plot
  p1 <- ggplot(data = analysis$preds_treat, aes(x = treatment, y = final_wood_density)) + 
    # raw_points + 
    conf_int + 
    pred_line + 
    base_density_line +
    xlab("Treatment") + ylab("Final wood density g cm-3") +
    theme_bw() 
  
  p1
  # return the plot
  return(list(plot = p1, analysis = analysis))
})()
  