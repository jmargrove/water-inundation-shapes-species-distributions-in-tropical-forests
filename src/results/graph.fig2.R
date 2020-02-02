# combining the result figure 2
# rm(list = ls())
(function(){
  loadPlots <- function(){
    panel_a <- source('./src/analysis/inla-spde-species-distributions/species-distribution.graph.R')$value 
    panel_b <- source('./src/analysis/sepilok-field-experiment/mortality-exp.graph.R')$value 
    return(list(a = panel_a, b = panel_b))
  }

  
  # load the plots 
  plots <- loadPlots()
  
  # additional themes
  gg_theme <- source("./src/utils/gg-theme.graph.R")$value
  
  library(ggpubr) # package to organise the plots
  ?ggarrange
  b <- ggarrange(
    plots$a$plot,
    plots$b$plot,
    ncol = 1, 
    nrow = 2, 
    # adding the labels 
    widths = c(1, 1), 
    heights = c(2, 2),
    
    labels = c('a', 'b'),
    # styling the labels 
    font.label = list(size = 14, face = "bold", family = gg_theme$font_family) 
  )
  
  dims <- (209.9 - (25.4 * 2)) # full width of A4 * 2
  b
  return(b)
  # save the panel plot for fig 3
  # ggsave(b, file ='./src/figures/graph.fig2.png', width = dims, height = dims, units = "mm")
})()