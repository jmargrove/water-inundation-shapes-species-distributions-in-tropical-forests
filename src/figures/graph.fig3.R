# combining the result figure 3 
rm(list = ls())
loadPlots <- function(){
  panel_a <- source('./src/analysis/elevation-predicted-by-density+inundation/riskratio.graph.R')$value 
  panel_b <- source('./src/analysis/elevation-predicted-by-density+inundation/density.graph.R')$value 
  panel_c <- source('./src/analysis/ontogenic-wood-density-change/ontogenic-density.graph.R')$value # the triangle
  panel_d <-  source('./src/analysis/nursery-experiment/final-wood-density.graph.R')$value 
  return(list(a = panel_a, b = panel_b, c = panel_c, d = panel_d))
}


# load the plots 
plots <- loadPlots()

# additional themes
gg_theme <- source("./src/utils/gg-theme.graph.R")$value

library(ggpubr) # package to organise the plots
b <- ggarrange(
  plots$a$plot,
  plots$b$plot,
  plots$c$plot,
  plots$d$plot,
  # adding the labels 
  labels = c('a', 'b', 'c', 'd'),
  # styling the labels 
  font.label = list(size = 14, face = "bold", family = gg_theme$font_family) 
)

dims <- (209.9 - (25.4 * 2)) # full width of A4 * 2

# save the panel plot for fig 3
ggsave(b, file ='./src/figures/graph.fig3.png', width = dims, height = dims, units = "mm")


