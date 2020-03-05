# combining the result figure 3 
rm(list = ls())
loadPlots <- function(){
  panel_a <- source('./src/analysis/ontogenic-wood-density-change/ontogenic-density.graph.R')$value # the triangle
  panel_b <-  source('./src/analysis/nursery-experiment/final-wood-density.graph.R')$value 
  return(list(a = panel_a, b = panel_b))
}


# load the plots 
plots <- loadPlots()

# additional themes
gg_theme <- source("./src/utils/gg-theme.graph.R")$value

library(ggpubr) # package to organise the plots
b <- ggarrange(
  plots$a$plot,
  plots$b$plot,
  # adding the labels 
  labels = c('a', 'b'),
  # styling the labels 
  font.label = list(size = 14, face = "bold", family = gg_theme$font_family) 
)

dims <- (209.9 - (25.4 * 2)) # full width of A4 * 2
b
# save the panel plot for fig 3
ggsave(b, file ='./src/results/figures/graph.fig4.png', width = dims, height = dims / 2, units = "mm")


