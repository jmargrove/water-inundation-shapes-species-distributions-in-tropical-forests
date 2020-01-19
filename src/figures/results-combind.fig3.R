# combining the result figure 3 
loadPlots <- function(){
  panel_a <- source('./src/analysis/elevation-predicted-by-density+inundation/riskratio.graph.R')$value 
  panel_b <- source('./src/analysis/elevation-predicted-by-density+inundation/density.graph.R')$value 
  panel_c <- source('./src/analysis/ontogenic-wood-density-change/ontogenic-density.graph.R')$value # the triangle
  panel_d <-  source('./src/analysis/nursery-experiment/final-wood-density.graph.R')$value 
  return(list(a = panel_a, b = panel_b, c = panel_c, d = panel_d))
}



plots <- loadPlots()
library(ggpubr)
b <- ggarrange(
  plots$a$plot,
  plots$b$plot,
  plots$c$plot + 
    theme(text = element_text(size = 20)) ,
  plots$d$plot + 
    theme(text = element_text(size = 20)),
  # adding the labels 
  labels = c('(A)', '(B)', '(C)', '(D)'), 
  # styling the labels 
  font.label = list(size = 24)
)

ggsave(b, file ='./src/figures/results-combind.fig3.png', width = 12, height = 12)

