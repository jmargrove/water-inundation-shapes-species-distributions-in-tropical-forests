# combining the result figure 3 

loadPlots <- function(){
  panel_a <- source('./src/analysis/elevation-predicted-by-density+inundation/riskratio.graph.R')$value 
  panel_b <- source('./src/analysis/elevation-predicted-by-density+inundation/density.graph.R')$value 
  # panel_c <- source() the triangle
  panel_d <-  source('./src/analysis/nursery-experiment/final-wood-density.graph.R')$value 
  return(list(a = panel_a, b = panel_b, c = NULL, d = panel_d))
}

plots <- loadPlots()

plots$a$plot
plots$b$plot
plots$c$plot
plots$d$plot
