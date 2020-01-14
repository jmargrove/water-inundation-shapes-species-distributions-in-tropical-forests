# combining the result figure 3 

panel_a <- source('./src/analysis/elevation-predicted-by-density+inundation/riskratio.graph.R')$value 
panel_b <- source('./src/analysis/elevation-predicted-by-density+inundation/density.graph.R')$value 
# panel_c <- source() the triangle
panel_d <-  source('./src/analysis/nursery-experiment/final-wood-density.graph.R')$value 

panel_a$plot
