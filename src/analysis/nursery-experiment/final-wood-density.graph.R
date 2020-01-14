(function(){
  # Start with a module... graphic of the change in wood density 
  analysis <- source("./src/analysis/nursery-experiment/delta-wood-density.analysis.R")$value
  themed <- source('./src/utils/theme.R')$value
  str(analysis$preds_treat)
  library(ggplot2)
  p1 <- ggplot(data = analysis$preds_treat, aes(x = treatment, y = final_wood_density)) + 
    geom_ribbon(aes(ymin = CI025, ymax = CI975), fill = themed$selectLightGrey() ) + 
    geom_line() + 
    xlab('Treatment') + 
    theme_bw() 
    
  
  p1
  # return the plot
  return(list(plot = p1, analysis = analysis))
})()