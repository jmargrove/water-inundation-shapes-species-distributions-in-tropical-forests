(function(){
  # Start with a module... graphic of the change in wood density 
  analysis <- source("./src/analysis/nursery-experiment/delta-wood-density.analysis.R")$value
  str(analysis)
  
  p1 <- ggplot(data = analysis$preds, aes(x = treat, y = final_wood_density)) + 
    geom_line()
  
  # return the plot
  return(list(plot = p1, analysis = analysis))
})()