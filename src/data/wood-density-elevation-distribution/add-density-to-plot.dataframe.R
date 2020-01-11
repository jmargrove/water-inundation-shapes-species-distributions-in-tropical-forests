# Create the dataframe for the wood denisity elevation distribution analysis
(function(){
  # load variabiables 
  plot_data <- read.csv('./src/data/sepilok-plot/sepilok-160ha-plot.raw.csv') # load plot data 
  wood_density_data <- read.csv("./src/data/sepilok-plot/sepilok-adult-wood-density.table.csv")
  
  wood_density <- c()
  for(sp in wood_density_data$sp){
    table_index <- which(wood_density_data$sp == sp) # get the table index... should run 1, 2, 3...
    index <- which(plot_data$sp == sp) # index of species in the plot dataframe 
    wood_density[index] <- wood_density_data$density[table_index] # assign density value to correct rows
  }
  
  plot_data$wood_density <- wood_density
  return(plot_data) # return the modified plotdata table 
})()



