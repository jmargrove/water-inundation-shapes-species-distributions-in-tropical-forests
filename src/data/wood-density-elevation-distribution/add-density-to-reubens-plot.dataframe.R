# Reubens plots 
# Create the dataframe for the wood denisity elevation distribution analysis 
(function(){
  # Load variabiables 
  reubens_plot_data <- read.csv('./src/data/reuben-plot/reuben-plots-combined.dataframe.csv') # load plot data 
  wood_density_data <- read.csv("./src/data/sepilok-plot/sepilok-adult-wood-density.table.csv")
  
  wood_density <- c() # Empty variable 
  for(sp in wood_density_data$sp){
    table_index <- which(wood_density_data$sp == sp) # get the table index... should run 1, 2, 3...
    index <- which(reubens_plot_data$sp == sp) # index of species in the plot dataframe 
    wood_density[index] <- wood_density_data$density[table_index] # assign density value to correct rows
  }
  
  reubens_plot_data$wood_density <- wood_density
  return(reubens_plot_data) # Return the modified plotdata table 
})()
