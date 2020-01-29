# Create the data frame for the wood density elevation distribution analysis
(function() {
    # load variables 
    plot_data <- read.csv('./src/data/sepilok-plot/sepilok-160ha-plot.raw.csv') # load plot data 
    wood_density_data <- read.csv("./src/data/sepilok-plot/sepilok-adult-wood-density.table.csv")

    wood_density <- c()
    for (sp in wood_density_data$sp) {
        table_index <- which(wood_density_data$sp == sp) # Get the table index... should run 1, 2, 3...
        index <- which(plot_data$sp == sp) # Index of species in the plot data frame 
        wood_density[index] <- wood_density_data$density[table_index] # Assign density value to correct rows
    }

    plot_data$wood_density <- wood_density
    return(plot_data) # Return the modified plot data table 
})()



