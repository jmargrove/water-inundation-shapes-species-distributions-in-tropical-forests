#' adult distribution data 
#' The adult distribution data 
(function() {
    #import the plot data and the species data 
    data_plot <- read.csv('./src/data/sepilok-plot/sepilok-160ha-plot.raw.csv') # plot 
    data_species <- source('./src/summary-tables/field-experiment-species-list.table.R')$value

    data <- data_plot[0,] # create the empty results data frame 
    for (sp in data_species$sp) {
        n <- length(data_plot$ID)
        occurance <- rep(0, n)
        index <- which(data_plot$sp == sp)
        occurance[index] <- 1
        data_copy <- data_plot
        data_copy$occurance <- occurance
        data_copy$focal_sp <- rep(sp, n)
        data <- rbind(data, data_copy)
    }
    
    # Return the organised data frame
    return(data)
})()