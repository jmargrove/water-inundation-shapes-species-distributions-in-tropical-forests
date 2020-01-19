#' adult distribution data 
#' The adult distribution data 

occurance_data <- function() {
    #import the plot data and the species data 
    data_plot <- read.csv('./src/data/sepilok-plot/sepilok-160ha-plot.raw.csv') # plot 
    data_species <- read.csv('./src/data/sepilok-field-experiment/field-experiment-species-list.table.csv') # speces 

    data <- data_plot[0,] # create the results data frame 
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
    return(data)
}