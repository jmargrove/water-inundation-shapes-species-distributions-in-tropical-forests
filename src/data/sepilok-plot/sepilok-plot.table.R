#' @title Sepilok plot table 
#' @description summary of the Sepilok plot 

rm(list = ls())
source('./src/utils/units.R')

# Import the table data
table <- read.csv('./src/summary-tables/species-list-plot.table.csv')
plot_data <- read.csv('./src/data/sepilok-160ha-plot.raw.csv')
density_data <- read.csv('./src/data/sepilok-adult-wood-density.table.csv')

# Add minimum size limits 
table$minimum_size <- aggregate(plot_data$minimum_size, by = list(plot_data$species), mean)[, 2]

# count the number of individuals in the plot 
table["count"] <- tapply(plot_data$DBH, plot_data$species, function(el) {
    length(el)
})

# calculate the mean DBH
e[paste("DBH", mu)] <- tapply(plot_data$DBH, plot_data$species, function(el) {
    round(mean(el, na.rm = T), 1)
})


names(table)[which(names(table) == 'species')] <- "Species"
names(table)[which(names(table) == 'minimum_size')] <- "Min Size"

#add wood density data
table$wood_density <- round(density_data$density, 2)
table$ref <- density_data$ref

# write the table
write.csv(table, file = './src/summary-tables/sepilok-plot.table.csv', row.names = FALSE)


