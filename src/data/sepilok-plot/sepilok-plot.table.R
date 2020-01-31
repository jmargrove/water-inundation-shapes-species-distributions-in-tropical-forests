#' summary of the Sepilok plot 

rm(list = ls())
units <- source('./src/utils/units.R')$value

# Import the table data
# table <- read.csv('./src/summary-tables/species-list-plot.table.csv')
plot_data <- read.csv('./src/data/sepilok-plot/sepilok-160ha-plot.raw.csv')
table <- unique(plot_data[, c("species", "sp")])
density_data <- read.csv('./src/data/sepilok-plot/sepilok-adult-wood-density.table.csv')

# Add minimum size limits 
table$minimum_size <- aggregate(plot_data$minimum_size, by = list(plot_data$species), mean)[, 2]

# count the number of individuals in the plot 
table["count"] <- tapply(plot_data$DBH, plot_data$species, function(el) {
    length(el)
})

# calculate the mean DBH
table[paste("DBH", units$mu)] <- tapply(plot_data$DBH, plot_data$species, function(el) {
    round(mean(el, na.rm = T), 1)
})


names(table)[which(names(table) == 'species')] <- "Species"
names(table)[which(names(table) == 'minimum_size')] <- "Min Size"

# Add wood density data
table$wood_density <- round(density_data$density, 2)
table$ref <- density_data$ref
table
# write the table
# write.csv(table, file = './src/summary-tables/sepilok-plot.table.csv', row.names = FALSE)


