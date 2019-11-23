rm(list = ls())

# Import the table data
table <- read.csv('./src/summary-tables/species-list-plot.table.csv')
plot_data <- read.csv('./src/data/sepilok-160ha-plot.csv')
table
# add minium size limits 
table$minimum_size <- aggregate(plot_data$minimum_size, by = list(plot_data$species), mean)[, 2]

# count the number of individuals in the plot 
table["count"] <- tapply(plot_data$DBH, plot_data$species, function(el){
  length(el) 
})

# calculate the density per ha
table["density"] <- tapply(plot_data$DBH, plot_data$species, function(el){
  round(length(el) / 180, 3)
})

# calculate the mean dbh 
table["DBH-mean"] <- tapply(plot_data$DBH, plot_data$species, function(el){
  round(mean(el), 1)
})

# calculate the diameter at breast height standard devitation 
table['DBH_sd'] <- tapply(plot_data$DBH, plot_data$species, function(el){
  round(sd(el), 2)
})

# write the table
write.csv(table, file = './src/summary-tables/sepilok_160ha_plot.table.csv', row.names = FALSE)


