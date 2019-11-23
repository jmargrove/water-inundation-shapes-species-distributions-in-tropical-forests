rm(list = ls())
source('./utils/index.R')
handleWd('/inla-species-distributions')

# Import the table data
data <- read.csv('data/sepilok-160ha-plot.csv')
str(data)

# set up table with species values
table <- data.frame(species = as.character(levels(data$species)))
table$sp <- as.character(levels(data$sp))
table$minimum_size <- aggregate(data$minimum_size, by = list(data$species), mean)[, 2]

# count the number of individuals in the plot 
table["count"] <- tapply(data$DBH, data$species, function(el){
  length(el) 
})

# calculate the density per ha
table["density ha^-1"] <- tapply(data$DBH, data$species, function(el){
  round(length(el) / 180, 3)
})

# calculate the mean dbh 
table["DBH μ"] <- tapply(data$DBH, data$species, function(el){
  round(mean(el), 1)
})

# calculate the diameter at breast height standard devitation 
table['DBH σ'] <- tapply(data$DBH, data$species, function(el){
  round(sd(el), 2)
})

# write the table
write.csv(table, file = './tables/sepilok_160ha_plot.table.csv', row.names = FALSE)

# reset the working directory
resetWd()


