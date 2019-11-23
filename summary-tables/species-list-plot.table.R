rm(list = ls())
# Import the table data
data <- read.csv('./data/sepilok-160ha-plot.csv')
str(data)

# set up table with species values
table <- data.frame(species = as.character(levels(data$species)))
table$sp <- as.character(levels(data$sp))

write.csv(table, file = "./summary-tables/species-list-plot.table.csv", row.names = FALSE)



