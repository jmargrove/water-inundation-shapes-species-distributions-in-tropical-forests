source('./utils/index.R')
handleWd('/inla-species-distributions')

# Import the table data
data <- read.csv('data/sepilok-160ha-plot.csv')
str(data)

# set up table with species values
table <- data.frame(species = as.character(levels(data$species)))
table$sp <- as.character(levels(data$sp))

write.csv(table, file = "./tables/species-list-plot.table.csv", row.names = FALSE)
# reset the working directory
resetWd()


