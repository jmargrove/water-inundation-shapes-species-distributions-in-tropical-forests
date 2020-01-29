# Species list for the 160ha forest plot 
(function(){
  # Import the table data
  data <- read.csv('./src/data/sepilok-plot/sepilok-160ha-plot.raw.csv')
  # Set up table with species values
  table <- data.frame(species = as.character(levels(data$species)))
  table$sp <- as.character(levels(data$sp))
  #write.csv(table, file = "./src/summary-tables/species-list-plot.table.csv", row.names = FALSE)
  return(table)
})()


