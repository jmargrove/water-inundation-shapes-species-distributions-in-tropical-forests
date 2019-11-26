# summary table for the sapling wood density 

data <- read.csv("./src/data/sepilok-wood-density-saplings.raw.csv")
str(data)

table <- aggregate(data, list(species = data$species, sp = data$sp ), function(el){
  return(round(mean(el), 2))
})[, -c(3, 4)]

     
table$wood_density_sd <- tapply(data$wood_density, data$sp, function(el){
  return(round(sd(el), 3))
})


write.csv(table, file = "./src/summary-tables/sepilok-sapling-wood-density.table.csv", row.names = FALSE)
