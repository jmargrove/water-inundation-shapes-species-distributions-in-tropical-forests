#' @author James Margrove 
#' @description Summary table of the sapling wood density data 

# Clear workspace 
rm(list = ls())

# Import data 
data <- read.csv('./src/data/saplings/sepilok-wood-density-saplings.raw.csv')
# Summary 
str(data)

# Create summery table of the sapling data 
summary_table <- data.frame(
  species = levels(data$species), 
  sp = levels(data$sp), 
  mean_wood_density = with(data, tapply(wood_density, sp, mean)), 
  sd_wood_density = with(data, tapply(wood_density, sp, sd)), 
  n = with(data, tapply(wood_density, sp, length))
)

# write table 
write.csv(summary_table, './src/data/saplings/sepilok-wood-density-sapling.table.csv', row.names = FALSE) 
