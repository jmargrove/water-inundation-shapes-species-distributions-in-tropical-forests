rm(list = ls())
wd_database <- read.csv('./src/data/wood-density-database.csv')
paine_traits <- read.csv('./src/data/paine-functional-traits-data.csv')
table <-  read.csv('./src/summary-tables/species-list-plot.table.csv')
names(paine_traits)[ which(names(paine_traits) == "WD")] <- "wood_density"
paine_traits$species <- paste(paine_traits$genus, paine_traits$species, sep = " ")

raw_data <- rbind(
  merge(table, wd_database)[, c("species", "wood_density")], 
  merge(table, paine_traits)[, c("species", "wood_density")]
  )
str(raw_data)

density <- tapply(raw_data$wood_density, raw_data$species, function(el){
  return(round(mean(el, na.rm = TRUE), 2))
})

n <- tapply(raw_data$wood_density, raw_data$species, function(el){
  length(el)
})


data <- data.frame(species = levels(raw_data$species), density, n)

data["Unknown", "density"] <- mean(raw_data$wood_density, na.rm = T)
data["Unknown", "n"] <- length(raw_data$wood_density)

data["Vatica dulitensis", "density"] <- mean(raw_data$wood_density[grep("Vatica", as.character(raw_data$species))], na.rm = TRUE)
data["Vatica dulitensis", "n"] <- length(grep("Vatica", as.character(raw_data$species)))

data["Shorea waltonii", "density"] <- mean(raw_data$wood_density[grep("Shorea", as.character(raw_data$species))], na.rm = TRUE)
data["Shorea waltonii", "n"] <- length(grep("Shorea", as.character(raw_data$species)))

data["Shorea symingtonii", "density"] <- mean(raw_data$wood_density[grep("Shorea", as.character(raw_data$species))], na.rm = TRUE)
data["Shorea symingtonii", "n"] <- length(grep("Shorea", as.character(raw_data$species)))

data["Shorea foxworthyii", "density"] <- mean(raw_data$wood_density[grep("Shorea", as.character(raw_data$species))], na.rm = TRUE)
data["Shorea foxworthyii", "n"] <- length(grep("Shorea", as.character(raw_data$species)))

data["Shorea confusa", "density"] <- mean(raw_data$wood_density[grep("Shorea", as.character(raw_data$species))], na.rm = TRUE)
data["Shorea confusa", "n"] <- length(grep("Shorea", as.character(raw_data$species)))

data["Dipterocarpus caudiferus", "density"] <- mean(raw_data$wood_density[grep("Dipterocarpus", as.character(raw_data$species))], na.rm = TRUE)
data["Dipterocarpus caudiferus", "n"] <- length(grep("Dipterocarpus", as.character(raw_data$species)))

data["Hopea spp.", "density"] <- mean(raw_data$wood_density[grep("Hopea", as.character(raw_data$species))], na.rm = TRUE)
data["Hopea spp.", "n"] <- length(grep("Hopea", as.character(raw_data$species)))

write.csv(data, file = "./src/summary-tables/sepilok-wood-density.table.csv", row.names = FALSE)