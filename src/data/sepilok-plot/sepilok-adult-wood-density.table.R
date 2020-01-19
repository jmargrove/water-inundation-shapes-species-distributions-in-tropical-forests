
rm(list = ls())

wd_database <- read.csv('./src/data/wood-density-database.raw.csv')
table <- read.csv('./src/summary-tables/species-list-plot.table.csv')[, c('species', 'sp')]

raw_data <- merge(table, wd_database)[, c("species", "wood_density")]
raw_data


density <- tapply(raw_data$wood_density, raw_data$species, function(el) {
    return(round(mean(el, na.rm = TRUE), 2))
})

n <- tapply(raw_data$wood_density, raw_data$species, function(el) {
    length(el)
})


data <- data.frame(species = levels(raw_data$species), sp = table$sp, density, n)
data$ref <- rep("Global wood density db", length(data$species))

data["Unknown", "density"] <- mean(raw_data$wood_density, na.rm = T)
data["Unknown", "n"] <- length(raw_data$wood_density)

data["Vatica dulitensis", "density"] <- 0.82 # Soerjanegara and Lemmens, R.H.M.J. (Editors), 1994. PROSEA 5(1): Timber trees: Major commercial timbers.
data["Vatica dulitensis", "n"] <- 1
data["Vatica dulitensis", "ref"] <- "Soerjanegara and Lemmens, R.H.M.J. (Editors), 1994. PROSEA 5(1): Timber trees: Major commercial timbers."

# Mean shorea 
mShorea <- mean(data$density[grep("Shorea", as.character(data$species))], na.rm = TRUE)
nShorea <- length(data$density[grep("Shorea", as.character(data$species))])

data["Shorea waltonii", "density"] <- mShorea
data["Shorea waltonii", "n"] <- nShorea

data["Shorea symingtonii", "density"] <- mShorea
data["Shorea symingtonii", "n"] <- nShorea

data["Shorea confusa", "density"] <- mShorea
data["Shorea confusa", "n"] <- nShorea

data["Dipterocarpus caudiferus", "density"] <- 0.69 # Meijer &Wood(1964)
data["Dipterocarpus caudiferus", "n"] <- 1
data["Dipterocarpus caudiferus", "ref"] <- "Meijer &Wood(1964)"



data["Hopea sangal", "density"] <- 0.7 # Martawaijaya et al. 1992
data["Hopea sangal", "n"] <- 1
data["Hopea sangal", "ref"] <- "Martawaijaya et al. 1992"

data["Shorea pauciflora", "density"] <- 0.61
data["Shorea pauciflora", "n"] <- 4

data["Parashorea tomentella", "density"] <- 0.51 # Oey Djoen Seng (1951) in Soewarsono (1990)
data["Parashorea tomentella", "n"] <- 1
data["Parashorea tomentella", "ref"] <- "Oey Djoen Seng (1951) in Soewarsono (1990)"

data["Hopea spp.", "density"] <- mean(raw_data$wood_density[grep("Hopea", as.character(raw_data$species))], na.rm = TRUE)
data["Hopea spp.", "n"] <- length(grep("Hopea", as.character(raw_data$species)))


write.csv(data, file = "./src/data/sepilok-adult-wood-density.table.csv", row.names = FALSE)

