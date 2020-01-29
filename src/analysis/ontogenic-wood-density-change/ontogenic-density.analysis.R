# Ontogenic wood density change module.
(function() {
    # Load data
    seedling_data <- read.csv("./src/data/nursery-experiment/wood-density-base.table.csv")
    colIndex_seedling <- which(names(seedling_data) == "wood_density")
    names(seedling_data)[colIndex_seedling] <- "seedling_density"

    # Load sapling data 
    sapling_data <- read.csv("./src/data/saplings/sepilok-wood-density-sapling.table.csv")
    colIndex_sapling <- which(names(sapling_data) == "mean_wood_density") # which index is the density column
    names(sapling_data)[colIndex_sapling] <- "sapling_density" # assign new col name 

    adult_data <- read.csv("./src/data/sepilok-plot/sepilok-adult-wood-density.table.csv")
    colIndex_adult <- which(names(adult_data) == "density")
    names(adult_data)[colIndex_adult] <- "adult_density"

    # Merge all data frames by species
    merged_data <- merge(seedling_data, sapling_data, by = "sp")
    merged_data <- merge(merged_data, adult_data, by = "sp")

    # Copy over required cols
    data <- merged_data[, c("species.x", "sp", "seedling_density", "sapling_density", "adult_density")]
    names(data)[1] <- "species" # species.x to species 

    # 3x analysis of the density
    adult_seedling_model <- summary(lm(adult_density ~ seedling_density, data))
    seedling_sapling_model <- summary(lm(seedling_density ~ sapling_density, data))
    adult_sapling_model <- summary(lm(adult_density ~ sapling_density, data))

    r2results <- data.frame(vars = c("seedling_adult", "seedling_sap", "adult_sap"),
             `r2` = c(adult_seedling_model$r.squared, seedling_sapling_model$r.squared, adult_sapling_model$r.squared))

    # Analysis object to send back the analysis 
    analysis <- list(
      r2 = r2results,
        models = list(
          adult_seedling = adult_seedling_model,
          seedling_sapling = seedling_sapling_model,
          adult_sapling = adult_sapling_model)
      )
    # Return the analysis 
    return(analysis)
})()
