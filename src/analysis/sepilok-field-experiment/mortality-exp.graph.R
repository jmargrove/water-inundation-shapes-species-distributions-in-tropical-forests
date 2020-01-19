# Graphic of the interaction between species and mortality 
(function() {
    # Import theme for graphing
    library(ggplot2)
    theme <- source("./src/utils/theme.R")$value
    # Import model & predictions
    mort_analysis <- source("./src/analysis/sepilok-field-experiment/field-exp-mortality.analysis.R")$value
    # Import the elevation data for ordering
    pelev_data <- read.csv("./src/analysis/inla-spde-species-distributions/predictions/occurance-probabilities.dataframe.csv")
    # import the wood density data for partitioning
    plot_density_data <- read.csv("./src/data/sepilok-plot/sepilok-adult-wood-density.table.csv", header = TRUE)
    # import species data
    species_data <- read.csv("./src/data/sepilok-field-experiment/field-experiment-species-list.table.csv")
    exp_density_data <- plot_density_data[plot_density_data$sp %in% as.character(species_data$sp),]

    # Create high and low wood density factors
    exp_density_data$fden <- cut(exp_density_data$density,
                      breaks = c(0, mean(exp_density_data$density), 1),
                      labels = c("Low wood density", "High wood density"))

    mort_analysis$preds$fden <- rep(exp_density_data$fden, times = 2) # add the density factors
    mort_analysis$preds$occurance_probability <- rep(pelev_data$occurance_probability, times = 2) # add the occurance probabilities
    mort_analysis$preds$fden <- relevel(mort_analysis$preds$fden, ref = "High wood density") # re-level the factor labels :)
    colnames(mort_analysis$preds)[which(colnames(mort_analysis$preds) == "flood")] <- "Water inundation" # flood -> water inundation
    levels(mort_analysis$preds$`Water inundation`) <- c("Dry", "Wet") # relabel high low to wet and dry

    # Graphing the results
    p1 <- ggplot(mort_analysis$preds, aes(x = reorder(sp, occurance_probability), y = mortality, group = `Water inundation`)) +
          facet_grid(~fden, space = "free", scale = "free") +
          geom_errorbar(aes(ymin = CI025, ymax = CI975), width = 0.3, alpha = 0.2) +
          theme_classic() +
          geom_point(size = 4) +
          geom_point(size = 3, aes(color = `Water inundation`)) +
          theme(legend.position = c(0.35, 0.8)) +
          xlab("Species") +
          ylab("p(Mortality)") +
          theme(axis.text.x = element_text(face = "italic", angle = 45, vjust = .7)) +
          scale_color_manual(values = c(theme$selectBlack(), theme$selectRed()))

    return(p1)
})()