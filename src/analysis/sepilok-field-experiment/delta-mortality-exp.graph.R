#graphing the seedling mortality data 
(function() {
    theme <- source("./src/utils/theme.R")$value
    # import bootstrapped data 
    boot_data <- read.table("./src/analysis/sepilok-field-experiment/predictions/delta-mortality.bootstrapped.R", header = TRUE)
    # import model
    mort_analysis <- source("./src/analysis/sepilok-field-experiment/field-exp-mortality.analysis.R")$value
    library(ggplot2)
    # Import the elevation data for ordering 
    pelev_data <- read.csv("./src/analysis/inla-spde-species-distributions/predictions/occurance-probabilities.dataframe.csv")
    # import the wood density data for partitioning
    plot_density_data <- read.csv("./src/data/sepilok-plot/sepilok-adult-wood-density.table.csv", header = TRUE)
    # import species data 
    species_data <- read.csv("./src/data/sepilok-field-experiment/field-experiment-species-list.table.csv")
    exp_density_data <- plot_density_data[plot_density_data$sp %in% as.character(species_data$sp),]

    pred_diff <- mort_analysis$preds[1:16,]
    pred_diff$diff_mort <- mort_analysis$preds$mortality[17:32] - mort_analysis$preds$mortality[1:16]
    pred_diff$CI025 <- as.numeric(boot_data[1, 33:48])
    pred_diff$CI975 <- as.numeric(boot_data[2, 33:48])
    pred_diff$occurance_probability <- pelev_data$occurance_probability
    pred_diff$`Different from Zero` <- rep("diff", 16)
    pred_diff$`Different from Zero`[which(pred_diff$CI025 < 0)] <- "no diff"
    pred_diff$Wooddensity <- exp_density_data$density

    # plot the graph of the differences
    p1 <- ggplot(pred_diff,
      aes(x = reorder(sp, occurance_probability),
      y = diff_mort, color = `Different from Zero`)) +
  geom_errorbar(aes(ymin = CI025, ymax = CI975), width = 0.5, size = 1) +
  theme_bw() +
  xlab("Species") +
  ylab("Risk ratio (Mortality)") +
  theme(axis.text.x = element_text(face = "italic", angle = 45, vjust = .7)) +
  scale_color_manual(values = c(theme$selectBlack(), theme$selectRed())) +
  geom_point(size = 5, fill = theme$selectWhite()) +
  geom_hline(aes(yintercept = 0), linetype = 2, col = theme$selectRed()) +
  theme(legend.position = c(0.15, 0.8)) +
  theme(text = element_text(size = 20))

    return(p1)
})()