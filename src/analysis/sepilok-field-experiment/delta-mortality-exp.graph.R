#graphing the seedling mortality data 
(function() {
    theme <- source("./src/utils/theme.R")$value
    # import bootstrapped data 
    boot_data <- read.table("./src/analysis/sepilok-field-experiment/predictions/delta-mortality.bootstrapped.R", header = TRUE)
    # import model
    analysis <- source("./src/analysis/sepilok-field-experiment/field-exp-mortality.analysis.R")$value
    library(ggplot2)
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
    

    pred_diff <- analysis$preds[1:16,]
    pred_diff$diff_mort <- analysis$preds$mortality[17:32] - analysis$preds$mortality[1:16]
    pred_diff$CI025 <- as.numeric(boot_data[1, 33:48])
    pred_diff$CI975 <- as.numeric(boot_data[2, 33:48])
    pred_diff$occurance_probability <- pelev_data$occurance_probability
    pred_diff$`Different from Zero` <- rep("diff", 16)
    pred_diff$`Different from Zero`[which(pred_diff$CI025 < 0)] <- "no diff"
    pred_diff$Wooddensity <- exp_density_data$density
    pred_diff$fden <- exp_density_data$fden

    
    # plot the graph of the differences
    gg_theme <- source('./src/utils/gg-theme.graph.R')$value
    themed <- source('./src/utils/theme.R')$value
    
    # facet the plots like the main figuer 
    facet <- facet_grid(~fden, space = "free", scale = "free") 
    # error bars 
    error_bars <- geom_errorbar(data = pred_diff, 
                                aes(x = reorder(sp, occurance_probability), 
                                    y = diff_mort, 
                                    group = `Different from Zero`, 
                                    ymin = CI025, ymax = CI975), 
                                width = 0.3, 
                                color = themed$selectMedGrey(), 
                                size = 0.5) 
    # Color palet 
    color_palet <- scale_fill_manual(values = c(theme$selectBlack(), theme$selectRed()))
    
    p1 <- ggplot(pred_diff,
      aes(x = reorder(sp, occurance_probability),
      y = diff_mort, fill = `Different from Zero`)) +
  # geom_errorbar(aes(ymin = CI025, ymax = CI975), width = 0.5, size = 1) +
  error_bars +
  facet + 
  theme_bw() +
  color_palet +
  xlab("Species") +
  ylab("Risk-ratio (delta Mortality)") +

  geom_point(size = 2, pch = 21) +
  geom_hline(aes(yintercept = 0), linetype = 2, col = theme$selectRed()) +
  gg_theme$t2 +
  theme(axis.text.x = element_text(face = "italic", angle = 45, vjust = .7, family = gg_theme$font_family)) +
  theme(strip.text = element_text(face = "italic", 
                                  family = gg_theme$font_family, 
                                  size = 8, 
                                  margin = margin(t = 0.25, r = 0, b = 0.25, l = 0, unit = "mm"))) +
  theme(axis.text = element_text(size = 8, family = gg_theme$font_family)) +
  theme(axis.text.x = element_text(face = "italic", angle = 45, vjust = .7)) +
  theme(axis.line = element_line(size = 0.2, color = themed$selectBlack())) +
  theme(axis.ticks = element_line(size = 0.15)) +
  theme(strip.background = element_rect(colour="black", size = 0.25), 
        legend.title = element_text(family = gg_theme$font_family, size = 8), 
        legend.key.width = unit(5, units = "mm"),
        legend.box.spacing = unit(0, units = "mm"),
        legend.box.margin = margin(0, 0, 0, 0, unit = 'mm')) 

    return(list(plot = p1, analysis = analysis ))
})()