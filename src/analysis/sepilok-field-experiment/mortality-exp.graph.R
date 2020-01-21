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

    
    gg_theme <- source('./src/utils/gg-theme.graph.R')$value
    themed <- source('./src/utils/theme.R')$value
    
    # error_bars 
    error_bars <- geom_errorbar(data = mort_analysis$preds, 
                                aes(x = reorder(sp, occurance_probability), y = mortality, group = `Water inundation`, ymin = CI025, ymax = CI975), 
                                width = 0.3, color = themed$selectMedGrey(), 
                                size = 0.3) 
    # facet 
    facet <- facet_grid(~fden, space = "free", scale = "free") 
    xlabel <-  xlab("Species")
    ylabel <- ylab("p(Mortality)")
    
    # points 
    model_points <- geom_point(data = mort_analysis$preds, aes(x = reorder(sp, occurance_probability), y = mortality, fill = `Water inundation`), size = 2, pch = 21) 
    # point theme 
    point_theme <- scale_fill_manual(values = c(theme$selectBlack(), theme$selectRed()))
    # elevation ordering indication
    yh <- 0.7
    jh <- 0.03
    text_data <- data.frame(x = c(1 + 0.1, 7 - 0.1 , 1 + 0.1, 9 - 0.1),
                            y = rep(yh + jh , 4),
                            fden = c('High wood density', 'High wood density', 'Low wood density', 'Low wood density'),
                            label = c('high', "low", 'high', "low")
                            )
    
    text <- geom_text(data = text_data,
                      aes(x = x, y = y, label = label), size = 2,
                      family = gg_theme$font_family
                      )
    # arrows 
    line_data <- data.frame(x = c(1, 7, 1, 9), 
                            y = c(yh, yh, yh, yh), 
                            fden = c('High wood density', 'High wood density', 'Low wood density', 'Low wood density')
                            )
    arrows <- geom_line(data = line_data, 
                        aes(x = x, y = y, group = fden), 
                        size = 0.25, 
                        arrow = arrow(ends = "both", length = unit(0.03, "npc")))
    
    # adding elevation (m) asl 
    elevation_text_position <- data.frame(x = c((1 + 7) / 2, (1 + 9) / 2),
                                          y = rep(yh + jh , 2),
                                          fden = c('High wood density', 'Low wood density'),
                                          label = rep(('Elevation (m) asl'), 2)
    )
    
    elevation_text <- geom_text(data = elevation_text_position, 
                                aes(x = x, y = y, label = label), size = 2, family = gg_theme$font_family)
    
    # Graphing the results
    p1 <- ggplot() +
          error_bars +
          facet +
          model_points +
          point_theme +
          xlabel +
          ylabel +
          gg_theme$t2 +
          theme(axis.text.x = element_text(face = "italic", angle = 45, vjust = .7, family = gg_theme$font_family)) +
        theme(strip.text = element_text(face = "italic", 
                                        family = gg_theme$font_family, 
                                        size = 8, 
                                        margin = margin(t = 0.25, r = 0, b = 0.25, l = 0, unit = "mm"))) +
    theme(axis.text = element_text(size = 8, family = gg_theme$font_family)) +
    theme(axis.line = element_line(size = 0.2, color = themed$selectBlack())) +
    theme(axis.ticks = element_line(size = 0.15)) +
    theme(strip.background = element_rect(colour="black", size = 0.25), 
          legend.title = element_text(family = gg_theme$font_family, size = 10), 
          legend.key.width = unit(5, units = "mm"),
          legend.box.spacing = unit(0, units = "mm"),
          legend.box.margin = margin(0, 0, 0, 0, unit = 'mm')) + 
    arrows +
    text + 
    elevation_text
    
    p1

    return(list(plot = p1))
})()
    