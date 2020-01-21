(function() {
    # Start with a module... graphic of the change in wood density 
    analysis <- source("./src/analysis/nursery-experiment/final-wood-density.analysis.R")$value
    themed <- source("./src/utils/theme.R")$value
    str(analysis$preds_treat)

    # Loading ggplot2 
    library(ggplot2)

    gg_theme <- source('./src/utils/gg-theme.graph.R')$value
    # Model prediction
    pred_line <- geom_line()

    # Raw data points as box plot 
    raw_points <- geom_point(data = analysis$mode@frame,
                             aes(x = exp(`log(treatment + 1)`) - 1,
                                 y = final_wood_density,
                                 group = factor(`log(treatment + 1)`)),
                             color = themed$selectBlue(), size = 0.2, alpha = 0.5)

    # Confidence intervals 95% n = 5000
    conf_int <- geom_ribbon(aes(ymin = CI025, ymax = CI975), 
                            fill = gg_theme$ribbon_color, 
                            alpha = gg_theme$ribbon_alpha)

    # Mean species line 
    base_density_data <- read.csv("./src/data/nursery-experiment/wood-density-nursery-base.raw.csv")

    # Baseline for the density
    base_density_line <- geom_abline(slope = 0,
                           intercept = mean(base_density_data$wood_density, na.rm = T),
                           linetype = 2,
                           color = themed$selectRed())
    
    # The raw data is very messy, I will try an add species means for each treatment
    head(analysis$data)
    mean_data <- expand.grid(treatment  = seq(0, 21, 3), sp = levels(analysis$data$sp))
    mean_data$mean_density <- as.vector(with(analysis$data, tapply(final_wood_density, list(sp, treatment), mean, na.rm = TRUE)))
    mean_data_points <- geom_point(data = mean_data, aes(x = treatment, y = mean_density))

    
    # Compose the plot
    p1 <- ggplot(data = analysis$preds_treat, aes(x = treatment, y = final_wood_density)) +
    # raw_points +
    # mean_data_points +
    conf_int +
    pred_line +
    base_density_line +
    # xlab("Treatment") + 
    xlab(bquote("Treatment"^phantom("/"))) +
    ylab(bquote("Final wood density g cm"^-3)) +
    # ylab(expression("" ~ g ~ cm ^ -3)) +
    gg_theme$t
    

    p1
    # Return the plot
    return(list(plot = p1, analysis = analysis))
})()
