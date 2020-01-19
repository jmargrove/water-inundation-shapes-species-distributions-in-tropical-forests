(function() {
    # Start with a module... graphic of the change in wood density 
    analysis <- source("./src/analysis/nursery-experiment/final-wood-density.analysis.R")$value
    themed <- source("./src/utils/theme.R")$value
    str(analysis$preds_treat)

    # Loading ggplot2 
    library(ggplot2)

    # Model prediction
    pred_line <- geom_line()

    # Raw data points as box plot 
    raw_points <- geom_point(data = analysis$mode@frame,
                             aes(x = exp(`log(treatment + 1)`) - 1,
                                 y = final_wood_density,
                                 group = factor(`log(treatment + 1)`)),
                             color = themed$selectBlue(), width = 1)

    # Confidence intervals 95% n = 5000
    conf_int <- geom_ribbon(aes(ymin = CI025, ymax = CI975), fill = themed$selectMedGrey())

    # Mean species line 
    base_density_data <- read.csv("./src/data/nursery-experiment/wood-density-nursery-base.raw.csv")

    # Baseline for the density
    base_density_line <- geom_abline(slope = 0,
                           intercept = mean(base_density_data$wood_density, na.rm = T),
                           linetype = 2,
                           color = themed$selectRed())

    # Compose the plot
    p1 <- ggplot(data = analysis$preds_treat, aes(x = treatment, y = final_wood_density)) +
    raw_points +
    conf_int +
    pred_line +
    base_density_line +
    xlab("Treatment") + ylab("Final wood density g cm-3") +
    theme_bw()

    p1
    # Return the plot
    return(list(plot = p1, analysis = analysis))
})()
