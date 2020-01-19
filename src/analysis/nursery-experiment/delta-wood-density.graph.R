

(function() {
    analysis <- source("./src/analysis/nursery-experiment/delta-wood-density.analysis.R")$value
    themed <- source("./src/utils/theme.R")$value
    str(analysis$preds_treat)
    # plot the data 
    library(ggplot2)
    pred_line <- geom_line()
    raw_points <- geom_boxplot(data = analysis$mode@frame,
                           aes(x = exp(`log(treatment + 1)`) - 1,
                               y = delta_density, by = factor(exp(`log(treatment + 1)`) - 1)),
                           color = themed$selectBlue(),
                           width = 0.5)

    conf_int <- geom_ribbon(aes(ymin = CI025, ymax = CI975), fill = themed$selectMedGrey())
    zero_line <- geom_abline(slope = 0, linetype = 2, color = themed$selectRed())

    p1 <- ggplot(data = analysis$preds_treat, aes(x = treatment, y = delta_density)) +
    raw_points + conf_int + pred_line + zero_line +
    xlab("Treatment") + ylab("Delta wood density g cm-3") +
    theme_bw()

    p1
    return(p1)
})()

