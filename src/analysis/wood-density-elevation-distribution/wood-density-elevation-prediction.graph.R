# Combining generalized least squares result and quantile regression into one figure 
(function() {
    dir <- "./src/analysis/wood-density-elevation-distribution/"
    data <- source("./src/data/wood-density-elevation-distribution/convert-density-points-matrix.dataframe.R")$value

    # model the quantile regression, with predictions
    qr_analysis <- source(paste(dir, "qr-wood-density-elevation-distribution.analysis.R", sep = ""))$value

    # generalized least squares analysis with predictions 
    gls_analysis <- source(paste(dir, "gls-wood-density-elevation-distribution.analysis.R", sep = ""))$value

    # colors 
    theme <- source("./src/utils/theme.R")$value
    gg_theme <- source("./src/utils/gg-theme.graph.R")$value

    # Plot of the results 
    p1 <- ggplot(gls_analysis$preds, aes(x = density_mean, y = elevation_mean)) +
      geom_point(data = data, aes(x = density_mean, y = elevation_mean), alpha = 0.5, size = 0.5) +
      geom_line(size = 0.5) +
      geom_ribbon(aes(ymin = CI025, ymax = CI975), alpha = gg_theme$ribbon_alpha, color = gg_theme$ribbon_color ) +
      ylab("Elevation (m) asl") +
      xlab(bquote("Adult wood density g" ~ cm ^ -3)) +
      theme(legend.position = "top") +
      scale_fill_manual(values = theme$selectColorPalette()) +
      xlim(NA, 0.62) +
      ylim(NA, 140) +
      geom_line(data = qr_analysis$preds[qr_analysis$preds$Quantile != "0.5",], # except the median 
                aes(x = density_mean, y = elevation_mean, color = Quantile), linetype = 1, size = 0.5) +
      scale_color_manual(values = theme$selectColorPalette()) + # color the lines
      gg_theme$t
    

    return(
      list(
        plot = p1, 
        qr_analysis = qr_analysis,
        gls_analysis = gls_analysis
      )
    )
    })()

