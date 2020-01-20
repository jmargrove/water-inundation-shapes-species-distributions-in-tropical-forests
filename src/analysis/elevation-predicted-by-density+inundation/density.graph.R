(function() {
    # import the model, predictions and the data
    analysis <- source("./src/analysis/elevation-predicted-by-density+inundation/elevation-density-inundation.analysis.R")$value
    # calculating the partial residuals 
    analysis$data$partials_density <- residuals(analysis$model, type = "partial")[, 1] + mean(analysis$data$elevation)
    # lines to join the partials 
    partial_lines_data <- data.frame(x = rep(analysis$data$density, 2),
                                   y = c(analysis$data$elevation,
                                   analysis$data$partials_density))

    # ANOVA type II to compare the variance 
    var_res_dd <- car::Anova(lm(elevation ~ density + riskratio, analysis$data))[, 1]
    density_explaied_variation <- paste("ANOVA:",
      round(var_res_dd / sum(var_res_dd) * 100, 1)[1],
      "%")

    # jittering the species names 
    vj <- rep(2.5, 16)
    vj[which(analysis$data$sp == "Spar")] <- -5
    vj[which(analysis$data$sp == "Smec")] <- -7
    vj[which(analysis$data$sp == "Sfal")] <- 5
    vj[which(analysis$data$sp == "Sxan")] <- 22
    vj[which(analysis$data$sp == "Swal")] <- -5

    #horazontal 
    hj <- rep(0.01, 16)
    hj[which(analysis$data$sp == "Spar")] <- -0.025
    hj[which(analysis$data$sp == "Smec")] <- -0.02
    hj[which(analysis$data$sp == "Slep")] <- 0.02
    hj[which(analysis$data$sp == "Pmal")] <- -0.02
    hj[which(analysis$data$sp == "Sacu")] <- -0.02
    hj[which(analysis$data$sp == "Sgib")] <- -0.02
    hj[which(analysis$data$sp == "Sbec")] <- 0.02
    hj[which(analysis$data$sp == "Spau")] <- 0.02
    hj[which(analysis$data$sp == "Sfal")] <- -0.02
    hj[which(analysis$data$sp == "Dry")] <- 0.02
    hj[which(analysis$data$sp == "Ssem")] <- -0.025
    hj[which(analysis$data$sp == "Ptom")] <- -0.025

    # Loading the themes
    themed <- source("./src/utils/theme.R")$value
    gg_theme <- source("./src/utils/gg-theme.graph.R")$value
    
    
    # error ribbon
    error_ribbon <- geom_ribbon(data = analysis$preds_density, 
                                aes(x = density, 
                                    y = elevation, 
                                    ymin = CI025, 
                                    ymax = CI975), 
                                fill = gg_theme$ribbon_color,
                                alpha = gg_theme$ribbon_alpha)
    # the model line
    model_line <- geom_line(data = analysis$preds_density, aes(x = density, y = elevation))
    # partial residuals connection lines 
    partial_residual_line <- geom_line(data = partial_lines_data, aes(x = x, y = y, group = factor(x)), alpha = 0.3)
    # partial data points 
    partial_data_points <- geom_point(data = analysis$data, aes(y = partials_density, x = density),
                                      color = themed$selectBlack(),
                                      pch = 21,
                                      fill = themed$selectLightGrey(), 
                                      size = gg_theme$partial_points_size) 
    # raw data points 
    raw_data_points <- geom_point(data = analysis$data, 
                                  aes(y = elevation, x = density), 
                                  size = gg_theme$raw_data_points_size,
                                  color = themed$selectRed())
    # axis labels 
    xlabel <- xlab(expression("Adult wood density" ~ g ~ cm ^ -3))
    ylabel <- ylab(bquote("E(elevation) m asl"^phantom("/")))
    # anova explained variation 
    anova_explained_variation <- geom_text(aes(x = 0.42, y = 122, 
                                               label = density_explaied_variation), 
                                           size = gg_theme$anova_text)
    # null model 
    null_model <- stat_smooth(data = analysis$data, aes(x = density, y = elevation),
                                    se = FALSE, method = "lm", color = themed$selectRed(),
                                    linetype = 2, size = 0.5)
    # species labels 
    species_name_labels <- geom_text(data = analysis$data, aes(x = density, y = elevation, label = sp),
              nudge_y = vj,
              nudge_x = hj,
              fontface = "italic", size = gg_theme$species_names_size) 
    
    
    # Plotting the data 
    p1 <- ggplot() +
      partial_residual_line +
      error_ribbon +
      model_line +
      null_model + 
      partial_data_points +
      raw_data_points +
      ylabel +
      xlabel +
      anova_explained_variation + 
      species_name_labels + 
      gg_theme$t
      
  p1
    # returning the wood density graph
    return(list(plot = p1, analysis = analysis))
})()