# Creating the wood density graph showing variation.. explained by the model.
(function() {
    # Import the model, predictions and the data...
    analysis <- source("./src/analysis/elevation-predicted-by-density+inundation/elevation-density-inundation.analysis.R")$value
    
    # Calculating the partial residuals 
    analysis$data$partials_density <- residuals(analysis$model, type = "partial")[, 1] + mean(analysis$data$elevation)
    
    # Lines to join the partials 
    partial_lines_data <- data.frame(x = rep(analysis$data$density, 2),
                                     y = c(analysis$data$elevation,
                                           analysis$data$partials_density)
                                     )

    # ANOVA to calculate the variance
    var_res_dd <- car::Anova(lm(elevation ~ density + riskratio, analysis$data))[, 1]
    exp_var_dd <-  round(var_res_dd / sum(var_res_dd) * 100, 1)[1]
    density_explaied_variation <- paste("ANOVA:", exp_var_dd, "%")

    # Jittering the species names 
    vj <- rep(2.5, 16)
    vj[which(analysis$data$sp == "Spar")] <- -6
    vj[which(analysis$data$sp == "Smec")] <- -7
    vj[which(analysis$data$sp == "Sfal")] <- 6
    vj[which(analysis$data$sp == "Sxan")] <- 22
    vj[which(analysis$data$sp == "Swal")] <- -5
    vj[which(analysis$data$sp == "Sacu")] <- 3.1
    vj[which(analysis$data$sp == "Ssmi")] <- 3.5
    vj[which(analysis$data$sp == "Spau")] <- 3

    # Horizontal 
    hj <- rep(0.01, 16)
    hj[which(analysis$data$sp == "Spar")] <- -0.03
    hj[which(analysis$data$sp == "Smec")] <- -0.02
    hj[which(analysis$data$sp == "Slep")] <- 0.02
    hj[which(analysis$data$sp == "Pmal")] <- -0.02
    hj[which(analysis$data$sp == "Sacu")] <- 0
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


    # Error ribbon
    ErrorComponent <- geom_ribbon(data = analysis$preds_density,
                                  aes(x = density,
                                      y = elevation,
                                      ymin = CI025,
                                      ymax = CI975),
                                  fill = gg_theme$ribbon_color,
                                  alpha = gg_theme$ribbon_alpha
                                  )
    
    # The model line
    ModelComponent <- geom_line(data = analysis$preds_density, 
                                aes(x = density, 
                                    y = elevation
                                    )
                                )
    
    # Partial residuals connection lines 
    PartialDataLineComponent <- geom_line(data = partial_lines_data, 
                                          aes(x = x, 
                                              y = y, 
                                              group = factor(x)
                                              ), 
                                          alpha = 0.3
                                          )
    
    # Partial data points 
    PartialDataComponent <- geom_point(data = analysis$data, 
                                       aes(y = partials_density, 
                                           x = density
                                           ),
                                       color = themed$selectBlack(),
                                       pch = 21,
                                       fill = themed$selectLightGrey(),
                                       size = gg_theme$partial_points_size
                                       )
    # Raw data points 
    RawDataComponent <- geom_point(data = analysis$data,
                                  aes(y = elevation, 
                                      x = density
                                      ),
                                  size = gg_theme$raw_data_points_size,
                                  color = themed$selectRed()
                                  )
    # Axis labels 
    xlabel <- xlab(expression("Adult wood density" ~ g ~ cm ^ -3))
    ylabel <- ylab(bquote("E(elevation) m asl" ^ phantom("/")))
    
    # Anova explained variation 
    AnovaComponent <- geom_text(aes(x = 0.42, 
                                    y = 122,
                                    label = density_explaied_variation
                                    ),
                                family = gg_theme$font_family,
                                size = gg_theme$anova_text
                                )
    # Null model 
    NullModelComponent <- stat_smooth(data = analysis$data,
                                      aes(x = density, 
                                          y = elevation
                                          ),
                                      se = FALSE, 
                                      method = "lm",
                                      color = themed$selectRed(),
                                      linetype = 2, 
                                      size = 0.5
                                      )
    # Species labels 
    SpeciesNameComponent <- geom_text(data = analysis$data, 
                                      aes(x = density, 
                                          y = elevation, 
                                          label = sp
                                          ),
                                      nudge_y = vj,
                                      nudge_x = hj,
                                      family = gg_theme$font_family,
                                      fontface = "italic", 
                                      size = gg_theme$species_names_size
                                      )


    # Combining components 
    p1 <- ggplot() +
      PartialDataLineComponent +
      ErrorComponent +
      ModelComponent +
      NullModelComponent +
      PartialDataComponent +
      RawDataComponent +
      ylabel +
      xlabel +
      AnovaComponent +
      SpeciesNameComponent +
      gg_theme$t

    p1
    
    # Return the wood density graph
    return(list(plot = p1, analysis = analysis))
})()