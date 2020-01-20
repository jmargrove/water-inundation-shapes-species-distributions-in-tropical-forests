(function() {
    # import the the model, data, and preds module
    analysis <- source("./src/analysis/elevation-predicted-by-density+inundation/elevation-density-inundation.analysis.R")$value
    # import the theme
    themed <- source("./src/utils/theme.R")$value

    # calculating the partial residuals 
    analysis$data$partials_rr <- residuals(analysis$model, type = "partial")[, 2] + mean(analysis$data$elevation)
    # lines to join the partials 
    partial_lines_data <- data.frame(x = rep(analysis$data$riskratio, 2),
                                   y = c(analysis$data$elevation, analysis$data$partials_rr))
    # ANOVA type II to compare the variance 
    var_res_rr <- car::Anova(lm(elevation ~ density + riskratio, analysis$data))[, 1]
    rr_explaied_variation <- paste("ANOVA:", round(var_res_rr / sum(var_res_rr) * 100, 1)[2], "%")

    # positioning needs to be better
    # jittering the species names 
    vj_rr <- rep(4, 16)
    vj_rr[which(analysis$data$sp == "Spar")] <- -3.5
    vj_rr[which(analysis$data$sp == "Smac")] <- 12
    vj_rr[which(analysis$data$sp == "Slep")] <- -5
    vj_rr[which(analysis$data$sp == "Smec")] <- -3
    vj_rr[which(analysis$data$sp == "Pmal")] <- -4
    vj_rr[which(analysis$data$sp == "Sgib")] <- 10
    vj_rr[which(analysis$data$sp == "Ssmi")] <- -4
    vj_rr[which(analysis$data$sp == "Sacu")] <- -4

    #horazontal 
    hj_rr <- rep(0.01, 16)
    hj_rr[which(analysis$data$sp == "Ssmi")] <- -0.02
    hj_rr[which(analysis$data$sp == "Sbec")] <- -0.03
    hj_rr[which(analysis$data$sp == "Sxan")] <- 0.01
    hj_rr[which(analysis$data$sp == "Ptom")] <- -0.01
    hj_rr[which(analysis$data$sp == "Smec")] <- 0.015


    # plotting the data 
    gg_theme <- source("./src/utils/gg-theme.graph.R")$value
    

        # Partial residuals line 
        partial_residual_line <-  geom_line(data = partial_lines_data, aes(x = x, y = y, group = factor(x)), alpha = 0.5) 
        # Error ribbons for the plot
        error_ribbon <- geom_ribbon(data = analysis$preds_riskratio, 
                                    aes(x = riskratio, 
                                        y = elevation, 
                                        ymin = CI025, 
                                        ymax = CI975), 
                                    fill = gg_theme$ribbon_color,
                                    alpha = gg_theme$ribbon_alpha)
        # The model line 
        model_line <- geom_line(data = analysis$preds_riskratio, aes(x = riskratio, y = elevation))  
        
        
        partial_data_points <- geom_point(data = analysis$data, aes(y = partials_rr, x = riskratio),
                                              color = themed$selectBlack(),
                                              pch = 21,
                                              fill = themed$selectLightGrey(), 
                                          size = gg_theme$partial_points_size)
        
        raw_data_points <- geom_point(data = analysis$data, 
                                      aes(y = elevation, x = riskratio), 
                                      size = gg_theme$raw_data_points_size,
                                      color = themed$selectRed())
        # axis labels 
        
        ylabel <- ylab(bquote("E(elevation) m asl"^phantom("/"))) 
        xlabel <- xlab(bquote("Innundation sensitivity"^phantom("/")))
        
        # ANOVA values of the explained variation 
        anova_explained_variation <- geom_text(aes(x = 0.12, y = 122, 
                                                label = rr_explaied_variation), 
                                               family = gg_theme$font_family, 
                                               size = gg_theme$anova_text) 
        # species names
        species_name_labels <-  geom_text(data = analysis$data, aes(x = riskratio, y = elevation, label = sp),
                                             size = gg_theme$species_names_size,
                                             nudge_y = vj_rr,
                                             nudge_x = hj_rr,
                                          family = gg_theme$font_family,
                                             fontface = "italic") 
        null_model <- stat_smooth(data = analysis$data, aes(x = riskratio, y = elevation),
                                  se = F, method = "lm", color = themed$selectRed(),
                                  linetype = 2, size = 0.5)
    
    
    p1 <- ggplot(data = analysis$preds_riskratio, aes(x = riskratio, y = elevation)) +
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
    
    # p1 <- ggplot(analysis$preds_riskratio, aes(x = riskratio, y = elevation)) +
    # geom_line(data = partial_lines_data, aes(x = x, y = y, group = factor(x)), alpha = 0.5) +
    # geom_line() +
    # geom_point(data = analysis$data, aes(y = elevation, x = riskratio), color = themed$selectRed()) +
    # geom_ribbon(aes(ymin = CI025, ymax = CI975), alpha = gg_theme$ribbon_alpha) +
    # geom_point(data = analysis$data, aes(y = partials_rr, x = riskratio),
    #            color = themed$selectBlack(),
    #            pch = 21,
    #            fill = themed$selectLightGrey(), size = gg_theme$raw_data_points_size) +
    # geom_text(aes(x = 0.12, y = 120, label = rr_explaied_variation), size = gg_theme$anova_text) +
    # ylab("E(elevation) m asl") +
    # xlab("Inundation sensitivity") +
    # stat_smooth(data = analysis$data, aes(x = riskratio, y = elevation),
    #             se = F, method = "lm", color = themed$selectRed(),
    #             linetype = 2, size = 0.5) +
    # geom_text(data = analysis$data, aes(label = sp),
    #           size = gg_theme$species_names_size,
    #           nudge_y = vj_rr,
    #           nudge_x = hj_rr,
    #           fontface = "italic") +
    # gg_theme$t

    # return the graph
    return(list(plot = p1, analysis = analysis))
})()
