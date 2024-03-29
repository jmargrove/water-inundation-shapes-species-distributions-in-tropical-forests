import(ggplot2)
import("./src/utils/theme.R")

fig3_theme <- list(
  t = theme(text = element_text(size = 12)) +
    theme_bw() +
    theme(text = element_text(family = "Times")) +
    theme(
      plot.margin = unit(c(2, 2, 1, 1), units = "mm"),
      axis.text.x = element_text(vjust = 0.5)
    ), # superscript causing pain
  t2 = theme(text = element_text(size = 12)) +
    theme_classic() +
    theme(text = element_text(family = "Times")) +
    theme(
      plot.margin = unit(c(2, 2, 1, 1), units = "mm"),
      axis.text.x = element_text(vjust = 0.5)
    ),

  # additional params
  anova_text = 3,
  species_names_size = 2.75,
  raw_data_points_size = 1,
  partial_points_size = 2,
  ribbon_alpha = 0.4,
  ribbon_color = themed$selectMedGrey(),
  font_family = "Times",
  axis_size = 1
) # spaces out the plots



export(fig3_theme)
