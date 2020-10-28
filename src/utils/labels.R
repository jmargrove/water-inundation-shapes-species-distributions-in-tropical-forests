#' Labels for ggplot axis
#' lab list holds the labels
#' functions return the ggplot2 x/y lab function call for addition to the project

import("ggplot2") # ensure ggplot2 is loaded

Labels <- R6Class("Labels",
  public = list(
    lab = list(
      species = "Species",
      wood_density = expression(paste("Wood Density g cm")^"-3")
    ),
    xLabSpecies = function() {
      return(xlab(self$lab$species))
    },
    yLabWoodDensity = function() {
      return(ylab(self$lab$wood_density))
    }
  )
)

# labels class for use.
labels <- Labels$new()
