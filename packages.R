#!/usr/bin/env Rscript --vanilla

if ("R6" %in% rownames(installed.packages()) == FALSE) {
  # Is R6 already installed?
  print("Installing R6 classes package")
  install.packages("R6", repos = "https://stat.ethz.ch/CRAN/") # Install R6 package
}

library(R6) # Load R56 package

# Using some emojis
if ("utf8" %in% rownames(installed.packages()) == FALSE) {
  # Is R6 already installed?
  print("Installing utf8  classes package")
  install.packages("utf8 ", repos = "https://stat.ethz.ch/CRAN/") # Install R6 package
}

library(utf8) # Load R56 package

repos <- "https://stat.ethz.ch/CRAN/"

# Package loader solution.
Packages <- R6Class("Packages",
  public = list(
    initialize = function() {
      self$checkAndInstallPackages()
    },
    packages = list(
      ggpubr = list(name = "ggpubr", repos = repos, dep = TRUE),
      lme4 = list(name = "lme4", repos = repos, dep = TRUE),
      foreach = list(name = "foreach", repos = repos, dep = TRUE),
      MuMIn = list(name = "MuMIn", repos = repos, dep = TRUE),
      nlme = list(name = "nlme", repos = repos, dep = TRUE),
      quantreg = list(name = "quantreg", repos = repos, dep = TRUE),
      arm = list(name = "arm", repos = repos, dep = TRUE),
      gtable = list(name = "gtable", repos = repos, dep = TRUE),
      grid = list(name = "grid", repos = repos, dep = TRUE),
      vegan = list(name = "vegan", repos = repos, dep = TRUE),
      raster = list(name = "raster", repos = repos, dep = TRUE),
      Hmisc = list(name = "Hmisc", repos = repos, dep = TRUE),
      extrafont = list(name = "extrafont", repos = repos, dep = TRUE),
      svglite = list(name = "svglite", repos = repos, dep = TRUE),
      ggplot2 = list(name = "ggplot2", repos = repos, dep = TRUE),
      systemfonts = list(name = "systemfonts", repos = repos, dep = TRUE),
      INLA = list(
        name = "INLA",
        repos = c(getOption("repos"),
          INLA = "https://inla.r-inla-download.org/R/stable"
        ),
        dep = TRUE
      )
    ),
    # Call function to install packages that are not currently on system
    checkAndInstallPackages = function() {
      for (index in 1:length(self$packages)) {
        # Loop for each packages
        package <- self$packages[[index]] # Get the package from class
        if (package$name %in% rownames(installed.packages()) == FALSE) {
          # Is installed?
          # Install package
          install.packages(package$name,
            repos = package$repos,
            dep = package$dep
          )
          print(paste("install package: ",
            package$name,
            sep = ""
          ))
          utf8_print("\U0001f680")
        } else {
          # Package is already installed
          print(
            paste("No worries, you already have this package: ",
              package$name,
              sep = ""
            )
          )
          utf8_print("\U0001f680")
        }
      }
    }
  )
)

# Initialize package loader
Packages$new()
