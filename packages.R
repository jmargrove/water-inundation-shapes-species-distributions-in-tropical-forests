#!/usr/bin/env Rscript --vanilla
rm(list = ls())

r6 <- "R6"
if (r6 %in% rownames(installed.packages()) == FALSE){ # is R6 already installed?
  print('Installing R6 classes package')
  install.packages('R6', repos = 'https://stat.ethz.ch/CRAN/') # install R6 package 
}
library(R6) # load R56 package

if ("utf8" %in% rownames(installed.packages()) == FALSE){ # is R6 already installed?
  print('Installing utf8  classes package')
  install.packages('utf8 ', repos = 'https://stat.ethz.ch/CRAN/') # install R6 package 
}
library(utf8) # load R56 package

Packages <- R6Class("Packages",
          public = list(
            initialize = function() {
                self$checkAndInstallPackages()
            },
          packages = list(
            lme4 = list(name = "lme4", repos = "https://stat.ethz.ch/CRAN/", dep = TRUE),
            nlme = list(name = "nlme", repos = "https://stat.ethz.ch/CRAN/", dep = TRUE),
            quantreg = list(name = "quantreg", repos = "https://stat.ethz.ch/CRAN/", dep = TRUE),
            arm = list(name = "arm", repos = "https://stat.ethz.ch/CRAN/", dep = TRUE),
            gtable = list(name = "gtable", repos = "https://stat.ethz.ch/CRAN/", dep = TRUE),
            grid = list(name = "grid", repos = "https://stat.ethz.ch/CRAN/", dep = TRUE),
            vegan = list(name = "vegan", repos = "https://stat.ethz.ch/CRAN/", dep = TRUE),
            raster = list(name = "raster", repos = "https://stat.ethz.ch/CRAN/", dep = TRUE),
            Hmisc = list(name = "Hmisc", repos = "https://stat.ethz.ch/CRAN/", dep = TRUE),
            extrafont= list(name = "extrafont", repos = "https://stat.ethz.ch/CRAN/", dep = TRUE),
            svglite = list(name = "svglite", repos = "https://stat.ethz.ch/CRAN/", dep = TRUE),
            ggplot2 = list(name = "ggplot2", repos = "https://stat.ethz.ch/CRAN/", dep = TRUE),
            systemfonts = list(name = "systemfonts", repos = "https://stat.ethz.ch/CRAN/", dep = TRUE),
            INLA = list(name = "INLA",
                        repos = c(getOption("repos"),
                            INLA = "https://inla.r-inla-download.org/R/stable"),
                        dep = TRUE)
          ),
          # call function to install packages that are not currently on system
          checkAndInstallPackages = function() {
            for (index in 1:length(self$packages)) { # loop for each packages 
                package <- self$packages[[index]] # get the package from class
                if (package$name %in% rownames(installed.packages()) == FALSE) { # isInstalled?
                  # install package 
                    install.packages(package$name,
                                      repos = package$repos,
                                      dep = package$dep)
                    print(paste("install package: ",
                    package$name,
                    sep = "")
                    )
                    utf8_print("\U0001f680")
                } else {
                  # package is already installed 
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


Packages$new()

