rm(list = ls())
library("R6")

Packages <- R6Class("Packages",
          public = list(
            initialize = function() {
                self$checkAndInstallPackages()
            },
          packages = list(
            doSnow = list(name = "doSNOW",
                          repos = "0-Cloud",
                          dep = TRUE
            ),
            lme4 = list(name = "lme4", repos = "0-Cloud", dep = TRUE),
            future = list(name = "future", repos = "0-Cloud", dep = TRUE),
            svglite = list(name = "svglite", repos = "0-Cloud", dep = TRUE),
            ggplot2 = list(name = "ggplot2", repos = "0-Cloud", dep = TRUE),
            flextable = list(name = "flextable", repos = "0-Cloud", dep = TRUE),
            officer = list(name = "officer", repos = "0-Cloud", dep = TRUE),
            systemfonts = list(name = "systemfonts",
            repos = "0-Cloud", dep = TRUE),
            INLA = list(name = "INLA",
                        repos = c(getOption("repos"),
                            INLA = "https://inla.r-inla-download.org/R/stable"),
                        dep = TRUE)
          ),
          checkAndInstallPackages = function() {
            for (index in 1:length(self$packages)) {
                package <- self$packages[[index]]
                if (package$name %in% rownames(installed.packages()) == FALSE) {
                    install.packages(package$name,
                                      repos = package$repos,
                                      dep = package$dep)
                    print(paste("install package: ", package$name, sep = ""))
                } else {
                    print(
                      paste("No worries, you allready has package:",
                        package$name,
                        sep = ""
                      )
                    )
                }
            }
          }
        )
)


Packages$new()