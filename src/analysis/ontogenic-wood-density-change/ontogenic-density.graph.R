# graphing
(function() {
    # load the analysis of the data 
    analysis <- source("./src/analysis/ontogenic-wood-density-change/ontogenic-density.analysis.R")$value

    # Load R6 classes library
    library(R6)

    # Set drawing settings
    Settings <- R6Class("Settings",
                        public = list(
                            initialize = function() {
                                # create height
                                self$height = self$width / 3 * 2

                                # create the x values 
                                self$x = list(
                                    max = self$width - self$border,
                                    min = self$border,
                                    mid = self$width / 2
                                )

                                # create the y values 
                                self$y = list(
                                    max = self$height - self$border,
                                    min = self$border,
                                    mid = self$height / 2
                                )
                                # create the data frame 
                                self$data <- data.frame(
                                    x = c(self$x$min, self$x$max, self$x$mid, self$x$min),
                                    y = c(self$y$max, self$y$max, self$y$min, self$y$max),
                                    lab = c("Seedling", "Adult", "Sapling", NA),
                                    vj = c(1, 1, -1, NA) * self$vj
                                    )

                            },
                            width = 120,
                            height = NULL, # Initial value is null 
                            border = 20,
                            vj = 2,
                            x = NULL, # Initial values to start 
                            y = NULL, # Initial values to start
                            data = NULL # Initial values to start 
                        )
                    )



    settings <- Settings$new()

    ggplot(settings$data, aes(x = x, y = y)) +
        geom_line() +
        geom_text(aes(x = x, y = y, label = lab), nudge_y = settings$data$vj) +
        xlim(0, settings$width) +
        ylim(0, settings$height) +
        theme_void()

})()
