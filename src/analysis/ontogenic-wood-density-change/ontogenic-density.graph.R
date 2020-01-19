# graphing
(function() {
    # load the analysis of the data 
    analysis <- source("./src/analysis/ontogenic-wood-density-change/ontogenic-density.analysis.R")$value

    # Load R6 classes library
    library(R6)

    # Define the class for the settings
        Settings <- R6Class("Settings",
                        public = list(
                            
                            initialize = function(analysis) {
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
                                    lab = c(" Seedling ", " Adult ", " Sapling ", NA),
                                    vj = c(1, 1, -1, NA) * self$vj
                                    )
                                
                                # The center of the graph
                                self$center <- data.frame(
                                    x = self$width / 2, 
                                    y = self$height / 2
                                    )
                                
                                # the r2 position 
                                self$r_position = data.frame(
                                    x = c(25, self$x$mid, 95), 
                                    y = c(40, 65, 40), 
                                    r = round(c(analysis$r2$r2[2], analysis$r2$r2[1], analysis$r2$r2[3]), 2)
                                )
                            },
                            width = 120,
                            height = NULL, # Initial value is null 
                            center = NULL, 
                            border = 20,
                            vj = 2,
                            x = NULL, # Initial values to start 
                            y = NULL, # Initial values to start
                            data = NULL, # Initial values to start 
                            r_position = NULL,
                            plot = function(){
                                p1 <- ggplot(self$data) +
                                    geom_line(data = self$data[c(1, 2), ], aes(x = x, y = y)) +
                                    geom_line(data = self$data[c(2, 3), ], aes(x = x, y = y)) +
                                    geom_line(data = self$data[c(3, 4), ], aes(x = x, y = y)) +
                                    geom_text(aes(x = x, y = y, label = lab), nudge_y = self$data$vj) +
                                    xlim(0, self$width) +
                                    ylim(0, self$height) +
                                    geom_text(data = self$center, aes(x = x, y = y, label = " R2 "))  +
                                    geom_text(data = self$r_position, aes(x = x, y = y, label = r)) +
                                    theme_void()
                                
                                return(p1)
                            }
                        )
                    )


    # Instantiate the class 
    settings <- Settings$new(analysis = analysis)

    # return the object withe the plot and anlaysis parameters 
    return (
        list(
            plot = settings$plot(), 
            analysis = analysis
        )
    )

})()
