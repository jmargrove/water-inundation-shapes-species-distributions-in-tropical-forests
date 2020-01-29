# Organise the data in to 1/4 ha plots for analysis 
(function() {
    # Organise the data frame 
    setPlotData <- function() {
        # load the data module 
        data <- source('./src/data/wood-density-elevation-distribution/add-density-to-plot.dataframe.R')$value
        # 
        # plot extent 
        plot_extent <- read.csv('./src/data/sepilok-plot/plot-extent.table.csv')

        # plot distances  
        plotXlength = plot_extent[2, "latitude"] - plot_extent[1, "latitude"]
        plotYlength = plot_extent[3, "longitude"] - plot_extent[1, "longitude"]

        # cut equally @ 50m for 1/4 ha forest plots 
        nxcuts <- round(plotXlength / 50)
        nycuts <- round(plotYlength / 50)

        # Figure out where to cut the data 
        Xbreaks <- seq(plot_extent[1, "longitude"], plot_extent[3, "longitude"], length = nxcuts)
        Ybreaks <- seq(plot_extent[1, "latitude"], plot_extent[2, "latitude"], length = nycuts)

        # place the cuts in the data frame 
        data$XCut <- cut(data$longitude, breaks = Xbreaks)
        data$YCut <- cut(data$latitude, breaks = Ybreaks)

        # Calculate average, elevation and wood density per square 
        elevation_mean <- as.vector(unlist(with(data,
                               tapply(elevation, list(XCut, YCut),
                                      mean,
                                      na.rm = TRUE)
                               )
                          )
                   )
        density_mean <- as.vector(unlist(with(data,
                               tapply(wood_density, list(XCut, YCut),
                                      mean,
                                      na.rm = TRUE)
                               )
                          )
                   )
        n <- as.vector(unlist(with(data,
                               tapply(latitude, list(XCut, YCut),
                                      function(x) {
                                        length(!is.na(x))
                                      }
                                      )
                               )
                          )
                   )

        return(data.frame(elevation_mean, density_mean, n))
    }


    setReubenData = function() {
        reubens_plot_data <- source("./src/data/wood-density-elevation-distribution/add-density-to-reubens-plot.dataframe.R")$value
        res_data <- data.frame(elevation_mean = 0, density_mean = 0, n = 0)[0,] # Create results data frame 

        for (forest in levels(reubens_plot_data$Forest)) {
            for (quarter in as.character(1:4)) {
                subplot <- subset(reubens_plot_data, Forest == forest & ha4plot == quarter)

                xr <- range(subplot$longitude)
                yr <- range(subplot$latitude)

                mid_point <- list(
          lat = mean(range(subplot$latitude)),
          lng = mean(range(subplot$longitude))
        )

                sub_plot_quarter1 <- subset(subplot,
                           longitude > xr[1] &
                             longitude < mid_point$lng &
                             latitude > yr[1] &
                             latitude < mid_point$lat
                           )

                sub_plot_quarter2 <- subset(subplot,
                           longitude > xr[1] &
                             longitude < mid_point$lng &
                             latitude < yr[2] &
                             latitude > mid_point$lat
                           )

                sub_plot_quarter3 <- subset(subplot,
                           longitude < xr[2] &
                             longitude > mid_point$lng &
                             latitude < yr[2] &
                             latitude > mid_point$lat
                           )

                sub_plot_quarter4 <- subset(subplot,
                           longitude < xr[2] &
                             longitude > mid_point$lng &
                             latitude > yr[1] &
                             latitude < mid_point$lat
                           )

                sub_plot_quarter1$Q <- rep("Q1", nrow(sub_plot_quarter1))
                sub_plot_quarter2$Q <- rep("Q2", nrow(sub_plot_quarter2))
                sub_plot_quarter3$Q <- rep("Q3", nrow(sub_plot_quarter3))
                sub_plot_quarter4$Q <- rep("Q4", nrow(sub_plot_quarter4))


                quaters <- rbind(sub_plot_quarter1,
                         sub_plot_quarter2,
                         sub_plot_quarter3,
                         sub_plot_quarter4)

                new_row <- data.frame(elevation_mean = with(quaters, tapply(elevation, Q, mean)),
                              density_mean = with(quaters, tapply(wood_density, Q, mean)),
                              n = with(quaters, tapply(longitude, Q, function(x) { length(!is.na(x)) })))

                res_data <- rbind(res_data, new_row)
            }
        }

        return(data.frame(elevation_mean = res_data$elevation_mean,
                      density_mean = res_data$density_mean,
                      n = res_data$n)
           )
    }

    # Calculate data frame 
    large_plot_data <- setPlotData() # large 160ha forest plot 
    reuben_plot_data <- setReubenData() # Reubens plots 
    combined_data <- rbind(large_plot_data, reuben_plot_data)
    return(combined_data[!is.na(combined_data$d),])

})()







