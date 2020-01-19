#' @author James Margrove 
#' @description Summary table of the sapling wood density data. 

(function() {
    # Import data 
    data <- read.csv('./src/data/saplings/sepilok-wood-density-saplings.raw.csv') 
    # Create summery table of the sapling data 
    summary_table <- data.frame(
      species = levels(data$species),
      sp = levels(data$sp),
      mean_wood_density = with(data, tapply(wood_density, sp, mean)),
      sd_
     with(data, tapply(wood_density, sp, length))
    )
    file_name <- "./src/data/saplings/sepilok-wood-density-sapling.table.csv"
    if (!file.exists(file_name)) {
        # write table 
        write.csv(summary_table, file_name, row.names = FALSE)
    }
    # otherwise do nothing and return the summary table 
    return(summary_table)
})()
