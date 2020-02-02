# Water Inundation Shapes Species Distributions In Tropical Forests


# TL;DR
Go straight to the ```src/results/**``` and open the ```results.pdf``` to view main results in easy format. If cloned, can open ```src/results/results.md``` with RStudio and ```CMD + click``` on the source() file path to navigate around the file structure to find where data came from.

Note for understanding format: most files return an IIFE (Immediately invoked function expression) which allows cleaner importing. 
```
# comment 
(function(){
    # R code goes in here i.e. 
    val <- 1 + 3
    return(val) # retuns
})()
```

# Installing R, and packages.

I am using R version 3.6.2 (2019-12-12) -- "Dark and Stormy Night" on macOS Catalina v10.15.1 installed using `brew cask install R`. Note: I think now xQuartz has to be downloaded separately as it is not part of macOS Catalina (https://cloud.r-project.org/)[https://cloud.r-project.org/].

To install packages used in this project, run the scrip `./packages.R` at the root. This will run a r-bash script that will create all the packages. 

