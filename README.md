# Water Inundation Shapes Species Distributions In Tropical Forests

## TL;DR

Go straight to the ```src/results/**``` and open the ```results.pdf``` to view main results in easy format. If cloned, can open ```src/results/results.md``` with RStudio and ```CMD + click``` on the source() file path to navigate around the file structure to find where data came from.

## Installing R, and packages.

I am using R version 3.6.2 (2019-12-12) -- "Dark and Stormy Night" on macOS Catalina v10.15.1 installed using `brew cask install R`. Note: I think now xQuartz has to be downloaded separately as it is not part of macOS Catalina (https://cloud.r-project.org/)[https://cloud.r-project.org/].

To install packages used in this project, run the scrip `./packages.R` at the root. This will run a r-bash script that will create all the packages. 

## Notes for understanding the code

### Use of immediately invoked function expressions
Files often return an IIFE (Immediately invoked function expression), for instance:
```
# comment 
(function(){
    # R code goes in here i.e. 
    a <- 1
    b <- 3
    val <- a + b

    result <- list(val = val)
    return(val) # retuns
})()
```
I am using this format to avoid importing unnecessary variables ```a``` and ```b``` in the workspace when using ```source()```, and to take advantage of closures to keep variables with the same name separate. Allowing for example to use the ```data``` variable name in all files where there is a main data frame for analysis.

```
result <- source('./file/path)$value
result$val # is equal to 4
```