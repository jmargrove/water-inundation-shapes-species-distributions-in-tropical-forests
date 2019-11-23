# A function to handle the setting of working dirs 
handleWd <- function(localDir = '') {
  firstChar <- substr(localDir, 1, 1)
  if(firstChar != '/'){
    warning("There is no forward slash!")
    stop()
  }
  wd <- getwd()
  thisDir = paste(wd, localDir, sep = "")
  setwd(thisDir)
}
