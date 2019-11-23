# install these packages
install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)

checkAndInstallPackages = function(){
  if("lme4" %in% rownames(installed.packages())){
    print("install packages")
  } else {
    print("allready has package")
  }
}

checkAndInstallPackages()



