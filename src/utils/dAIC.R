#' @title dAIC
#' @description calculates the difference in AIC between models
#' @param x1 model1
#' @param x2 model2
#' @return delta AIC

dAIC <- function(x1, x2) abs(diff(AIC(x1, x2)[, 2]))

export(dAIC)
