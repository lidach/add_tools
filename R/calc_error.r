#' @title calc_error
#'
#' @description calculate relative error, absolute error, median relative error (MRE), and median absolute relative error (MARE)
#'
#' @param true true value (being compared to)
#' @param est estimate value
#'
#' @return list of each metric
#' 
#' @export
#'

calc_error <- function(true, est) {
  list <- list(abs_error = NA, rel_error = NA, MRE = NA, MARE = NA)
  # absolute error
  list$abs_error <- (est - true)
  # relative error
  list$rel_error <- (est - true) / true
  # median relative error - bias
  list$MRE <- median(list$rel_error, na.rm = TRUE)
  # median relative error - precision
  list$MARE <- median(abs(list$rel_error), na.rm = TRUE)

  return(list)
}
