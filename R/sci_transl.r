#' @title sci_transl
#'
#' @description helps do scientific notation without the e+XX stuff (created by Z. Siders)
#'
#' @param range range of values to convert scientific notation c(min, max)
#'
#' @return list of objects with scale, label, and range
#' 
#' @export

sci_transl <- function(range) {
  x <- gsub("^[[:graph:]]+e\\+", "", formatC(max(range), format = "g"))
  v <- switch(x,
    "01" = "10's",
    "02" = "100's",
    "03" = "1000's",
    "04" = "10's of Thousands",
    "05" = "100's of Thousands",
    "06" = "Millions",
    "07" = "10's of Millions",
    "08" = "100's of Millions",
    "09" = "Billions"
  )
  return(list(
    scale = as.numeric(paste0("1e", x)),
    label = v,
    r = range / as.numeric(paste0("1e", x))
  ))
}
