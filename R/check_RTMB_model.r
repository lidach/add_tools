#' @title check_RTMB_model
#'
#' @description check convergence, gradient, Hessian, and identifiability
#'
#' @param obj objective function of RTMB model
#' @param opt result from nlminb optimizer
#'
#' @return list of model diagnostics (convergence, gradient, Hessian, identifiability)
#' 
#' @export
#'

check_RTMB_model <- function(
    obj,
    opt) {
  res <- list()
  res$convergence <- opt$convergence
  res$max_gradient <- max(abs(obj$gr(opt$par)))
  final_gradient <- obj$gr(opt$par)

  if (res$convergence == 1) {
    message("Model did not converge! (ノಠ益ಠ)ノ彡 ┻━┻")
  }
  if (res$max_gradient > 0.001) {
    res$whichbad_params <- data.frame(
      parameters = names(opt$par),
      MLE = as.numeric(opt$par),
      gradient = as.numeric(final_gradient),
      parameter_check = c(ifelse(abs(as.numeric(final_gradient)) > 0.001, "Bad", "OK"))
    )
    message("Gradients are high, please improve optimization!")
    return(res)
  } else {
    res$whichbad_params <- "All parameter gradients look good!"
  }
  # look at fixed estimated parameters
  if (length(obj$env$random) == 0) {
    fixed_obj <- obj$env$last.par.best
  } else {
    fixed_obj <- obj$env$last.par.best[-c(obj$env$random)]
  }
  # extract parameters and uncertainty
  res$Hess <- optimHess(par = fixed_obj, fn = obj$fn, gr = obj$gr)
  if (is.nan(max(res$Hess))) {
    res$hess_status <- "The hessian was not invertible."
  } else {
    res$eigen <- eigen(res$Hess)
    res$whichbad_eigen <- which(res$eigen$values < sqrt(.Machine$double.eps))
    # check for parameters
    if (length(res$eigen$vectors[, res$whichbad_eigen]) > 0) {
      rowmax <- apply(as.matrix(res$eigen$vectors[, res$whichbad_eigen]),
        MARGIN = 1, FUN = function(x) {
          max(abs(x))
        }
      )
      res$whichbad_eigen <- data.frame(
        "Parameters" = names(obj$par),
        "MLE" = fixed_obj,
        "Parameter_check" = ifelse(rowmax > 0.001, "Bad", "OK")
      )
    } else {
      res$whichbad_eigen <- "All parameters are identifiable."
    }
  }
  if (res$convergence == 0 & res$max_gradient < 0.001 & class(res$whichbad_eigen) == "character") {
    message("Model diagnostics consistent with convergence.")
    res <- list()
    res$convergence <- opt$convergence
    res$max_gradient <- max(abs(obj$gr(opt$par)))
    res$message <- "Good to go!"
  }
  return(res)
}
