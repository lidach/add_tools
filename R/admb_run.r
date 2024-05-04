#' @title admb_run
#'
#' @description run ADMB model
#'
#' @param fn ADMB model name
#'
#' @return ADMB files
#'
#' @export
#'

admb_run <- function(fn, verbose = FALSE, mcmc = FALSE, mcmc.opts = mcmc.control(),
                     profile = FALSE, extra.args = "",
                     admb_errors = c("stop", "warn", "ignore")) {
  admb_errors <- match.arg(admb_errors)
  args <- ""
  if (mcmc) {
    if (is.null(mcmc.opts$mcmcpars)) stop("you must specify at least one parameter in 'mcmc.opts$mcmcpars' (see ?mcmc.control)")
    args <- paste(args, mcmc.args(mcmc.opts))
  }
  if (profile) args <- paste(args, "-lprof")
  if (!missing(extra.args)) {
    args <- paste(args, extra.args)
  }
  if (verbose) cat("running compiled executable with args: '", args, "'...\n")

  outfn <- paste(fn, "out", sep = ".")

  if (.Platform$OS.type == "windows") {
    cmdname <- paste(fn, ".exe", sep = "")
    shellcmd <- shell
  } else {
    cmdname <- paste("./", fn, sep = "")
    shellcmd <- system
  }
  if (!file.exists(cmdname)) stop("executable ", cmdname, " not found: did you forget to compile it?")
  res <- shellcmd(paste(cmdname, args, ">", outfn), intern = TRUE)

  outfile <- readLines(paste(fn, ".out", sep = ""))
  ## replace empty res with <empty> ?
  if (mcmc) {
    ## write MC info to disk so it will be retrievable ...
    mcinfofile <- file(paste(fn, "mcinfo", sep = "."), "w")
    mctab <- unlist(mapply(function(x, y) {
      c(paste("# ", x), if (is.null(y)) "" else paste(y, collapse = " "))
    }, names(mcmc.opts), mcmc.opts))
    writeLines(mctab, paste(fn, "mcinfo", sep = "."))
  }
  if (verbose) {
    cat("Run output:\n", res, "\n", sep = "\n")
    cat(outfile, "\n", sep = "\n")
  }
  if (length(grep("^Error", outfile) > 0)) {
    runerrmsg <- "errors detected in ADMB run: run with verbose=TRUE to view"
    if (admb_errors == "stop") stop(runerrmsg) else if (admb_errors == "warn") warning(runerrmsg)
  }
  invisible(res)
}
