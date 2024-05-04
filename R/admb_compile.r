#' @title admb_compile
#'
#' @description compile ADMB model
#'
#' @param fn ADMB model name
#'
#' @return ADMB executable files
#'
#' @export
#'

admb_compile <- function(fn, safe = FALSE, re = FALSE, verbose = FALSE,
                         admb_errors = c("stop", "warn", "ignore")) {
  admb_errors <- match.arg(admb_errors)
  if (!file.exists(fn2 <- paste(fn, "tpl", sep = "."))) {
    stop("can't find TPL file ", fn2)
  }
  test <- try(system("admb", intern = TRUE), silent = TRUE)
  if (inherits(test, "try-error")) stop("base admb command failed: run setup_admb(), or check ADMB installation")
  args <- ""
  if (re) args <- "-r"
  if (safe) args <- paste(args, "-s")
  if (verbose) cat("compiling with args: '", args, "' ...\n")
  res0 <- system(paste("admb", args, fn, " 2>", paste(fn, ".cout", sep = "")),
    intern = TRUE
  )
  coutfile <- readLines(paste(fn, ".cout", sep = ""))
  if (verbose) {
    cat("compile output:\n", res0, "\n")
    cat("compile log:\n")
    cat(coutfile, sep = "\n")
  }
  ## sorting out the lines that come BEFORE the warnings
  admb_warnerr_index <- grepl("warning|error", coutfile)
  csplit <- split(coutfile, head(c(0, cumsum(admb_warnerr_index)), -1))
  wchunks <- which(sapply(lapply(csplit, grep, pattern = "warning"), length) > 0)
  echunks <- which(sapply(lapply(csplit, grep, pattern = "error"), length) > 0)
  if (length(wchunks) > 0) {
    if (!verbose) {
      ## figure we don't need these warnings
      ## if we are spitting them out above anyway
      admb_warnings <- paste("from ADMB:", unlist(csplit[wchunks]))
      sapply(admb_warnings, warning)
    }
    csplit <- csplit[-wchunks]
  }
  Sys.chmod(fn, mode = "0755")
  if (length(echunks) > 0) {
    comperrmsg <- "errors detected in compilation: run with verbose=TRUE to view"
    if (admb_errors == "stop") stop(comperrmsg) else if (admb_errors == "warn") warning(comperrmsg)
  }
}
