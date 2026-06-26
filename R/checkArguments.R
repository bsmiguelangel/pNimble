#' Check input arguments
#'
#' Check that the main arguments passed to `pNimble()` have the expected type,
#' length and basic structure.
#'
#' @param inits Initial values for the MCMC chains.
#' @param nchains Number of MCMC chains to run.
#' @param seeds Numeric or integer vector with one seed per chain.
#' @param summary Logical value indicating whether posterior summaries should be
#'   calculated.
#' @param monitors Character vector with the names of the model variables to
#'   monitor.
#' @param ntfyAccount Optional account or topic used by `notify()` to send a
#'   notification when the model run finishes.
#' @param email Logical value indicating whether an email notification should be
#'   sent.
#' @param WAIC Logical value indicating whether WAIC should be calculated.
#' @param HMC Logical value indicating whether HMC sampling should be used.
#' @param replaceSamplers Optional list used to replace default NIMBLE samplers.
#' @param parallel Logical value indicating whether chains should be run in
#'   parallel.
#' @param control.model Optional list of arguments passed to
#'   `nimble::nimbleModel()`.
#' @param control.compile Optional list of arguments passed to
#'   `nimble::compileNimble()`.
#' @param control.configure Optional list of arguments passed to
#'   `nimble::configureMCMC()` or `nimbleHMC::configureHMC()`.
#' @param control.build Optional list of arguments passed to
#'   `nimble::buildMCMC()`.
#'
#' @returns `NULL`. Stops execution if any argument is invalid.
#'
#' @keywords internal
checkArguments <- function(inits, nchains, seeds, summary, monitors,
                           ntfyAccount, email, WAIC, HMC, replaceSamplers,
                           parallel, control.model, control.compile,
                           control.configure, control.build) {

  if (!is.null(inits) && !is.function(inits)) {
    stop("inits argument should be a function.")
  }

  if (!is.numeric(nchains) || length(nchains) != 1 || is.na(nchains) ||
      nchains < 1 || nchains %% 1 != 0) {
    stop("nchains argument should be a positive integer.")
  }

  if (!is.numeric(seeds) || anyNA(seeds)) {
    stop("seeds argument should be a numeric vector without missing values.")
  }

  if (length(seeds) != nchains) {
    stop("length of seeds argument does not match nchains.")
  }

  if (!is.logical(summary) || length(summary) != 1 || is.na(summary)) {
    stop("summary argument should be a single logical value.")
  }

  if (!is.null(monitors) && (!is.character(monitors) || anyNA(monitors))) {
    stop("monitors argument should be a character vector without missing values.")
  }

  if (!is.null(ntfyAccount) &&
      (!is.character(ntfyAccount) || length(ntfyAccount) != 1 || is.na(ntfyAccount))) {
    stop("ntfyAccount argument should be either NULL or a single character value.")
  }

  if (!is.logical(email) || length(email) != 1 || is.na(email)) {
    stop("email argument should be a single logical value.")
  }

  if (!is.logical(WAIC) || length(WAIC) != 1 || is.na(WAIC)) {
    stop("WAIC argument should be a single logical value.")
  }

  if (!is.logical(HMC) || length(HMC) != 1 || is.na(HMC)) {
    stop("HMC argument should be a single logical value.")
  }

  if (!is.null(replaceSamplers) && !is.list(replaceSamplers)) {
    stop("replaceSamplers argument should be a list.")
  }

  if (!is.logical(parallel) || length(parallel) != 1 || is.na(parallel)) {
    stop("parallel argument should be a single logical value.")
  }

  if (!is.list(control.model)) {
    stop("control.model argument should be a list.")
  }

  if (!is.null(control.compile) && !is.list(control.compile)) {
    stop("control.compile argument should be either NULL or a list.")
  }

  if (!is.null(control.configure) && !is.list(control.configure)) {
    stop("control.configure argument should be either NULL or a list.")
  }

  if (!is.null(control.build) && !is.list(control.build)) {
    stop("control.build argument should be either NULL or a list.")
  }
}
