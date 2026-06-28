#' Load Leroux distribution
#'
#' Make the Leroux CAR distribution functions available in the global
#' environment so that NIMBLE can find them when processing BUGS model code.
#'
#' @returns No return value. The function assigns `dcar_leroux` and `rcar_leroux` to the
#'   global environment.
#'
#' @keywords internal
load_leroux <- function() {

  # Deregister previous Leroux definition, if any.
  suppressWarnings(
    try(nimble::deregisterDistributions("dcar_leroux"), silent = TRUE)
  )

  assign("dcar_leroux", dcar_leroux, envir = globalenv())
  assign("rcar_leroux", rcar_leroux, envir = globalenv())
}
