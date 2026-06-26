utils::globalVariables(c("dnorm", "nimNumeric", "nimStop", "pow", "returnType"))

#' Load Leroux distribution
#'
#' Define the Leroux CAR distribution functions used by NIMBLE.
#'
#' @returns `NULL`. The function defines `dcar_leroux` and `rcar_leroux` in the
#'   global environment.
#'
#' @keywords internal
load_leroux <- function() {

  dcar_leroux <- nimble::nimbleFunction(
    name = "dcar_leroux",
    run = function(x = double(1),        # Spatial random effect (vector)
                   rho = double(0),      # Amount of spatial dependence (scalar)
                   sd.theta = double(0), # Standard deviation (scalar)
                   Lambda = double(1),   # Eigenvalues of matrix D - W
                   from.to = double(2),  # Matrix of distinct pairs of neighbors from.to[, 1] < from.to[, 2]
                   log = integer(0, default = 0)) {

      # Number of small areas
      NMuni <- dim(x)[1]

      # Number of distinct pairs of neighbors
      NDist <- dim(from.to)[1]

      # Required vectors
      x.from <- nimNumeric(NDist)
      x.to <- nimNumeric(NDist)
      for (Dist in 1:NDist) {
        x.from[Dist] <- x[from.to[Dist, 1]]
        x.to[Dist] <- x[from.to[Dist, 2]]
      }

      logDens <- sum(dnorm(x[1:NMuni], mean = 0, sd = sd.theta * pow(1 - rho, -1/2), log = TRUE)) -
        NMuni/2 * log(1 - rho) +  sum(log(rho * (Lambda[1:NMuni] - 1) + 1))/2 -
        pow(sd.theta, -2) * rho * sum(pow(x.from[1:NDist] - x.to[1:NDist], 2))/2

      if (log) {
        return(logDens)
      } else {
        return(exp(logDens))
      }

      returnType(double())
    },
    buildDerivs = list(run = list(ignore = "Dist"))
  )

  rcar_leroux <- nimble::nimbleFunction(
    name = "rcar_leroux",
    run = function(n = integer(0),
                   rho = double(0),
                   sd.theta = double(0),
                   Lambda = double(1),
                   from.to = double(2)) {

      returnType(double(1))
      nimStop("user-defined distribution dcar_leroux provided without random generation function.")
      x <- nimNumeric(length(Lambda))
      return(x)
    }
  )

  assign("dcar_leroux", dcar_leroux, envir = globalenv())
  assign("rcar_leroux", rcar_leroux, envir = globalenv())
}
