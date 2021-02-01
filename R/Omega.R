#' normalized feasibility
#'
#' these functions are taken from
#' "A guideline to study the feasibility domain of multi-trophic and changing ecological communities" by:
#' Chuliang Song, Rudolf P. Rohr, Serguei Saavedra
#' published in: Journal of Theoretical Biology
#'
#' @param alpha interaction matrix
#'
#' @return numeric value
#' @export
Omega <- function(alpha) {
  S <- nrow(alpha)
  omega <- function(S, Sigma) {
    m <- matrix(0, S, 1)
    a <- matrix(0, S, 1)
    b <- matrix(Inf, S, 1)
    d <- try(mvtnorm::pmvnorm(lower = rep(0, S),
                     upper = rep(Inf, S),
                     mean = rep(0, S), sigma = Sigma),silent = TRUE)
    out <- ifelse(class(d) == "try-error",0,d[1]^(1 / S))
    return(out)
  }
  #   if (length(which(diag(alpha) == 0)) == 0) {
  #     Sigma <- chol2inv(alpha, size = NCOL(alpha), LINPACK = FALSE)
  #     return(omega(S, Sigma))
  #   }
  #   else {
  f <- function(m) class(try(solve(t(m) %*% m,tol = 1e-17), silent = T)) == "matrix"
  if (f(alpha) == FALSE) {
    return(0)
  }
  else {
    Sigma <- solve(t(alpha) %*% alpha,tol = 1e-17)
    return(omega(S, Sigma))
  }
  #   }
  # }
}
