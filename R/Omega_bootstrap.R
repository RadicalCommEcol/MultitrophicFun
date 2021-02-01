#' Bootstrap version of feasibility calculation
#'
#' Motivated because the original function 'Omega' is
#' not deterministic
#'
#' @param alpha interaction matrix
#' @param replicates number of bootstrap replicates
#'
#' @return normalized feasibility
#' @export
#'
#' @examples
Omega_bootstrap <- function(alpha, replicates = 1e2) {

  # helper functions
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

  f <- function(m) class(try(solve(t(m) %*% m,tol = 1e-17),
                             silent = T)) == "matrix"

  S <- nrow(alpha)
  solvable <- f(alpha)
  if (!solvable) {
    return(0)
  }
  else {
    Sigma <- solve(t(alpha) %*% alpha,tol = 1e-17)
    res <- 0
    for(i in 1:replicates){
      res <- res + omega(S, Sigma)
    }
    res <- res/replicates
    return(res)
  }
}
