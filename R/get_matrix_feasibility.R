#' Get the feasibility domain of a given matrix
#'
#' this is encapsulated to account for cases in which the matrix is not invertible,
#' and we add a small amount of noise
#'
#' @param A interaction matrix
#' @param bootstrap.replicates replicates in case A is not invertible
#' @param noise.threshold numeric vector with a minimum and a maximum amount of noise
#' to be incorporated in case A is not invertible
#' @param noise.type 'random' or 'targeted'. Noise added to any column at random, or
#' to columns which are linear combinations of the rest
#'
#' @return dataframe with feasibility domain, the condition number (see base function 'kappa'),
#' and other info if A is not invertible.
#'
#' @export
get_matrix_feasibility <- function(A = NULL,
                                   bootstrap.replicates = 1e3,
                                   noise.threshold = c(0,1),
                                   noise.type = c("random","targeted")){

  # is A invertible?
  f <- function(m) class(try(solve(t(m) %*% m),
                             silent = T))[[1]] == "matrix"
  # the operation fails, A is not invertible
  if (f(A) == FALSE) {

    res <- data.frame(replicate = 1:bootstrap.replicates,
                      noise.min = noise.threshold[1],
                      noise.max = noise.threshold[2],
                      invertible = FALSE,
                      condition.number = NA,
                      feasibility.domain = NA)

    # add noise to the matrix, replicated bootstrap.replicates times
    perturbed_matrices <- replicate(n = bootstrap.replicates,
                                    expr = add_noise_matrix(A,
                                                            noise.threshold = noise.threshold,
                                                            noise.type = noise.type),
                                    simplify = FALSE)

    # calculate the average feas.dom. of the replicates
    res$condition.number <- sapply(perturbed_matrices,FUN = kappa)
    # use the bootstrap version of Omega
    res$feasibility.domain <- sapply(perturbed_matrices,FUN = Omega_bootstrap)
    res$invertible <- sapply(perturbed_matrices,FUN = f)

  }else{
    res <- data.frame(replicate = NA,
                      noise.min = NA,
                      noise.max = NA,
                      condition.number = kappa(A),
                      feasibility.domain = Omega_bootstrap(A))
  }
  return(res)
}
