#' species-level metrics
#'
#' different functions with the same structure, metric is in function name
#'
#' @param A interaction matrix
#' @param sp.names which sp to calculate, must be in names of A
#'
#' @return dataframe
#' @export
intraguild_intraspecific <- function(A, sp.names){

  result <- data.frame(sp = rownames(A),
                       value = diag(A), row.names = NULL)

  return(result)

}
