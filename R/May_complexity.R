#' May's complexity metric
#'
#' Calculates the classic complexity metric from May (1972) for a given -unipartite- interaction matrix
#' or connectance, richness, and interaction strength values.
#'
#' @param A numeric matrix. Either A or S,C,sigma must be provided
#' @param S optional, species richness
#' @param C optional, connectance
#' @param sigma optional, standard deviation of interaction strengths
#'
#' @return numeric value
#' @export
#'
#'
May_complexity <- function(A = NULL, S = NULL, C = NULL, sigma = NULL){

  if(!is.null(A)){
    S <- nrow(A)
    C <- connectance(A)
    sigma <- stats::sd(A)
    comp <- sigma * sqrt(S*C)
  }else{
    comp <- sigma * sqrt(S*C)
  }

  return(comp)
}
