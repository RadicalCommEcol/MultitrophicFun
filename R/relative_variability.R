#' Title
#' Uses Heath (2006) method to compute temporal variability in population abundances,
#' and compares it to the coefficient of variation.
#' DOI:10.1111/j.2006.0030-1299.15067.x
#'
#' @param x vector of population densities
#' @param Diff
#'
#' @return list with two values
#' @export
#'
#' @examples
relative_variability <- function(x,Diff=NULL) {
  # Population variability according to Heath (2006) 10.1111/j.2006.0030-1299.15067.x
  C = combn(x,2)
  for(m in 1:choose(length(x),2)){
    Diff[m] = abs(C[1,m]-C[2,m])/max(C[1,m],C[2,m])
  }
  return(list("Heath_PV"=mean(Diff),
              "Coeff_of_Var"=sd(x)/mean(x)))
}
