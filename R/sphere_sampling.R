#' sample random growth rates 
#' 
#' these functions are taken from 
#' "A guideline to study the feasibility domain of multi-trophic and changing ecological communities" by:
#' Chuliang Song, Rudolf P. Rohr, Serguei Saavedra
#' published in: Journal of Theoretical Biology
#' 
#' @param m number of sp
#' @param positive.r positions of sp with strictly positive growth rate
#'
#' @return vector of sampled growth rates
#' @export
#'
#' @examples
sphere_sampling <- function(m,positive.r = NULL) {
  r <- (rnorm(m))
  if(!is.null(positive.r)){
    r[positive.r] <- abs(r[positive.r])
  }
  d <- sqrt(sum(r^2))
  r <- r/d
  return(r)
}