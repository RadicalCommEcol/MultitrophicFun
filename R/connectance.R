#' connectance function for unipartite or bipartite matrices
#'
#' either qualitative or quantitative
#'
#' @param interaction.matrix interaction matrix
#' @param quant whether to calculate quantitative connectance after LDq' in Banasek-Richter et al. 2009
#'
#' @return connectance value (0-1)
#' @export
#'
#' @examples
connectance <- function(interaction.matrix, 
                        quant = FALSE){
  
  # auxiliary function
  hill.diversity <- function(abundances,q = 1){
    abundances <- abundances[abundances != 0]
    abundances <- abundances/sum(abundances)
    R <- length(abundances)
    # hill diversity is not defined for q = 1,
    # but its limit exists and equals
    # the exponential of shannon entropy 
    # (Jost 2006,2007,Tuomisto 2012)
    if(q == 1){
      D <- exp(-sum(abundances*log(abundances)))
    }else{
      mean.p <- (sum(abundances*abundances^(q-1)))^(1/(q-1))
      D <- 1/mean.p
    }
    return(D)
  }

  num.sp <- nrow(interaction.matrix)
  
  # this accomodates bipartite and unipartite matrices
  den <- nrow(interaction.matrix) * ncol(interaction.matrix)
  if(quant){
    # after LDq' in Banasek-Richter et al. 2009
    
    # check consistency of the coefficients, they need to be > 0
    # for calculating the shannon index
    if(sum(interaction.matrix < 0) > 0){
      interaction.matrix <- abs(interaction.matrix)
    }
    
    # 1 - in and out degrees given by the effective number of links
    in.degrees <- apply(interaction.matrix,MARGIN = 2,FUN = hill.diversity)
    out.degrees <- apply(interaction.matrix,MARGIN = 1,FUN = hill.diversity)
    
    # 2 - link density
    LD <- 1/(2*num.sp) * (sum(in.degrees) + sum(out.degrees))
  }else{
    LD <- sum(interaction.matrix != 0)
  }
  # 3 - connectance
  res <- (LD/den)
  return(res)
}