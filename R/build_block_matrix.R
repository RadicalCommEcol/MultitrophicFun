#' build an empty block matrix
#'
#' build the skeleton of a block matrix given
#' names of plants, herbivores, and floral visitors
#'
#' @param plants character vector
#' @param herbivores character vector
#' @param floral.visitors character vector
#'
#' @return block matrix with appropriate names, NA in their elements
#' @export
build_block_matrix <- function(plants,herbivores,floral.visitors){
  block.matrix <- matrix(NA,nrow = sum(length(plants),length(herbivores),
                                      length(floral.visitors)),
                         ncol = sum(length(plants),length(herbivores),
                                    length(floral.visitors)),
                         dimnames = list(c(plants,herbivores,floral.visitors),
                                         c(plants,herbivores,floral.visitors)))
  return(block.matrix)
}


