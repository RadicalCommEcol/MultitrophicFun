#' calculate binary taxonomic overlap
#' returns a binary matrix with 1 if two species are from the same order
#' and 0 otherwise
#' @param sp.data dataframe with the following columns: sp.ID, order, year,
#' min.month, min.day, max.month, max.day.
#' This dataframe can be built from the function 'filter_sp_taxonomy_phenology'
#'
#' @return binary (0-1) matrix
#' @export
get_taxonomic_overlap <- function(sp.data){
  taxo.matrix <- matrix(0,nrow = nrow(sp.data),ncol = nrow(sp.data),
                        dimnames = list(sp.data$sp.ID,sp.data$sp.ID))
  for(i.row in 1:nrow(taxo.matrix)){
    for(i.col in 1:ncol(taxo.matrix)){
      if(sp.data$order[i.row] == sp.data$order[i.col]){
        taxo.matrix[i.row,i.col] <- 1
      }
    }# for i.col
  }# for i.row

  return(taxo.matrix)

}
