#' find the structural zeros of a unipartite matrix
#'
#' structural zeros, (or forbidden links) in this very basic definition, are combinations
#' of floral visitors and herbivores. These are assumed not to interact.
#' All other combinations may potentially interact.
#'
#' @param A interaction matrix
#' @param plants names of plants (character vector)
#' @param fv names of floral visitors
#' @param herb names of herbivores
#'
#' @return dataframe with two columns, giving the position in A of the
#' structural zeros.
#' @export
get_structural_zeros <- function(A = NULL,
                                 plants = NULL,
                                 fv = NULL,
                                 herb = NULL){
  sz <- expand.grid(row = 1:nrow(A),col = 1:ncol(A),sz = FALSE)

  for(i.pos in 1:nrow(sz)){
    if((rownames(A)[sz$row[i.pos]] %in% fv &
        colnames(A)[sz$col[i.pos]] %in% herb) |
       (rownames(A)[sz$row[i.pos]] %in% herb &
        colnames(A)[sz$col[i.pos]] %in% fv)){
      sz$sz[i.pos] <- TRUE
    }# if herb-fv
  }# for each matrix position
  return(sz[sz$sz == TRUE,c("row","col")])
}
