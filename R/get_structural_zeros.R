#' find the structural zeros of a unipartite matrix
#'
#' structural zeros, (or forbidden links) in this very basic definition, are combinations
#' of floral visitors and herbivores. These are assumed not to interact.
#' Furthermore, if a separate dataframe is given, these are incorporated. This is because
#' I compiled inter-guild structural zeros elsewhere for the caracoles matrices,
#' but need to join them with the basic ones.
#'
#' @param A interaction matrix
#' @param plants names of plants (character vector)
#' @param fv names of floral visitors
#' @param herb names of herbivores
#' @param dfz optional dataframe with columns "plant", "animal", "structural.zero"
#'
#' @return dataframe with two columns, giving the position in A of the
#' structural zeros.
#' @export
get_structural_zeros <- function(A = NULL,
                                 plants = NULL,
                                 fv = NULL,
                                 herb = NULL,
                                 dfz = NULL){
  sz <- expand.grid(row = 1:nrow(A),col = 1:ncol(A),sz = FALSE)

  for(i.pos in 1:nrow(sz)){
    if((rownames(A)[sz$row[i.pos]] %in% fv &
        colnames(A)[sz$col[i.pos]] %in% herb) |
       (rownames(A)[sz$row[i.pos]] %in% herb &
        colnames(A)[sz$col[i.pos]] %in% fv)){
      sz$sz[i.pos] <- TRUE
    }else if(!is.null(dfz)){
      # is there a structural zero in dfz for this pair?
      my.pos <- which((dfz$plant == rownames(A)[sz$row[i.pos]] &
                        dfz$animal == colnames(A)[sz$col[i.pos]]) |
                        (dfz$animal == rownames(A)[sz$row[i.pos]] &
                           dfz$plant == colnames(A)[sz$col[i.pos]]))
      if(length(my.pos) == 1){
        if(dfz$structural.zero[my.pos] == TRUE){
          sz$sz[i.pos] <- TRUE
        }
      }
    }# if fv-herb, or else, if I need to check dfz
  }# for each matrix position
  return(sz[sz$sz == TRUE,c("row","col")])
}
