#' Overlap in links/frequency between species pairs
#'
#' given a matrix with focal species in columns and resources in rows, calculates
#' the overlap among the focal species in resource use, considering
#' either binary or quantitative resource use (e.g. number of visits).
#' aij is the effect of j over i, the relative number of resources/visits
#' from i that are shared with j.
#' note that song et al. 2018 ecol.lett. do a column-wise sum, 
#' and that is not correct. In addition, their normalization
#' includes the diagonal elements, which is also incorrect.
#'
#' @param A Matrix of resource use. Focal species in columns, resources in rows. 
#' Can be binary (e.g. an adjacency matrix) or quantitative (e.g. number of visits to different plant species).
#' @param mask Matrix of potential overlap. 0 for pairs that do not overlap,
#' and a value 0-1 for species with different degrees of potential overlap (e.g. because
#' of phenological constraints). Needs to be of the same dimensions as A, otherwise
#' it will fail without warning.
#' @param quant whether we want to obtain binary or quantitative overlap
#' @param relative.diag whether the overlap of a species with itself is 1 (relative.diag = FALSE) or,
#' the overlap of i is relative to the overlap of other species. In this case, assume
#' the species with maximum abundance has an overlap of 1, and the rest are relative to that.
#' This parameter only affects quantitative matrices.
#'
#' @return overlap matrix, of NxN dimensions
#' @export
#'
#' @examples
#' 
#' plant.pol.matrix <- matrix(c(1,1,1,1,1,
#'                              1,1,0,0,0,
#'                              0,0,1,1,1),nrow = 5)
#' 
#' visits <- matrix(c(10,5,3,2,1,
#'                    2,1,0,0,0,
#'                    0,0,1,2,3),nrow = 5)
#'                    
#' get_overlap_matrix(plant.pol.matrix)
#' get_overlap_matrix(visits,quant = TRUE)
#' get_overlap_matrix(visits,quant = TRUE, relative.diag = TRUE)
#' 
get_overlap_matrix <- function(A, mask, quant = FALSE, relative.diag = FALSE){
    if(quant){
        # 1 - intermediate matrix,
        # number of visits shared by each pair
        visits_matrix <- matrix(0,nrow = ncol(A),ncol = ncol(A),
                                dimnames = list(colnames(A),colnames(A)))
        # diag: colsum
        # non-diag: first, select non-zero rows (rows with shared visits)
        # second, sum of the minimums (min number of shared visits)
        for(i in 1:nrow(visits_matrix)){
            for(j in 1:ncol(visits_matrix)){
                if(i ==j){
                    visits_matrix[i,j] <- colSums(A)[i]
                }else{
                    # only selected columns
                    subm <- A[,c(i,j)]
                    # # only non-zero rows
                    # subm <- subm[apply(subm,1,FUN = function(x){all(x != 0)}),]
                    # note: the above is not necessary, actually
                    # sum of the minimum values
                    visits_matrix[i,j] <- sum(apply(subm,1,FUN = function(x){min(x)}))
                }
            }# for j
        }# for i
      
        # standardize
        overlap_matrix <- visits_matrix
        for(i in 1:nrow(overlap_matrix)){
          if(diag(overlap_matrix)[i] != 0){
            overlap_matrix[i,] <- overlap_matrix[i,]/diag(overlap_matrix)[i]
          }
        }
        
        if(relative.diag){
          orig.diag <- diag(visits_matrix)
          weighted.diag <- orig.diag / max(orig.diag)
          diag(overlap_matrix) <- weighted.diag
        }
        
    }else{
        overlap_matrix <- t(A) %*% A
        diag(overlap_matrix) <- 0
        for(i in 1:nrow(overlap_matrix)){
            overlap_matrix[i,] <- overlap_matrix[i,]/sum(overlap_matrix[i,])
        }
        diag(overlap_matrix) <- 1
    }
  
    return(overlap_matrix * mask)
}
