
# TODO document

#' get the valid names from a series of guild matrices
#'
#' extracts the names of sp that will form the row
#' built by combining these three matrices. This is because the different bipartite
#' matrices may not be 100% concordant in their species. So, this serves as a kind
#' of filter
#' 
#' @param pp.matrix plant-plant matrix
#' @param ph.matrix plant-herbivores matrix
#' @param pfv.matrix plant-floral visitors matrix
#'
#' @return list with three character vectors, one per guild
#' @export
#'
#' @examples
get_valid_sp <- function(pp.matrix = NULL,
                         ph.matrix = NULL,
                         pfv.matrix = NULL){
  
  # 1 - valid plants
  valid.plants <- NULL
  # if there is a plant-plant matrix, stick with these
  # and discard any other sp from the other matrices
  if(!is.null(pp.matrix)){
    valid.plants <- rownames(pp.matrix)
  }else{
    valid.plants <- unique(c(rownames(pfv.matrix),
                                       rownames(ph.matrix)))
  }
  
  # 2 - valid herbs
  valid.herbs <- NULL
  
  # 2.1 remove invalid plants
  valid.plants.h <- valid.plants[which(valid.plants %in% rownames(ph.matrix))]
  h1 <- ph.matrix[valid.plants.h,]
  sum.herb.int <- apply(h1,2,sum)
  valid.herbs <- names(sum.herb.int)[which(sum.herb.int != 0)]
  
  # 3 - valid fv
  valid.fv <- NULL
  
  # 2.1 remove invalid plants
  valid.plants.fv <- valid.plants[which(valid.plants %in% rownames(pfv.matrix))]
  fv1 <- pfv.matrix[valid.plants.fv,]
  sum.fv.int <- apply(fv1,2,sum)
  valid.fv <- names(sum.fv.int)[which(sum.fv.int != 0)]
  
  return(list(valid.plants,valid.herbs,valid.fv))

}
  
  
  