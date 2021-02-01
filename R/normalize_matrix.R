
#' Normalize a series of matrices in a list
#'
#' Normalize according to a provided max.value or according
#' to the maximum value observed across all matrices in the list
#'
#' @param matrix.list list of matrices, potentially nested
#' @param max.value optional value for normalizing
#'
#' @return list with the same structure as matrix.list, containing normalized matrices
#' @export
#'
#' @examples
normalize_matrix <- function(matrix.list = NULL, max.value = NULL){
  my.max <- ifelse(is.null(max.value),max(unlist(matrix.list)),max.value)
  
  result.list <- list()
  
  if(class(matrix.list[[1]]) == "list"){
    for(i.elem in 1:length(matrix.list)){
      result.list[[i.elem]] <- normalize_matrix(matrix.list[[i.elem]], my.max)
    }
    names(result.list) <- names(matrix.list)
  }else{
    result.list <- lapply(matrix.list, FUN = function(x){x/my.max})
  }  
  
  result.list
  
}


