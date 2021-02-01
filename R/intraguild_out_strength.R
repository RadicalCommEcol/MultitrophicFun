#' species-level metrics
#' 
#' different functions with the same structure, metric is in function name
#'
#' @param A interaction matrix
#' @param sp.names which sp to calculate, must be in names of A
#'
#' @return dataframe
#' @export
#'
#' @examples
intraguild_out_strength <- function(A, sp.names){
  
  result <- data.frame(sp = rownames(A), 
                       value = NA_real_,
                       stringsAsFactors = FALSE)
  
  plant.pos <- which(rownames(A) %in% sp.names[[1]]$plants)
  fv.pos <- which(rownames(A) %in% sp.names[[1]]$floral.visitors)
  herb.pos <- which(rownames(A) %in% sp.names[[1]]$herbivores)
  
  for(i.sp in 1:nrow(A)){
    if(result$sp[i.sp] %in% sp.names[[1]]$plants){
      result$value[i.sp] <- sum(A[plant.pos,i.sp]) - A[i.sp,i.sp]
    }else if(result$sp[i.sp] %in% sp.names[[1]]$herbivores){
      result$value[i.sp] <- sum(A[herb.pos,i.sp]) - A[i.sp,i.sp]
    }else if(result$sp[i.sp] %in% sp.names[[1]]$floral.visitors){
      result$value[i.sp] <- sum(A[fv.pos,i.sp]) - A[i.sp,i.sp]
    }

  }# for i.sp
  
  return(result)

}
