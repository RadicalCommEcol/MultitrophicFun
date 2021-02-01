#' add random or targeted noise to a non-invertible matrix
#'
#' @param A numeric matrix
#' @param noise.threshold vector of two values, minimum and maximum value for the perturbation
#' @param noise.type random (to any column) or targeted (to columns that are linear combinations of others)
#'
#' @return perturbed matrix
#' @export
add_noise_matrix <- function(A = NULL,
                             noise.threshold = c(0,1),
                             noise.type = c("random","targeted")){
  if(noise.type == "random"){
    noise.matrix <- matrix(stats::runif(ncol(A)*nrow(A),
                                 noise.threshold[1],
                                 noise.threshold[2]),nrow = nrow(A))
  }else{
    noise.matrix <- independent_base(A,noise.threshold = noise.threshold)

    lindep.cols <- colnames(A)[which(!colnames(A) %in% colnames(noise.matrix))]
    colnames(noise.matrix)[grep("I_",colnames(noise.matrix))] <- lindep.cols
    noise.matrix[,which(!colnames(noise.matrix) %in% lindep.cols)] <- 0
    noise.matrix <- noise.matrix[,colnames(A)]
  }

  return(A + noise.matrix)
}
