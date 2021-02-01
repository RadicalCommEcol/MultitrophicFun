#' complete vector space base
#' 
#' complete the base of the vector space for a square matrix, by
#' adding independent columns to it. New columns are denoted by I_1 to I_N
#'
#' @param P1 square matrix
#' @param noise.threshold vector, lower and upper limit to the noise term
#'
#' @return square matrix
#' @export
#' 
#' @examples
independent_base <- function(P1,noise.threshold){
    q <- Matrix::qr(P1)
    result <- P1[,q$pivot[seq(q$rank)]]
    # positions <- numeric(length = ncol(P1))
    counter <- 1
    for(i in 1:dim(P1)[1]){
        aux=rep(0,dim(P1)[1])
        aux[i] <- runif(1,noise.threshold[1],noise.threshold[2])
        cbind(result,aux)
        if((Matrix::rankMatrix(result) < Matrix::rankMatrix(cbind(result,aux))) &
           (Matrix::rankMatrix(result)<dim(P1)[1])){
            result <- cbind(result,aux)
            colnames(result)[colnames(result)=="aux"] <- paste0("I_",counter)
            counter <-  counter + 1
        }
    }
    return (result)
} 