#' Get species preferences for a series of matrices
#'
#' Species preferences as defined in Staniczenko et al. (2013) Nat. Comm
#' This function is similar in structure to 'get_overlap_matrix'
#'
#' @param matrix.list list of matrices, potentially nested
#'
#' @return list with the same structure as matrix.list
#' @export
get_preference_matrix <- function(matrix.list){

  GetPreference <- function(A){
    # get adjacency quantitative matrix
    Aquant <- mat.or.vec(nr=dim(A)[1], nc=dim(A)[1])
    Aquant <- cbind(Aquant, A)
    Aquant2 <- t(A)
    Aquant2 <- cbind(Aquant2, mat.or.vec(nr=dim(A)[2], nc=dim(A)[2]))
    Aquant <- rbind(Aquant, Aquant2)

    # get binary matrix
    Abinary <- Aquant
    Abinary[Abinary > 0] <- 1

    # get interaction list matrix, K, and log scaled entries vector, y
    K <- mat.or.vec(nr=sum(Abinary), nc=dim(Abinary)[1])
    y <- mat.or.vec(nr=sum(Abinary), nc=1)
    counter <- 0
    for (i in 1:dim(Abinary)[1]){
      for (j in 1:dim(Abinary)[2]){
        if (Abinary[i, j] == 1){
          counter <- counter + 1
          y[counter] <- log(Aquant[i, j])
          K[counter, i] <- 1
          K[counter, j] <- 1
        }
      }
    }

    # get t(K)K
    KTK <- t(K) %*% K

    # use LDL decomposition method?
    if(FALSE){
      # LDL decomposition
      gt <- bdsmatrix::gchol(KTK)
      L <- as.matrix(gt)
      D <- diag(gt)
      D[D==0] <- 0.05
      LDLT<-L%*%diag(D)%*%t(L)

      # get t(K)y
      KTy <- t(K) %*% y

      # solve for X
      X <- solve(LDLT, KTy)
    }

    # use pseudoinverse method
    if (TRUE){
      X <- corpcor::pseudoinverse(K, 1e-8) %*% y
    }

    X <- exp(X)

    # get mass action normalisation
    norm <- X %*% t(X)

    # perfrom normalisation
    Anorm <- Aquant / norm

    # select out bipartite component
    pref <- Anorm[1:dim(A)[1], (dim(Anorm)[2] - dim(A)[2] + 1):dim(Anorm)[2]]
    pref <- round(pref, digits=3)

    return(pref)
  }

  result.list <- list()

  if(class(matrix.list[[1]]) == "list"){
    for(i.elem in 1:length(matrix.list)){
      result.list[[i.elem]] <- get_preference_matrix(matrix.list[[i.elem]])
    }
    names(result.list) <- names(matrix.list)
  }else{
    result.list <- lapply(matrix.list, FUN = GetPreference)
  }

  result.list
}
