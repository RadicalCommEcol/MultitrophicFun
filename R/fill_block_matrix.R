#' Fill a block matrix
#'
#' fill the elements of an empty block matrix (build_block_matrix) given
#' a series of intra- and interguild matrices
#'
#' @param block.matrix empty block matrix
#' @param pp.matrix plant-plant neighbours
#' @param ph.matrix plant-herbivore matrix
#' @param pfv.matrix plant-floral visitor matrix
#' @param fv.overlap.matrix floral visitor intraguild matrix
#' @param h.overlap.matrix herbivore intraguild matrix
#' @param switch.herb.sign if TRUE, plants have different signs over herbivores than herbivores over plants
#'
#' @return a filled block matrix of the same dimensions as block.matrix
#' @export
fill_block_matrix <- function(block.matrix,
                              pp.matrix = NA,
                              ph.matrix = NA,
                              pfv.matrix = NA,
                              fv.overlap.matrix = NA,
                              h.overlap.matrix = NA,
                              switch.herb.sign = TRUE){

  result.matrix <- block.matrix

  all.names <- rownames(block.matrix)
  plant.names <- rownames(pp.matrix)
  plant.in.herb.names <- rownames(ph.matrix)
  plant.in.fv.names <- rownames(pfv.matrix)
  herb.names <- colnames(ph.matrix)
  fv.names <- colnames(pfv.matrix)

  for(i.row in 1:nrow(result.matrix)){
    for(i.col in 1:ncol(result.matrix)){

      # row in plants
      if(all.names[i.row] %in% plant.names){
        # col in plants
        if(all.names[i.col] %in% plant.names){

          # plant-plant
          result.matrix[i.row,i.col] <- pp.matrix[all.names[i.row],
                                                 all.names[i.col]]

        }else if(all.names[i.col] %in% herb.names){

          # plant-herb
          # further check whether this plant is in the ph matrix
          if(all.names[i.row] %in% plant.in.herb.names){
            result.matrix[i.row,i.col] <- ph.matrix[all.names[i.row],
                                                   all.names[i.col]]
          }else{
            result.matrix[i.row,i.col] <- 0
          }

        }else if(all.names[i.col] %in% fv.names){

          # plant-floral visitor
          # further check whether this plant is in the pfv matrix
          if(all.names[i.row] %in% plant.in.fv.names){
            result.matrix[i.row,i.col] <- pfv.matrix[all.names[i.row],
                                                    all.names[i.col]]
          }else{
            result.matrix[i.row,i.col] <- 0
          }

        }
      }else if(all.names[i.row] %in% herb.names){

        if(all.names[i.col] %in% plant.names){

          # herb-plant
          # further check whether this plant is in the ph matrix
          if(all.names[i.col] %in% plant.in.herb.names){
            # check herbivore sign
            if(switch.herb.sign){
              result.matrix[i.row,i.col] <- - ph.matrix[all.names[i.col],
                                                      all.names[i.row]]
            }else{
              result.matrix[i.row,i.col] <- ph.matrix[all.names[i.col],
                                                      all.names[i.row]]
            }
          }else{
            result.matrix[i.row,i.col] <- 0
          }

        }else if(all.names[i.col] %in% herb.names){

          # herb-herb
          result.matrix[i.row,i.col] <- h.overlap.matrix[all.names[i.row],
                                                 all.names[i.col]]

        }else if(all.names[i.col] %in% fv.names){

          # herb-fv
          result.matrix[i.row,i.col] <- 0

        }
      }else if(all.names[i.row] %in% fv.names){

        if(all.names[i.col] %in% plant.names){

          # fv-plant
          # further check whether this plant is in the pfv matrix
          if(all.names[i.col] %in% plant.in.fv.names){
            result.matrix[i.row,i.col] <- pfv.matrix[all.names[i.col],
                                                    all.names[i.row]]
          }else{
            result.matrix[i.row,i.col] <- 0
          }

        }else if(all.names[i.col] %in% herb.names){

          # fv-herb
          result.matrix[i.row,i.col] <- 0

        }else if(all.names[i.col] %in% fv.names){

          # fv-fv
          result.matrix[i.row,i.col] <- fv.overlap.matrix[all.names[i.row],
                                                         all.names[i.col]]
        }# if-else col
      }# if-else row

    }# for i.col
  }# for i.row

  return(result.matrix)

}
