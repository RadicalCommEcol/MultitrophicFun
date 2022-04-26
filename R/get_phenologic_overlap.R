#' Calculate phenologic overlap
#'
#' returns a numeric matrix, with each element being the overlap
#' between sp i and sp j relative to the complete phenological period of sp i
#'
#' @param sp.data dataframe with the following columns: ID, year,
#' min.month, min.day, max.month, max.day.
#' This dataframe can be built from the function 'filter_sp_taxonomy_phenology'
#'
#' @return numeric matrix
#' @export
get_phenologic_overlap <- function(sp.data){
  pheno.matrix <- matrix(0,nrow = nrow(sp.data),ncol = nrow(sp.data),
                         dimnames = list(sp.data$ID,sp.data$ID))

  day.start <- as.numeric(strftime(paste(sp.data$year,"-",
                                           sp.data$min.month,"-",
                                           sp.data$min.day,sep = ""),
                                     format = "%j"))
  day.end <- as.numeric(strftime(paste(sp.data$year,"-",
                                       sp.data$max.month,"-",
                                       sp.data$max.day,sep = ""),
                                 format = "%j"))

  for(i.row in 1:nrow(pheno.matrix)){
    for(i.col in 1:ncol(pheno.matrix)){

      days.i <- day.start[i.row]:day.end[i.row]
      days.j <- day.start[i.col]:day.end[i.col]

      rel.phen.overlap.i <- sum(days.i %in% days.j)/length(days.i)
      pheno.matrix[i.row,i.col] <- rel.phen.overlap.i
    }# for i.col
  }# for i.row
  return(pheno.matrix)
}
