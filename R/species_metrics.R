#' wrapper function for several species-level metrics
#'
#' calculates a series of metrics from a set of species in an interaction matrix
#'
#' @param A interaction matrix
#' @param metrics character vector, including one or more of the following:
#' "intraguild_out_strength",
#' "interguild_out_strength",
#' "intraguild_in_strength",
#' "interguild_in_strength",
#' "intraguild_intraspecific"
#' @param sp.names which species to calculate, must be named in A
#'
#' @return dataframe
#' @export
species_metrics <- function(A,
                            metrics = c("intraguild_out_strength",
                                        "interguild_out_strength",
                                        "intraguild_in_strength",
                                        "interguild_in_strength",
                                        "intraguild_intraspecific"),
                            sp.names){

  intra.out <- NULL
  inter.out <- NULL
  intra.in <- NULL
  inter.in <- NULL
  intrasp <- NULL

    if("intraguild_out_strength" %in% metrics){
      intra.out <- intraguild_out_strength(A,sp.names)
      intra.out$metric <- "intraguild_out_strength"
    }

  if("intraguild_in_strength" %in% metrics){
    intra.in <- intraguild_in_strength(A,sp.names)
    intra.in$metric <- "intraguild_in_strength"
  }

  if("interguild_out_strength" %in% metrics){
    inter.out <- interguild_out_strength(A,sp.names)
    inter.out$metric <- "interguild_out_strength"
  }

  if("interguild_in_strength" %in% metrics){
    inter.in <- interguild_in_strength(A,sp.names)
    inter.in$metric <- "interguild_in_strength"
  }

  if("intraguild_intraspecific" %in% metrics){
    intrasp <- intraguild_intraspecific(A,sp.names)
    intrasp$metric <- "intraguild_intraspecific"
  }

  result <- dplyr::bind_rows(intra.out,intra.in,inter.out,inter.in,intrasp)

}

