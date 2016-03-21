#' @title Portion Size Estimate
#' @usage
#' bite.size(file_name = "")
#' @description
#' Given an input file of food codes and relevant proportions, returns matrices of food portion size
#' descriptive statistics based on NHANES dietary survey data.
#' @details
#' This function reads an input file of food codes and relevant proportion of the food code, and
#' produces portion size statistics for various age strata, currently hard-coded.
#' 
#' See biteR-package help >package?biteR for details of strata and input file format requirements.
#' In subsequent versions it is expected that these strata and features of the input file
#' will be parameterizable by the user.
#' 
#' This function is a wrap-around for 3 underlying steps in the BITE process:
#' param.bite(file_name = "", type = "size"), raw.bite(<Bite object>), pop.bite(<Raw object>)
#' @param file_name  Name of input .csv file containing food code and relevant proportion for
#' inclusion in the category total. See help >package?biteR for input file format.
#' @return 3 matrices of population stats containing all strata, one for each:  full, female, and male
#' @export

bite.size <- function(file_name = "") {
  
  Bite <- param.bite(file_name = file_name, type = "size")
  Raw <- raw.bite(Bite)
  return(pop.bite(Raw))
  
}

