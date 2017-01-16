# Distance metrics

library(stringdist)


#' @title calculate distance between two BCRs.
#' 
#' @description \code{distanceb2b} returns the Damerau-Levenshtein Distance of its arguments.
#' 
#' @details More Information here.
#' 
#' @param bcr1 First BCR
#' @param bcr2 Second BCR
#' 
#' @return Distance between first and second BCR determined by Damerau-Levenshtein Distance
#' 
#' @examples
#' distanceb2b("", "")
#' distanceb2b("ATG", "ATC")
#' 
#' @seealso \code{\link[stringdist]{stringdist}}
#' 
#' @aliases DM
#' 
#' @keywords distance metric levenshtein damerau bcr
#'
#' @import stringdist
#' @export 
distanceb2b <- function(bcr1, bcr2){
  
  # using the Damerau-Levenshtein distance for two
  # strings of unequal length
  weight <- stringdist(bcr1, bcr2, method="dl")
  
  return (weight)
} 



#calculate distance between bcrs given a matrix of bcrs

distanceSetOfBcr <- function(){
  
  
  
}

