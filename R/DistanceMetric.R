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

# calculate distance between two bcrs
distanceb2b <- function(bcr1, bcr2){
  
  # using the Damerau-Levenshtein distance for two
  # strings of unequal length
  weight <- stringdist(bcr1, bcr2, method="dl")
  
  return (weight)
} 

# bcr1 and bcr2 should be of same length
distanceArrayOfBcr <- function(arrayBcr1, arrayBcr2){
  
  #stringdistmatrix()
  
  matrix <- matrix(nrow = length(arrayBcr1), ncol = length(arrayBcr2))
  
  for(i in 1:length(arrayBcr1)){
    for(j in 1:i){
      
      dist <- distanceb2b(arrayBcr1[i], arrayBcr2[j])
      
      matrix[i,j] <- dist
      matrix[j,i] <- dist
    }
  }
  
  return(matrix)
}




#calculate distance between bcrs given a matrix of bcrs

distanceSetOfBcr <- function(){
  
  
  
}
