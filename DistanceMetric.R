# Distance metrics
# package: stringdist

library(stringdist)


# calculate distance between two bcrs
distanceb2b <- function(bcr1, bcr2){
  
  # using the Damerau-Levenshtein distance for two
  # strings of unequal length
  weight <- stringdist(bcr1, bcr2, method="dl")
  
  return (weight)
} 

# bcr1 and bcr2 should be of same length
distanceArrayOfBcr <- function(arrayBcr1, arrayBcr2){
  
  stringdistmatrix()
  
  matrix <- matrix(, nrow = length(arrayBcr1), ncol = length(arrayBcr2))
  
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

