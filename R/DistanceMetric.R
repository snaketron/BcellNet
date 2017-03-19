# Distance metrics

usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dependencies = TRUE, repos="http://cran.us.r-project.org")
  require(p, character.only = TRUE)
}

usePackage("stringdist")


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



# bcr1 and bcr2 have to be of same length
distanceArrayOfBcr <- function(arrayBcr1, arrayBcr2, metric = "dl", parameter = -1, nthread = -1){
  
  time <- as.numeric(Sys.time())*1000
  
  q <- 1
  p <- 0
  
  if(parameter > -1){
    if(metric == "jw"){
      p <- parameter
    }else{
      q <- parameter
    }
  }

  print(paste("Applying distance metric with q:", q, "p:", p, "metric:", metric))
  
  matrix <- matrix(nrow = length(arrayBcr1), ncol = length(arrayBcr2))
  
  if(nthread > 0){
    matrix <- stringdistmatrix(arrayBcr1, arrayBcr2, method = metric, q = q, p = p, nthread = nthread)
  }else{
    matrix <- stringdistmatrix(arrayBcr1, arrayBcr2, method = metric, q = q, p = p)
  }
  

  print(paste("Applied distance metric in:", as.numeric(Sys.time())*1000 - time, "ms"))
  
  return(matrix)
}

# normalize values between 0 - 1; set groundzero to false you will get relatively values, 
# otherwise the minimum will be 0 and the maximum 1
normalizeMatrix <- function(matrixA, matrixB, groundZero = TRUE, update_progress = NULL){
  
  normalizedMatrixA <- matrix(nrow =nrow(matrixA), ncol = ncol(matrixA), 2)
  normalizedMatrixB <- matrix(nrow =nrow(matrixB), ncol = ncol(matrixB), 2)
  
  minVal <- min(matrixA,matrixB) 
  maxVal <- max(matrixA,matrixB) 

  if(maxVal == 0) return(list(matrixA,matrixB))
  
  if(groundZero == TRUE){
    matrix_a_nrow <- nrow(matrixA)
    for(i in 1:matrix_a_nrow){
      if (is.function(update_progress)) {
        update_progress(value = i/matrix_a_nrow, detail=paste0("normalize matrix: ", i, " of ", matrix_a_nrow))
      }
      
      for(j in 1:i){
        normalizedMatrixA[i,j] <- 1 - ((matrixA[i,j] - minVal) / maxVal)
        normalizedMatrixA[j,i] <- normalizedMatrixA[i,j]
      }
    }
    
    matrix_b_nrow <- nrow(matrixB)
    for(i in 1:matrix_b_nrow){
      if (is.function(update_progress)) {
        update_progress(value = i/matrix_b_nrow, detail=paste0("normalize matrix: ", i, " of ", matrix_b_nrow))
      }
      
      for(j in 1:i){
        normalizedMatrixB[i,j] <- 1 - ((matrixB[i,j] - minVal) / maxVal)
        normalizedMatrixB[j,i] <- normalizedMatrixB[i,j]
      }
    }
  } else {
    matrix_a_nrow <- nrow(matrixA)
    for(i in 1:matrix_a_nrow){
      if (is.function(update_progress)) {
        update_progress(value = i/matrix_a_nrow, detail=paste0("normalize matrix: ", i, " of ", matrix_a_nrow))
      }
      
      for(j in 1:i){
        normalizedMatrixA[i,j] <- 1 - (matrixA[i,j] / maxVal)
        normalizedMatrixA[j,i] <- normalizedMatrixA[i,j]
      }
    }
    
    matrix_b_nrow <- nrow(matrixB)
    for(i in 1:matrix_b_nrow){
      if (is.function(update_progress)) {
        update_progress(value = i/matrix_b_nrow, detail=paste0("normalize matrix: ", i, " of ", matrix_b_nrow))
      }
      
      for(j in 1:i){
        normalizedMatrixB[i,j] <- 1 - (matrixB[i,j] / maxVal)
        normalizedMatrixB[j,i] <- normalizedMatrixB[i,j]
      }
    }
  } 

  return(list(normalizedMatrixA,normalizedMatrixB))
}


#' @title Distance metric shortform mapping
#' 
#' @description \code{all_distance_metrics} provides easy access to all distance metric algorithms.
#' 
#' @keywords all_distance_metrics
all_distance_metrics <- function() {
  all_metrics <- data.frame(algorithm=character(), parameter_enabled=logical(),parameter_name=character(), stringsAsFactors = FALSE)
  all_metrics["Damerau-Levenshtein", ] <- c("dl", FALSE,"")
  all_metrics["Levenshtein", ] <- c("lv", FALSE,"")
  all_metrics["Optimal string aligment", ] <- c("osa", FALSE,"")
  all_metrics["Longest common substring", ] <- c("kcs", FALSE,"")
  all_metrics["Q-gram", ] <- c("qgram", TRUE,"q")
  all_metrics["Cosine of q-gram", ] <- c("cosine", TRUE,"q")
  all_metrics["Jaccard of q-gram", ] <- c("jaccard", TRUE,"q")
  all_metrics["Jaro-Winker", ] <- c("jw", TRUE,"p")
  
  return (all_metrics)
}

