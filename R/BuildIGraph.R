library(igraph)

loadSource <- function(sourceName) {
  pattern <- paste("^", sourceName, "$", sep = "")
  files <- list.files(pattern=pattern, recursive = FALSE)
  for (file in files) {
    source(file)
  }
}

loadSource("DistanceMetric.R")
loadSource("PlotBCR.R")


#' @title calculate distance between given bcr arrays
#' 
#' @description \code{calculateDistances} returns the distance of the given arrays.
#' 
#' @details More Information here.
#' 
#' @param arrayBcr1 First array of bcrs
#' @param arrayBcr2 Second array of bcrs
#' @param distanceMetric Possilble distance metrics ("LD" - Levenshtein Damerau (default), etc.)
#' 
#' 
#' @return Distance matrix between first and second BCR array determined by given distance. 
#' 
#' @examples #TODO
#' 
#' 
#' @seealso \code{\link[stringdist]{stringdist}}
#' 
#' @aliases BIG
#' 
#' @keywords distance metric levenshtein damerau bcr
#'
#' @export 

calculateDistances <- function(arrayBcr1, arrayBcr2, distanceMetric = "LD"){
  
  # calculate distance between all entries in subData
  matrix <- distanceArrayOfBcr(arrayBcr1, arrayBcr2)
  
  return (matrix)
}


#' @import utils
csvToSubset <- function(path, header = TRUE, sep = ";"){
  data <- read.csv(path, header = header, sep = sep,stringsAsFactors=FALSE)
  
  #split data into subset determine by patients
  partsOfData <- split(data, f = data$patient)
  
  return(partsOfData)
}



csvToDistanceMatrices <- function(path, header = TRUE, sep = ";", distanceMetric = "LD"){
  
  data <- read.csv(path, header = header, sep = sep)
  
  #split data into subset determine by patients
  partsOfData <- split(data, f = data$patient)
  
  distances <- array(length(partsOfData))
  
  i<-1
  for(parts in partsOfData){
    distances[i] <- calculateDistances(parts, parts, distanceMetric = distanceMetric)
    i <- i+1
  }
  
  
  return(distances)
}


buildIGraph <- function(arrayBcr, distanceMatrix, thresholdMax = 5, thresholdMin = 1){
  
  graph <- graph.empty()
  #fill graph
  #add bcrs as verticies 
  for(i in 1:length(arrayBcr)){
  
    graph <- graph + vertex(name = arrayBcr[i]) 

  }
  
  
  # Connect them with their distance (add edges) 
  for(i in 1:length(arrayBcr)){

    #for(bcr2 in arrayBcr2){ 
    for(j in i:length(arrayBcr)){ 
      weight <- distanceMatrix[i,j]
      if(weight >= thresholdMin && weight <= thresholdMax){
        graph <- graph + edge(arrayBcr[i],arrayBcr[j], weight = weight)
      }
    }
    
  }
  

  graph <- as.undirected(graph)
  
  return(graph)
}


printInformation <- function(iGraphData){
  
  
  #print number of vertices
  cat("vertices:", vcount(iGraphData),"\n")
  #print number of edges
  cat("edges:", toString(ecount(iGraphData)),"\n")
  
}


plotGraph <- function(iGraphData, label = "Patient X"){
  
  # plot graph
  plot_graph(weighted_graph = iGraphData, edge_threshold=1, community_threshold=1, label = label)
}



getMaxWeight <- function(matrix){
  
  maxWeight <- which(matrix == max(matrix), arr.ind = TRUE)
  
  return(maxWeight)
}

