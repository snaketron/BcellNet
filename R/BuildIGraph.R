library(igraph)

loadSource <- function(sourceName) {
  pattern <- paste("^", sourceName, "$", sep = "")
  files <- list.files(pattern=pattern, recursive = TRUE)
  for (file in files) {
    source(file)
  }
}

loadSource("DistanceMetric.R")
loadSource("PlotBCR.R")



#' @title calculate distances between given bcr arrays
#' 
#' @description \code{calculateDistances} returns the distance of the given arrays.
#' 
#' @details More Information here.
#' 
#' @param arrayBcr Array of bcrs, should contain only unique values
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

calculateDistances <- function(arrayBcr, distanceMetric = "LD"){
  
  if(is.null(arrayBcr)  || identical(arrayBcr,character(0)) ) return(NULL)
  
  # calculate distance between all entries in subData
  matrix <- distanceArrayOfBcr(arrayBcr, arrayBcr)
  
  return (matrix)
}





#' @importFrom igraph graph.empty
#' @importFrom igraph vertex
#' @importFrom igraph edge
#' @importFrom igraph as.undirected
buildIGraph <- function(arrayBcr, distanceMatrix, multiplyCounter, thresholdMax, thresholdMin){
  
  graph <- graph.empty()
  #fill graph
  #add bcrs as verticies 
  for(i in 1:length(arrayBcr)){
    
    graph <- graph + vertex(name = arrayBcr[i], multiplyCounter = multiplyCounter[[arrayBcr[i]]]) 
    
  }

  # Connect them with their distance (add edges) 
  for(i in 1:length(arrayBcr)){
    cat("build igraph: ", i, " of ", length(arrayBcr), "\n" )
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





plotGraph <- function(iGraphData, label = "Patient X"){
  
  # plot graph
  plot_graph(weighted_graph = iGraphData, edge_threshold=1, community_threshold=1, label = label)
}





####################### helper calsses #######################


#' returns an environments contain the bcrs and its number of occurrence
getMapOfBcrs <- function(arrayBcr){

  if(is.null(arrayBcr) || identical(arrayBcr,character(0))) return(NULL)
  
  envMultiplyCounter <- new.env()
  
  for(i in 1:length(arrayBcr)){
    
    curBcr <- arrayBcr[i]
    
    if(is.null(envMultiplyCounter[[curBcr]])){
      envMultiplyCounter[[curBcr]] <- 1
    }else{
      count <- envMultiplyCounter[[curBcr]]
      envMultiplyCounter[[curBcr]] <- (count + 1)
    }
    
  }
  
  return(envMultiplyCounter)
}




#' @import utils
csvToSubset <- function(path, header = TRUE, sep = "def"){
  
  
  if(sep == "def"){
    #detect separator (";", "," , "TAB")
    line <- readLines(path, n=1)
    if(grepl(";",line)){
      sep <- ";"
    }else if(grepl(",",line)){
      sep <- ","
    } else if(grepl("\t",line)){
      sep <- "\t"
    }else{
      sep <- ","
    }

  }


  print("read data. . . ")  
  data <- read.csv(path, header = header, sep = sep,stringsAsFactors=FALSE)
  print("data read!")
  
  print("split data. . . ")
  #split data into subset determine by patients
  partsOfData <- split(data, f = data$patient)
  print("data splitted!")
  
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






#' @importFrom igraph vcount
#' @importFrom igraph ecount
printInformation <- function(iGraphData){
  
  #print number of verticesd
  cat("vertices:", vcount(iGraphData),"\n")
  #print number of edges
  cat("edges:", toString(ecount(iGraphData)),"\n")
  
}



