# Some background information about the data to get you going...
source("R/DistanceMetric.R")
source("R/PlotBCR.R")
library(igraph)
#library(igraphdata)

# loading the input data
load("data/control.RData")


# positive and negative patient data are included in the data.frame control.data
# - positive (hepatitis C - HCV) individual has clonal BCR selection/expansion
# - negative individual does not have a clonal BCR selection/expansion
# about 300.000 BCR sequences for each individual
table(control.data$patient)



# summary of the different Vh-Jh segments and the number of sequences which belong 
# to each Vh-Jh segment separated for the positive and negative individuals. Some
# Vh-Jh segments contain 10.000s of sequences, while others only few.
table(control.data$VJ.segment, control.data$patient)




# reading first BCR sequence
# control.data$sequence[1]



# create subdata 
subData <- control.data$sequence[1:100]
arrayBcr1 <- subData
arrayBcr2 <- subData

# calculate distance between all entries in subData
matrix <- distanceArrayOfBcr(arrayBcr1, arrayBcr2)



#graph <- graph.empty(n=100, directed = FALSE)
# create an empty graph
graph <- graph.empty()


i <- 1

#fill graph
#add bcrs as verticies 
for(element in arrayBcr1){
  
  graph <- graph + vertex(name = i) #name = element
  i <- i+1
}

i <- 1


# Connect them with their distance (add edges) 
for(bcr1 in arrayBcr1){
  j <- 1
  for(bcr2 in arrayBcr2){ 
    
    weight <- matrix[i,j]
    if(weight > 5){
      graph <- graph + edge(i,j, weight = matrix[i,j])
    }
    
    j <- j+1
  }
  i <- i+1
}


#print number of vertices
vcount(graph)
#print number of edges
ecount(graph)


# simple plot (not recommended)
# plot(graph)

graph <- as.undirected(graph)

# plot graph
plot_graph(weighted_graph = graph, edge_threshold=6, community_threshold=1)



# some interesting packages:
# - network analysis => igraph
# - string distance computation stringdist
# - ....