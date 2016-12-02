# Some background information about the data to get you going...
source("DistanceMetric.R")
library(igraph)

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
control.data$sequence[1]



# calculate distances from data
bcr1 <- control.data$sequence[1]
bcr2 <- control.data$sequence[10000]
bcr3 <- control.data$sequence[2]
dist <- distanceb2b(bcr1, bcr2)

dist


graph <- graph.empty(n=3, directed = FALSE)

V(graph)
E(graph, P=NULL, path=NULL, directed=FALSE)


# some interesting packages:
# - network analysis => igraph
# - string distance computation stringdist
# - ....