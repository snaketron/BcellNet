# Some background information about the data to get you going...
source("R/DistanceMetric.R")
source("R/PlotBCR.R")
library(igraph)
#library(igraphdata)

# loading the input data
#load("data/control.RData")


n = c("sequence", "patient", "cell","VJ.segment","CDR3","V.sequence")
j = c("sdf","sdfsdf","dsfsdf","sdfsdf","sdfsdf","sdfsd")

control.data <- data.frame(n,j,j)

# positive and negative patient data are included in the data.frame control.data
# - positive (hepatitis C - HCV) individual has clonal BCR selection/expansion
# - negative individual does not have a clonal BCR selection/expansion
# about 300.000 BCR sequences for each individual

#table(control.data$patient)



# summary of the different Vh-Jh segments and the number of sequences which belong 
# to each Vh-Jh segment separated for the positive and negative individuals. Some
# Vh-Jh segments contain 10.000s of sequences, while others only few.

#table(control.data$VJ.segment, control.data$patient)




# reading first BCR sequence
# control.data$sequence[1]



# create subdata 
subData <- control.data$sequence[1:100]

subData <- data$sequence

arrayBcr1 <- subData
arrayBcr2 <- subData
matrix <- calculateDistances(arrayBcr1,arrayBcr2)


source("R/BuildIGraph.R")

data <- read.csv("data/Subset.csv", header = TRUE, sep = ";", stringsAsFactors=FALSE)
array <- data$sequence[1:50]

#funktioniert nicht richtig...
distances <- csvToSubset("data/Subset.csv")

array <- distances$negative$sequence


matrix <- calculateDistances(array,array)

graph <- buildIGraph(array, matrix, thresholdMax = 10, thresholdMin = 1)

printInformation(graph)

plotGraph(graph, label = "negative")




array <- distances$positive$sequence


matrix <- calculateDistances(array,array)

graph <- buildIGraph(array, matrix, thresholdMax = 10, thresholdMin = 1)

printInformation(graph)

plotGraph(graph, label = "positive")

