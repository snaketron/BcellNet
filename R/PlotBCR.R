library(igraph)
library(visNetwork)

library(igraphdata)
data("karate")

#' @title Plots graphs containing thresholded communties
#' 
#' @description \code{plot_graph} transforms a graph into an internal representation to abstract unnecessary noise away
#' and highlight important communities detected. You can adjust the behaviour by applying different arguments to the parameters.
#' 
#' @param weighted_graph The base graph. Will be used as a base for the layout of the final graph.
#' @param edge_threshold All edges lower than this are removed from the final graph.
#' @param community_threshold Only communities with higher vertex count than this are highlighted.
#' @param vertex_size Controls size of vertices.
#' @param vertex_color Controls color of vertices.
#' @param edge_width Controls width of edges.
#' @param edge_color Controls color of edges.
#' @param label title of the image directly plotted to the image.
#' @param community_algorithm which algorithm is used to calculate the communities
#' 
#' @examples
#' plot_graph(igraph::graph(edges=c(1,2), n=3, directed=FALSE))
#' 
#' \dontrun{
#' library(igraphdata)
#' data("karate")
#'  
#' plot_graph(karate)
#' }
#' 
#' @aliases gp
#' 
#' @keywords plot graph bcr community highlight
#'
#' @import visNetwork
#' @import graphics
#' @export
#' 
#' @seealso \code{\link[igraph]{igraph}}
#' @seealso \code{\link[igraph]{communities}}
plot_graph <- function(weighted_graph, edge_threshold=4, community_threshold=1, vertex_size=10, vertex_color="grey", edge_width=1, edge_color="darkgrey", label="Patient X", community_algorithm="cluster_louvain") {
  
  # for reproducibility
  set.seed(23548723)
  
  # network_layout <- layout_with_fr(weighted_graph)
  
  # to plot only the edges with at least of threshold level
  # we need to copy the graph and delete the edges from it
  trimmed_network <- delete.edges(weighted_graph, which(E(weighted_graph)$weight < edge_threshold))
  
  # detect communities
  # need to parse the network first. We use strings since the IO only supports string to string dictionaries
  algo <- eval(parse(text = community_algorithm))
  communities <- algo(trimmed_network)
  
  # Get community membership
  memb <- membership(communities)
  
  # Find number of members in each community
  tab <- table(memb)
  
  # Set colors for each member. (Adjust these as desired)
  # need to copy it first
  # print(length(communities))
  community_colors <- sample_n_colors(34)[memb]
  
  # But for members of communities of one, set the color to white
  singles <- which(memb %in% as.numeric(names(tab)[tab<=community_threshold]))
  community_colors[singles] <- vertex_color
  
  # mark.groups are a list of c(a,b,c) thus need to filter out the ones with size bigger than 1
  # mark_groups <- communities(communities)
  # mark_groups <- mark_groups[as.numeric(names(mark_groups)[tab>community_threshold])]
  
  # igraph will colorize communities provided by the col=X statement
  # if given plot(communities, network, ...) form and not just plot(network, ...)
  # plot(communities, trimmed_network, mark.groups=mark_groups, vertex.size=vertex_size, edge.width=edge_width,
  #      vertex.label=NA, edge.color=edge_color,layout=network_layout, col=community_colors,
  #      main=label, edge.label=NA) 
  # visNetwork()
  
  V(trimmed_network)$label <- NA
  V(trimmed_network)$color <- community_colors
  V(trimmed_network)$frame.color <- "black"
  V(trimmed_network)$color.border <- "black"
  # V(trimmed_network)$color <- list(background="red", border="black")
  
  
  
  E(trimmed_network)$color <- "black"
  # print(community_colors)
  visIgraph(trimmed_network, layout = "layout_with_fr", physics = FALSE, smooth = FALSE, type = "full", idToLabel = FALSE) %>%
    visInteraction(dragNodes = FALSE)
  # network_data <- toVisNetworkData(weighted_graph)
  
  # nodes <- network_data[[1]]
  # edges <- network_data[[2]]
  
  # delete edges below the threshold
  # print(attributes(edges))
  # edges <- edges[edges$]
  
  # library(RColorBrewer)
  
  # col <- brewer.pal(25, "Set3")[as.factor(nodes$community)]
  # nodes$shape <- "dot"
  # nodes$shadow <- TRUE # Nodes will drop shadow
  # nodes$title <- nodes$id # Text on click
  # nodes$size <- ((nodes$betweenness / max(nodes$betweenness))+.2)*20 # Node size
  # nodes$borderWidth <- 2 # Node border width
  # nodes$color.background <- col
  # nodes$color.border <- "black"
  # nodes$color.highlight.background <- "orange"
  # nodes$color.highlight.border <- "darkred"
  # edges$title <- round(edges$edge.width,3)
  
  
  # visNetwork(nodes, edges)
  # visOptions(highlightNearest = TRUE, selectedBy = "community", nodesIdSelection = TRUE)
}
plot_graph(karate)

all_communtiy_algorithms <- function() {
  algos <- c(
    "Edge Betweenness" = "cluster_edge_betweenness", # dense connected, loose interconnection
    "Fast Greedy" = "cluster_fast_greedy", # dense subgraphs
    "Label Prop" = "cluster_label_prop", # majority vote neighbors
    "Leading Eigen" = "cluster_leading_eigen", # dense subgraphs
    "Louvain" = "cluster_louvain", # modularity
    "Optimal" = "cluster_optimal", # modularity
    "Spinglass" = "cluster_spinglass", # spin
    "Walktrap" = "cluster_walktrap" # random
  )

  return(algos)
}

sample_n_colors <- function(n) {
  colors <- grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
  sample(colors, n)
}
