library(igraph)

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
#' 
#' @examples
#' plot_graph(igraph::graph(edges=c(1,2), n=3, directed=FALSE))
#' 
#' @aliases gp
#' 
#' @keywords plot graph bcr community highlight
#'
#' @import igraph
#' @import graphics
#' @export
#' 
#' @seealso \code{\link[igraph]{igraph}}
#' @seealso \code{\link[igraph]{communities}}
plot_graph <- function(weighted_graph, edge_threshold=4, community_threshold=1, vertex_size=10, vertex_color="grey", edge_width=1, edge_color="darkgrey", label="Patient X") {
  
  # for reproducibility
  set.seed(23548723)
  
  network_layout <- layout_with_fr(weighted_graph)
  
  # to plot only the edges with at least of threshold level
  # we need to copy the graph and delete the edges from it
  trimmed_network <- delete.edges(weighted_graph, which(E(weighted_graph)$weight < edge_threshold))
  
  # detect communities
  communities <- cluster_louvain(trimmed_network)
  
  # Get community membership
  memb <- membership(communities)
  
  # Find number of members in each community
  tab <- table(memb)
  
  # Set colors for each member. (Adjust these as desired)
  # need to copy it first
  community_colors <- memb
  
  # But for members of communities of one, set the color to white
  singles <- which(memb %in% as.numeric(names(tab)[tab<=community_threshold]))
  community_colors[singles] <- vertex_color
  
  # mark.groups are a list of c(a,b,c) thus need to filter out the ones with size bigger than 1
  mark_groups <- communities(communities)
  mark_groups <- mark_groups[as.numeric(names(mark_groups)[tab>community_threshold])]
  
  # igraph will colorize communities provided by the col=X statement
  # if given plot(communities, network, ...) form and not just plot(network, ...)
  plot(communities, trimmed_network, mark.groups=mark_groups, vertex.size=vertex_size, edge.width=edge_width,
       vertex.label=NA, edge.color=edge_color,layout=network_layout, col=community_colors,
       main=label, edge.label=NA) 
}

# 
 library(igraphdata)
 data("karate")
# 
plot_graph(karate)