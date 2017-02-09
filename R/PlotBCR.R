library(igraph)
library(visNetwork)


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
#' @param dynamic Logical Type. Determines if the plot is rendered dynamically via JS or static as a SVG
#' 
#' @examples
#' require(igraph)
#' plot_graph(igraph::graph(edges=c(1,2), n=3, directed=FALSE))
#' 
#' \dontrun{
#' library(igraphdata)
#' data("karate")
#'  
#' plot_graph(karate)
#' }
#' 
#' @aliases plot_graph
#' 
#' @keywords plot graph bcr community highlight
#'
#' @import visNetwork
#' @import graphics
#' @export
#' 
#' @seealso \code{\link[igraph]{igraph}}
#' @seealso \code{\link[visNetwork]{visNetwork}}
#' @seealso \code{\link[igraph]{communities}}
plot_graph <- function(weighted_graph, edge_threshold=4, community_threshold=1, vertex_size=10, vertex_color="grey", edge_width=1, edge_color="darkgrey", label="Patient X", community_algorithm=cluster_louvain, layout_algorithm="layout_nicely", dynamic=TRUE) {
  # preconditions: input validation
  if (!any(class(weighted_graph) %in% "igraph")) {
    stop("weighted_graph must be an igraph object")
  }
  validateInputNumeric(edge_threshold)
  validateInputNumeric(community_threshold)  
  validateInputNumeric(vertex_size)  
  validateInputString(vertex_color)
  validateInputString(edge_color)
  validateInputString(label)
  if (!is.function(community_algorithm)) {
    stop("community_algorithm must be a function but found '", class(community_algorithm), "'")
  }
  validateInputString(layout_algorithm)  
  if (!is.logical(dynamic)) {
    stop("dynamic must be a logical but found '", class(dynamic), "'")
  }
  
  
  # for reproducibility
  set.seed(23548723)
  
  # network_layout <- layout_with_fr(weighted_graph)
  
  # to plot only the edges with at least of threshold level
  # we need to copy the graph and delete the edges from it
  trimmed_network <- delete.edges(weighted_graph, which(E(weighted_graph)$weight < edge_threshold))
  
  # detect communities
  # need to parse the network first. We use strings since the IO only supports string to string dictionaries
  # algo <- eval(parse(text = community_algorithm))
  # print(parse(text = community_algorithm))
  communities <- community_algorithm(trimmed_network)
  
  # Get community membership
  memb <- membership(communities)
  
  # Find number of members in each community
  tab <- table(memb)
  
  # Set colors for each member. (Adjust these as desired)
  # need to copy it first
  # print(length(communities))
  community_colors <- sample_n_colors(100)[memb]
  
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
  # V(trimmed_network)$color.border.wtf <- "wtf"
  
  
  E(trimmed_network)$color <- "black"
  trimmed_network$main <- label
  # print(community_colors)
  # visNetwork(V(trimmed_network), E(trimmed_network), main=label) %>%
    # visIgraphLayout(layout="layout_with_fr", physics = FALSE, smooth = FALSE, type = "full") %>%
    # visInteraction(dragNodes = FALSE)
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

validateInputNumeric <- function(numeric) {
  if (!is.numeric(numeric)) {
    stop("'", quote(numeric), "' must be a numeric but found '", class(numeric), "'")
  }
  if (numeric <= 0) {
    stop("'", quote(numeric), "' must be positve")
  }
}

validateInputString <- function(string) {
  if (!is.character(string)) {
    stop("'", quote(string), "' must be a string but found '", class(string), "'")
  }
  if (nchar(string) <= 0) {
    stop("'", quote(string), "' must be a non-empty string")
  }
}



#' @title Provides all community algorithms available
#' 
#' @description \code{all_communtiy_algorithms} provides an easy dictionary of all algorithms to determine the communities. This enables access to a human readable representation of the internal functions. "Foo Bar" could be called foo_bar internally, which is not something supposed to being displayed. It will returns a dictionary mapping from \code{string} to \code{string}. The \code{plot_graph} will handle the decryption of the functions internally.
#' 
#' @examples
#' all_communtiy_algorithms()
#' 
#' @aliases all_communtiy_algorithms
#' 
#' @keywords igraph community algorithm provider
#'
#' @export
#' 
#' @seealso \code{\link[igraph]{igraph}}
#' @seealso \code{\link[igraph]{communities}}
all_communtiy_algorithms <- function() {
  algos <- c(
    # "Edge Betweenness" = cluster_edge_betweenness, # dense connected, loose interconnection does not work - crashes RStudio
    "Fast Greedy" = cluster_fast_greedy, # dense subgraphs
    "Label Prop" = cluster_label_prop, # majority vote neighbors
    "Leading Eigen" = cluster_leading_eigen, # dense subgraphs
    "Louvain" = cluster_louvain, # modularity
    "Optimal" = cluster_optimal, # modularity
    # "Spinglass" = cluster_spinglass, # spin # needs connected graphs
    "Walktrap" = cluster_walktrap # random
  )

  return(algos)
}

all_layout_algorithms <- function() {
  
}

sample_n_colors <- function(n) {
  colors <- grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
  sample(colors, n)
}
