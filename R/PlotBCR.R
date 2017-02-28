library(igraph)
library(visNetwork)


#' @title Plots graphs containing thresholded communties
#' 
#' @description \code{plot_graph} transforms a graph into an internal representation to abstract unnecessary noise away
#' and highlight important communities detected. You can adjust the behaviour by applying different arguments to the parameters.
#' 
#' @param weighted_graph The base graph. Will be used as a base for the layout of the final graph.
#' @param community_threshold Only communities with higher vertex count than this are highlighted.
#' @param vertex_size Controls size of vertices.
#' @param vertex_color Controls color of vertices.
#' @param edge_width Controls width of edges.
#' @param edge_color Controls color of edges.
#' @param community_algorithm which algorithm is used to calculate the communities
#' @param layout_algorithm which algorithm is used to calculate the layout of the graph
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
#' @importFrom igraph delete.edges
#' @importFrom igraph E
#' @importFrom igraph E<-
#' @importFrom igraph V
#' @importFrom igraph V<-
#' @importFrom igraph membership
#' 
#' @import visNetwork
#' @import graphics
#' 
#' @export
#' 
#' @seealso \code{\link[igraph]{igraph}}
#' @seealso \code{\link[visNetwork]{visNetwork}}
#' @seealso \code{\link[igraph]{communities}}
plot_graph <- function(weighted_graph, edge_threshold=4, community_threshold=1, vertex_size=10, vertex_color="grey", edge_width=1, edge_color="darkgrey", community_algorithm=cluster_louvain, layout_algorithm="layout_nicely", dynamic=TRUE) {
  # preconditions: input validation
  if (!any(class(weighted_graph) %in% "igraph")) {
    stop("weighted_graph must be an igraph object")
  }
  
  edge_threshold <- .normalize_numeric_inpuc(edge_threshold, 0)
  
  .validate_non_neg_input_numeric(edge_threshold)
  .validate_input_numeric(community_threshold)  
  .validate_input_numeric(vertex_size)  
  .validate_input_string(vertex_color)
  .validate_input_string(edge_color)
  if (!is.function(community_algorithm)) {
    stop("community_algorithm must be a function but found '", class(community_algorithm), "'")
  }
  .validate_input_string(layout_algorithm)  
  if (!is.logical(dynamic)) {
    stop("dynamic must be a logical but found '", class(dynamic), "'")
  }
  
  
  # for reproducibility
  set.seed(23548723)
  
  # to plot only the edges with at least of threshold level
  # we need to copy the graph and delete the edges from it
  # trimmed_network <- delete.edges(weighted_graph, which(E(weighted_graph)$weight < edge_threshold))
  
  # detect communities
  communities <- community_algorithm(weighted_graph)
  
  # Get community membership
  memb <- membership(communities)
  
  # Find number of members in each community
  tab <- table(memb)
  
  # Set colors for each member. (Adjust these as desired)
  community_colors <- array(dim=length(memb))
  # But for members of communities of one, set the color to white
  singles <- which(memb %in% as.numeric(names(tab)[tab<=community_threshold]))
  community_colors[singles] <- vertex_color
  
  # convert igraph to visgraph and prepare visual data
  nData <- toVisNetworkData(weighted_graph, FALSE)
  if (!is.null(nData$edges) && length(as.matrix(nData$edges)) != 0) {
    # need to check if node might not have an edge and thus no weight
    nData$edges$hidden <- if (!is.null(nData$edges$weight)) {
      nData$edges$weight < edge_threshold
    }

    # normalize the weight
    if (is.null(nData$edges$weight)) {
      nData$edges$weight <- 0
    }
    
    nData$edges$title <- paste0("Weight: ", nData$edges$weight)
  }

  nData$nodes$title <- paste0("sequence: ", nData$nodes$id, "<br />", "multiplier: ", nData$nodes$multiplyCounter)
  # hide label
  nData$nodes$label <- NA
  # apply communities onto vis graph groups
  nData$nodes$group <- as.numeric(memb)
  # nData$nodes$color.border <- "black"
  nData$nodes$borderWidth <- 1.5
  # colorize small communities grey
  nData$nodes$color <- community_colors
  # for https://github.com/snaketron/BcellNet/issues/10 entry point
  # default size of vis network is 25 so min is set to that size and the others are sized relative to that
  # maxMult <- max(nData$nodes$multiplyCounter)
  if (is.null(nData$nodes$multiplyCounter)) {
    nData$nodes$multiplyCounter <- 1
  }
  minMult <- min(nData$nodes$multiplyCounter)
  # cat("min mult: ", minMult, ", max mult: ", maxMult, "\n")
  # print(nData$nodes$multiplyCounter)
  nData$nodes$size <- nData$nodes$multiplyCounter / minMult * 25
  
  # finally plot it
  vn <- visNetwork(nodes = nData$nodes, edges = nData$edges) %>%
    visInteraction(dragNodes = FALSE) %>%
    visOptions(highlightNearest = list(enabled = T, hover = T))
  
  if (!is.null(nData$edges) && length(as.matrix(nData$edges)) != 0) {
    vn <- visIgraphLayout(vn, layout = layout_algorithm, smooth = FALSE, physics = FALSE, type = "square", randomSeed = NULL, layoutMatrix = NULL)
  }
  
  return (vn)
}

.normalize_numeric_inpuc <- function(numeric, default) {
  if (!is.numeric(numeric)) {
    print(paste0("'", numeric, "' must be a numeric but found '", class(numeric), "'. Auto-converting to default value '", default, "'"))
    
    return (default)
  }
  
  return (numeric)
}

# Helper function to validate the inputs
.validate_input_numeric <- function(numeric) {
  if (!is.numeric(numeric)) {
    stop("'", numeric, "' must be a numeric but found '", class(numeric), "'")
  }
  if (numeric <= 0) {
    stop("'", numeric, "' must be positve")
  }
}

.validate_non_neg_input_numeric <- function(numeric) {
  if (!is.numeric(numeric)) {
    stop("'", numeric, "' must be a numeric but found '", class(numeric), "'")
  }
  if (numeric < 0) {
    stop("'", numeric, "' must be positve")
  }
}

.validate_input_string <- function(string) {
  if (!is.character(string)) {
    stop("'", string, "' must be a string but found '", class(string), "'")
  }
  if (nchar(string) <= 0) {
    stop("'", string, "' must be a non-empty string")
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
#' @importFrom igraph cluster_fast_greedy
#' @importFrom igraph cluster_label_prop
#' @importFrom igraph cluster_leading_eigen
#' @importFrom igraph cluster_louvain
#' @importFrom igraph cluster_optimal
#' @importFrom igraph cluster_walktrap
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
  algos <- c(
    "Auto" = "layout_nicely",
    # "", layout_as_bipartite, # not possible since it is not partitioned
    "Star" = "layout_as_star",
    # "Tree" = "layout_as_tree", # not usefull since this is not a tree or if we have cycles
    "Circle" = "layout_in_circle",
    "Grid" = "layout_on_grid",
    "Sphere" = "layout_on_sphere",
    "Random" = "layout_randomly",
    "Davidson-Harel" = "layout_with_dh",
    "Distributed Recursive Layout" = "layout_with_drl",
    "Fruchterman-Reingold" = "layout_with_fr",
    "Generalized Expectation-Maximization" = "layout_with_gem",
    "GraphOpt" = "layout_with_graphopt",
    "Kamada-Kawai" = "layout_with_kk",
    "Large Graph" = "layout_with_lgl",
    "Multidimensional Scaling" = "layout_with_mds"
    # "Sugiyama" = "layout_with_sugiyama" not useful since this is for directed acyclic graphs
  )
  
  return (algos)
}