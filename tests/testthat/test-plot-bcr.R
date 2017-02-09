context("Plotting")

## TODO: Add more tests

test_that("default plotting with karate dataset works", {
  library(igraph)
  library(igraphdata)
  data("karate")
  
  p <- plot_graph(karate)
  
  expect_true(is.element("visNetwork", class(p)))
})

test_that("we have some algorithms to detect communities", {
  expect_true(length(all_communtiy_algorithms()) > 0)
})