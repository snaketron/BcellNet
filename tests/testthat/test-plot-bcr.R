context("Plotting")

## TODO: Add more tests

test_that("default plotting with karate dataset works", {
  library(igraph)
  library(igraphdata)
  data("karate")
  
  p <- plot_graph(karate)
  
  expect_true(is.element("visNetwork", class(p)))
})

test_that("default plotting with non graph fails", {
  expect_error(plot_graph(NA))
})

test_that("plotting with non community algorithm fails", {
  library(igraph)
  library(igraphdata)
  data("karate")
  
  expect_error(plot_graph(karate, community_algorithm = NA))
})

test_that("plotting with non layout algorithm fails", {
  library(igraph)
  library(igraphdata)
  data("karate")
  
  expect_error(plot_graph(karate, layout_algorithm = NA))
})

test_that("we have some algorithms to detect communities", {
  expect_true(length(all_communtiy_algorithms()) > 0)
})