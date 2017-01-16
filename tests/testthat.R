# the unit test framework
library(testthat)

# use this to run all tests on a built and installed package
# library(BcellNet)
# test_check("BcellNet")
# or test_package("BcellNet")

# use this for local package
# load_all(".")
library(BcellNet)
# test_package("BcellNet")
test_check("BcellNet")

# use this to run a single test
# you can also use source(".")
# source("R/DistanceMetric.R")
# source("tests/testthat/test_distance_metric.R")

# the first argument is the directory to the tests
# test_results <- test_dir("tests/testthat", reporter="summary")

# run test_results 