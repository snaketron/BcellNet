# the unit test framework
library(testthat)

# use this to run all tests on a built and installed package
# library(BcellNet)
# test_check("BcellNet")
# or test_package("BcellNet")

# use this for local package
load_all(".")
test_package("BcellNet")


# use this to run a single test
# you can also use source(".")
# source("DistanceMetric.R")
# source("tests/testthat/test_distance_metric.R")

# the first argument is the directory to the tests
# test_results <- test_dir("tests/testthat", reporter="summary")

# run test_results 