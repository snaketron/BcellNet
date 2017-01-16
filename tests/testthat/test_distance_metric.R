# This file can contain as many tests you want
# They have to start with test* though to be recognized by testthat package

# There can be 11 different expectations:
# - expect_true(x)	checks that an expression is true.
# - expect_false(x)	checks that an expression is false.
# - expect_is(x, y)	checks that an object inherit()s from a specified class
# - expect_equal(x, y)	check for equality with numerical tolerance
# - expect_equivalent(x, y)	a more relaxed version of equals() that ignores attributes
# - expect_identical(x, y)	check for exact equality
# - expect_matches(x, y)	matches a character vector against a regular expression.
# - expect_output(x, y)	matches the printed output from an expression against a regular expression
# - expect_message(x, y)	checks that an expression shows a message
# - expect_warning(x, y)	expects that you get a warning
# - expect_error(x, y)	verifies that the expression throws an error.

# Tests are generally contained besides the ones with sideeffects like I/O, global environment etc

# Contexts group tests together into blocks that test related functionality
# and are established with the code: context("My context").
# Normally there is one context per file, but you can have more if you want,
# or you can use the same context in multiple files.

context("Distance Metric")

test_that("Test Distance of empty BCRs", {
  first <- ""
  second <- ""
  
  expect_equal(distanceb2b(first, second), 0)
})
