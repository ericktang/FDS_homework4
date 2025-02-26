library(testthat)
library(KMeansR)

test_that("compute_clusters works correctly", {
  data <- matrix(rnorm(100), ncol = 2)
  result <- compute_clusters(data, k = 3)

  expect_true(is.list(result))
  expect_true("clusters" %in% names(result))
  expect_true("centers" %in% names(result))
  expect_equal(length(result$clusters), nrow(data))
})

test_that("plot_clusters works correctly", {
  data <- matrix(rnorm(100), ncol = 2)
  clusters <- compute_clusters(data, k = 3)$clusters
  expect_silent(plot_clusters(data, clusters))
})
