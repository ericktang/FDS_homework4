#' Compute K-Means Clustering
#'
#' This function computes K-means clustering for a given dataset.
#'
#' @param data A numeric matrix or data frame.
#' @param k The number of clusters.
#' @param nstart Number of random sets for K-means clustering. Default is 25.
#'
#' @return A list containing cluster assignments and cluster centers.
#' @examples
#' data(iris)
#' clusters <- compute_clusters(iris[, -5], k = 3)
#' @export

compute_clusters <- function(data, k, nstart = 25) {
  if (!is.numeric(k) || k <= 0) stop("k must be a positive integer")
  if (!is.numeric(data) && !is.data.frame(data)) stop("data must be a numeric matrix or data frame")

  kmeans_result <- kmeans(data, centers = k, nstart = nstart)
  return(list(
    clusters = kmeans_result$cluster,
    centers = kmeans_result$centers
  ))
}
