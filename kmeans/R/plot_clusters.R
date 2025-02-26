#' Plot K-Means Clusters
#'
#' This function plots the clusters obtained from the K-means clustering algorithm.
#'
#' @param data A numeric matrix or data frame.
#' @param clusters A vector of cluster assignments.
#'
#' @return A ggplot object showing the clusters.
#' @examples
#' data(iris)
#' clusters <- compute_clusters(iris[, -5], k = 3)
#' plot_clusters(iris[, -5], clusters$clusters)
#' @export

plot_clusters <- function(data, clusters) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Please install the 'ggplot2' package to use this function.")
  }

  if (ncol(data) < 2) stop("Data must have at least two dimensions for plotting")

  library(ggplot2)
  data <- as.data.frame(data)
  data$Cluster <- as.factor(clusters)

  ggplot(data, aes(x = data[, 1], y = data[, 2], color = Cluster)) +
    geom_point(size = 3) +
    labs(title = "K-Means Clustering", x = colnames(data)[1], y = colnames(data)[2]) +
    theme_minimal()
}
