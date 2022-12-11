#' Scales the columns of the dataset
#'
#' @param x dataset with observations in rows, variables in columns
#' @param center whether to center the columns to mean 0
#' @param scale whether to scale columns to variance 1
#' @return parameters for scaling the given dataset which can be applied to other data
scale_fit <- function(x, center=TRUE, scale=TRUE) {
  # Centers of each column
  means <- if (center) {
    colMeans(x)
  } else {
    rep(0, ncol(x))
  }
  # Standard deviations of each column
  sds <- if (scale) {
    apply(x, 2, sd)
  } else {
    rep(1, ncol(x))
  }
  list(means=means, sds=sds)
}

#' Transforms a dataset using the given scaling parameters
#'
#' @param x dataset with observations in rows, variables in columns
#' @param scal the scaling object
#' @return the normalized data
scale_transform <- function(scal, x) {
  out <- sweep(x, 2, scal$means, "-")
  sweep(out, 2, scal$sds, "/")
}

#' Returns a normalized dataset to its original scale
#'
#' @param x the data normalized by columns
#' @param scal the scaling object
#' @return the data in its original scale
#' @export
scale_untransform <- function(scal, x) {
  out <- sweep(x, 2, scal$sds, "*")
  sweep(out, 2, scal$means, "+")
}

#' Fits a principal components analysis model to the data
#'
#' @param x matrix containing the rows of observations and columns of variables
#' @param scale whether to scale each column to unit variance
#' @return list containing the decomposition and scaling data of x.
#'   Use pca_transform and pca_untransform to work with the returned object.
#' @export
pca_fit <- function(x, scale=TRUE) {
  # Normalize the data
  scal <- scale_fit(x, TRUE, scale)
  normalized <- scale_transform(scal, x)
  # Do PCA on normalized data
  svd_obj <- svd(normalized)
  list(scale=scal, svd=svd_obj)
}

#' Subsets the first n columns of x
#'
#' This may be needed when n = 1 so that R doesn't coerce
#' the result as a numeric vector
#'
#' @param x matrix or data frame
#' @param n number of columns to subset
#' @return a matrix with the first n columns of x
subset_cols <- function(x, n) {
  if (n == 1) {
    m <- as.matrix(x[,1])
    colnames(m) <- colnames(x)[1]
    m
  } else {
    x[,1:n]
  }
}

#' Returns the principal components of x
#'
#' @param x input matrix
#' @param pca PCA object returned by pca_fit
#' @param ncomp number of PCs to return or NA for all of them
#' @return x transformed to its principal components.
#'    Column order: PC1, PC2, ...
#' @export
pca_transform <- function(pca, x, ncomp=NA) {
  scal <- pca$scale
  normalized <- scale_transform(scal, x)
  if (is.na(ncomp)) {
    ncomp <- ncol(x)
  }
  normalized %*% subset_cols(pca$svd$v, ncomp)
}

#' Reconstructs matrix from its principal components
#'
#' @param pc principal components
#' @param pca PCA object returned by pca_fit
#' @return matrix in its original scale
#' @export
pca_untransform <- function(pca, pc) {
  scal <- pca$scale
  normalized <- pc %*% t(subset_cols(pca$svd$v, ncol(pc)))
  scale_untransform(scal, normalized)
}

#' Explained variance of each principal component
#' 
#' @param pca PCA object returned by pca_fit
#' @return a vector of each principal component's explained variance
#' @export
pca_explained_variance <- function(pca) {
  d <- pca$svd$d
  n <- nrow(pca$svd$u)
  d * d / (n-1)
}
