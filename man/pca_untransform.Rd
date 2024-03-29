% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PCA.R
\name{pca_untransform}
\alias{pca_untransform}
\title{Reconstructs matrix from its principal components}
\usage{
pca_untransform(pca, pc)
}
\arguments{
\item{pca}{PCA object returned by pca_fit}

\item{pc}{principal components}
}
\value{
matrix in its original scale
}
\description{
Reconstructs matrix from its principal components
}
