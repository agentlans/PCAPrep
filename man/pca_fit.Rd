% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PCA.R
\name{pca_fit}
\alias{pca_fit}
\title{Fits a principal components analysis model to the data}
\usage{
pca_fit(x, scale = TRUE)
}
\arguments{
\item{x}{matrix containing the rows of observations and columns of variables}

\item{scale}{whether to scale each column to unit variance}
}
\value{
list containing the decomposition and scaling data of x.
  Use pca_transform and pca_untransform to work with the returned object.
}
\description{
Fits a principal components analysis model to the data
}
