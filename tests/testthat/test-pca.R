set.seed(12345)
x <- matrix(rnorm(10*5), nrow = 10)

scal <- scale_fit(x)
scaled <- scale_transform(x, scal)

expect_contents_equal <- function(x, y) {
  expect_equal(max(abs(x-y)), 0, tolerance=1e-5)
}

test_that("Scaling works", {
  expect_contents_equal(scaled, scale(x))
  expect_contents_equal(scale_untransform(scaled, scal), x)
})

# Scaled PCA
pca <- pca_fit(x)
pcs <- pca_transform(x, pca)
pcs_actual <- prcomp(x, scale=TRUE)$x

# Unscaled PCA
pca2 <- pca_fit(x, FALSE)
pcs2 <- pca_transform(x, pca2)

test_that("PCA works", {
  expect_contents_equal(pcs, pcs_actual)
  expect_contents_equal(pca_untransform(pcs, pca), x)
  expect_contents_equal(pca_untransform(pcs2, pca2), x)
})

# Proportion of explained variance
prop_variance <- function(pca_obj) {
  x <- pca_explained_variance(pca_obj)
  x / sum(x)
}

test_that("Proportion of explained variance is correct", {
  expect_contents_equal(
    prop_variance(pca), 
    summary(prcomp(x, scale=TRUE))$importance[2,])
})