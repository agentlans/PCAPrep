
x <- as.matrix(iris[,1:4])
scal <- scale_fit(x)
scaled <- scale_transform(x, scal)

expect_contents_equal <- function(x, y) {
  expect_equal(max(abs(x-y)), 0)
}

test_that("Scaling works", {
  expect_contents_equal(scaled, scale(x))
  expect_contents_equal(scale_untransform(scaled, scal), x)
})

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
