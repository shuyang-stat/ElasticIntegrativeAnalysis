test_that("`print()`", {
  withr::with_seed(1234L, {
    data.rct <- list()
    data.rct$X <- matrix(stats::rnorm(300), 100, 3,
                         dimnames = list(NULL, c("X1", "X2", "X3")))
    data.rct$Y <- stats::rnorm(100)
    data.rct$A <- stats::rbinom(100, 1, 0.4)

    data.rwe <- list()
    data.rwe$X <- matrix(stats::rnorm(3000), 1000, 3,
                         dimnames = list(NULL, c("X1", "X2", "X3")))
    data.rwe$Y <- stats::rnorm(1000)
    data.rwe$A <- stats::rbinom(1000, 1, 0.35)
  })
  test_object <- withr::with_seed(2345L, elasticHTE(data.rct, data.rwe))

  expect_equal(print(test_object), test_object)
})

test_that("`summary()`", {
  withr::with_seed(1234L, {
    data.rct <- list()
    data.rct$X <- matrix(stats::rnorm(300), 100, 3,
                         dimnames = list(NULL, c("X1", "X2", "X3")))
    data.rct$Y <- stats::rnorm(100)
    data.rct$A <- stats::rbinom(100, 1, 0.4)

    data.rwe <- list()
    data.rwe$X <- matrix(stats::rnorm(3000), 1000, 3,
                         dimnames = list(NULL, c("X1", "X2", "X3")))
    data.rwe$Y <- stats::rnorm(1000)
    data.rwe$A <- stats::rbinom(1000, 1, 0.35)
  })
  test_object <- withr::with_seed(2345L, elasticHTE(data.rct, data.rwe))

  # n.estimator x p-1 matrix
  keep_cols <- c("X1", "X2", "X3")
  est.mat <- test_object$psi[, keep_cols]
  ve.mat <- test_object$ve[, keep_cols]

  # n.estimator x p-1 matrix
  inf.mat <- test_object$CIs.inf[, keep_cols]
  sup.mat <- test_object$CIs.sup[, keep_cols]

  psi <- list()
  for (i in 1L:nrow(est.mat)) {
    psi[[rownames(est.mat)[i]]] <- data.frame("est" = est.mat[i, ],
                           "ve" = ve.mat[i, ],
                           "CI_lower" = inf.mat[i, ],
                           "CI_upper" = sup.mat[i, ])
  }

  expected <- list("psi" = psi,
                   "nuispar" = test_object$nuispar[c("gamma", "c.gamma",
                                                     "Icomb", "Icomb.pval",
                                                     "eta")],
                   "Tstat" = test_object$Tstat)


  expect_equal(summary(test_object), expected)
})
