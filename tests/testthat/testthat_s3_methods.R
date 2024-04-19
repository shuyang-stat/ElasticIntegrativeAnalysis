withr::with_seed(1234L, {
  n <- 1000L
  m <- n * 10L
  p <- 3L
  data.rct <- list()
  data.rct$X <- matrix(stats::rnorm(n * p), n, p,
                       dimnames = list(NULL, paste0("X", 1L:p)))
  beta <- stats::runif(p + 1L, -1.0,1.0)
  data.rct$Y <- drop(data.rct$X %*% beta[-1L]) + beta[1L] + stats::rnorm(n)
  data.rct$A <- stats::rbinom(n, 1, 0.4)
  data.rct$mainName <- colnames(data.rct$X)
  data.rct$psName <- colnames(data.rct$X)
  data.rct$contName <- colnames(data.rct$X)

  data.rwe <- list()
  data.rwe$X <- matrix(stats::rnorm(m * p), m, p,
                       dimnames = list(NULL, paste0("X", 1L:p)))
  data.rwe$Y <- drop(data.rct$X %*% beta[-1L]) + beta[1L] + stats::rnorm(m)
  data.rwe$A <- stats::rbinom(m, 1, 0.35)
  data.rwe$mainName <- colnames(data.rwe$X)
  data.rwe$psName <- colnames(data.rwe$X)
  data.rwe$contName <- colnames(data.rwe$X)
})

test_object <- withr::with_seed(2345L,
                                elasticHTE(data.rct, data.rwe,
                                           n.boot = 10L, n.gamma = 10L,
                                           n.pert = 10L))

test_that("`print()`", {
  expect_invisible(out <- capture_output(print(test_object)))
})

test_that("`summary()`", {
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
