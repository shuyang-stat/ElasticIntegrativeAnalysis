test_that("`.outcome()` returns expected errors", {

  expect_error(.outcome(), "`X` must be a named numeric matrix")
  expect_error(.outcome(X = matrix("A", 10L, 3L)),
               "`X` must be a named numeric matrix")
  expect_error(.outcome(X = c(1.0, 2.0, 3.0)),
               "`X` must be a named numeric matrix")
  expect_error(.outcome(X = matrix(1.0, 10L, 3L)),
               "`X` must be a named numeric matrix")
  expect_error(.outcome(X = as.data.frame(matrix(1.0, 10L, 3L))),
               "`X` must be a named numeric matrix")

  X <- matrix(1, 10L, 3L, dimnames = list(NULL, c("X1", "X2", "X3")))

  expect_error(.outcome(X = X),
               "`Y` must be a numeric vector")
  expect_error(.outcome(X = X, Y = rep("a", 10)),
               "`Y` must be a numeric vector")
  expect_error(.outcome(X = X, Y = matrix(1.0, 10L, 1L)),
               "`Y` must be a numeric vector")
  expect_error(.outcome(X = X, Y = rep(1.0, 9L)),
               "`Y` must be a numeric vector")

  Y <- rep(1, 10L)
  expect_error(.outcome(X = X, Y = Y),
               "`A` must be a binary vector")
  expect_error(.outcome(X = X, Y = Y, A = rep("a", 10)),
               "`A` must be a binary vector")
  expect_error(.outcome(X = X, Y = Y, A = matrix(1.0, 10L, 1L)),
               "`A` must be a binary vector")
  expect_error(.outcome(X = X, Y = Y, A = rep(1.0, 9L)),
               "`A` must be a binary vector")
  expect_error(.outcome(X = X, Y = Y, A = c(1, 2, 3, 1, 2)),
               "`A` must be a binary vector")
  expect_error(.outcome(X = X, Y = Y, A = c(1, 2, 1, 2, NA_real_)),
               "`A` must be a binary vector")

  A <- rep(1L, 10L)
  expect_error(.outcome(X = X, Y = Y, A = A, outcome.type = "cont"),
               "`wgt` must be a numeric vector")
  expect_error(.outcome(X = X, Y = Y, A = A, outcome.type = "cont", wgt = rep("a", 10)),
               "`wgt` must be a numeric vector")
  expect_error(.outcome(X = X, Y = Y, A = A, outcome.type = "cont", wgt = matrix(1.0, 10L, 1L)),
               "`wgt` must be a numeric vector")
  expect_error(.outcome(X = X, Y = Y, A = A, outcome.type = "cont", wgt = rep(1.0, 9L)),
               "`wgt` must be a numeric vector")

  wgt <- rep(1.0, 10L)
  expect_error(.outcome(X = X, Y = Y, A = A, outcome.type = "cont", wgt = wgt),
               "`sieve.degree` must be a scalar numeric")
  expect_error(.outcome(X = X, Y = Y, A = A, outcome.type = "cont", wgt = wgt,
                        sieve.degree = "a"),
               "`sieve.degree` must be a scalar numeric")
  expect_error(.outcome(X = X, Y = Y, A = A, outcome.type = "cont", wgt = wgt,
                        sieve.degree = TRUE),
               "`sieve.degree` must be a scalar numeric")
  expect_error(.outcome(X = X, Y = Y, A = A, outcome.type = "cont", wgt = wgt,
                        sieve.degree = c(1.0, 2.0)),
               "`sieve.degree` must be a scalar numeric")
  expect_error(.outcome(X = X, Y = Y, A = A, outcome.type = "cont", wgt = wgt,
                        sieve.degree = matrix(1, 1L, 1L)),
               "`sieve.degree` must be a scalar numeric")

  expect_error(.outcome(X = X, Y = Y, A = A, outcome.type = "cont", wgt = wgt,
                        sieve.degree = 2L),
               "`method` must be provided")

  expect_error(.outcome(X = X, Y = Y, A = A, outcome.type = "cont", wgt = wgt,
                        sieve.degree = 2L, method = "glm"),
               "`method.controls` must be a list")
  expect_error(.outcome(X = X, Y = Y, A = A, outcome.type = "cont", wgt = wgt,
                        sieve.degree = 2L, method = "glm",
                        method.controls = c("family" = "binomial")),
               "`method.controls` must be a list")
  expect_error(.outcome(X = X, Y = Y, A = A, outcome.type = "cont", wgt = wgt,
                        sieve.degree = 2L, method = "glm",
                        method.controls = 1.0),
               "`method.controls` must be a list")

})

test_that("`.outcome()` returns the expected results", {

  n <- 1000L

  X <- withr::with_seed(1234L, matrix(stats::runif(3*n), n, 3))
  X <- cbind(X, withr::with_seed(3456L, stats::rbinom(n, 1, 0.3)))
  colnames(X) <- c("X1", "X2", "X3", "X4")
  A <- c(rep(0L, n/2), rep(1L, n/2))
  wgt <- rep(1.0, n)
  beta <- withr::with_seed(2345L, stats::runif(ncol(X) + 1L, -1.0, 1.0))
  Y <- drop(X %*% beta[-1L]) + beta[1L]

  method.controls <- list("family" = gaussian())

  res <- list()

  res$me0 <- .sieveEstimator(X = X, Y = Y, wgt = wgt,
                             sieve.degree = 1L,
                             subset = c(rep(TRUE, n/2), rep(FALSE, n/2)),
                             method = "glm",
                             method.controls = method.controls)
  res$me0.conditional.var <- stats::var({Y[1L:{n/2}] - res$me0[1L:{n/2}]})

  res$ml.me0 <- .sieveEstimator(X = X, Y = Y, wgt = wgt,
                                sieve.degree = 2L,
                                subset = c(rep(TRUE, n/2), rep(FALSE, n/2)),
                                method = "glm",
                                method.controls = method.controls)
  res$ml.me0.conditional.var <- stats::var({{Y[1L:{n/2}] - res$ml.me0[1L:{n/2}]}})

  res$ml.me1 <- .sieveEstimator(X = X, Y = Y, wgt = wgt,
                                sieve.degree = 2L,
                                subset = c(rep(FALSE, n/2), rep(TRUE, n/2)),
                                method = "glm",
                                method.controls = method.controls)

  res$inv.sig2 <- rep(1.0 / res$me0.conditional.var, nrow(X))
  res$ml.inv.sig2 <- rep(1.0 / res$ml.me0.conditional.var, nrow(X))

  expect_equal(.outcome(X = X, Y = Y, A = A, wgt = wgt, sieve.degree = 2L,
                        method = "glm",
                        method.controls = method.controls,
                        outcome.type = "cont"),
               res)

})

test_that("`.outcome()` returns the expected results; single covariate", {

  n <- 1000L

  X <- withr::with_seed(1234L, matrix(runif(1*n), n, 1))
  colnames(X) <- c("X1")
  A <- c(rep(0L, n/2), rep(1L, n/2))
  wgt <- rep(1.0, n)
  beta <- withr::with_seed(2345L, stats::runif(ncol(X) + 1L, -1.0, 1.0))
  Y <- drop(X %*% beta[-1L]) + beta[1L]

  method.controls <- list("family" = gaussian())

  res <- list()

  res$me0 <- .sieveEstimator(X = X, Y = Y, wgt = wgt,
                             sieve.degree = 1L,
                             subset = c(rep(TRUE, n/2), rep(FALSE, n/2)),
                             method = "glm",
                             method.controls = method.controls)
  res$me0.conditional.var <- var({{Y[1L:{n/2}] - res$me0[1L:{n/2}]}})

  res$ml.me0 <- .sieveEstimator(X = X, Y = Y, wgt = wgt,
                                sieve.degree = 2L,
                                subset = c(rep(TRUE, n/2), rep(FALSE, n/2)),
                                method = "glm",
                                method.controls = method.controls)
  res$ml.me0.conditional.var <- var({{Y[1L:{n/2}] - res$ml.me0[1L:{n/2}]}})

  res$ml.me1 <- .sieveEstimator(X = X, Y = Y, wgt = wgt,
                                sieve.degree = 2L,
                                subset = c(rep(FALSE, n/2), rep(TRUE, n/2)),
                                method = "glm",
                                method.controls = method.controls)

  res$inv.sig2 <- rep(1.0 / res$me0.conditional.var, nrow(X))
  res$ml.inv.sig2 <- rep(1.0 / res$ml.me0.conditional.var, nrow(X))

  expect_equal(.outcome(X = X, Y = Y, A = A, wgt = wgt, sieve.degree = 2L,
                        method = "glm",
                        method.controls = method.controls,
                        outcome.type = "cont"),
               res)

})

test_that("`.outcome()` returns the expected results; no covariate", {

  n <- 1000L

  X <- matrix(NA_real_, n, 0L)
  A <- c(rep(0L, n/2), rep(1L, n/2))
  wgt <- rep(1.0, n)
  beta <- withr::with_seed(2345L, stats::runif(ncol(X) + 1L, -1.0, 1.0))
  Y <- drop(X %*% beta[-1L]) + beta[1L]

  method.controls <- list("family" = gaussian())

  res <- list()

  res$me0 <- .sieveEstimator(X = X, Y = Y, wgt = wgt,
                             sieve.degree = 1L,
                             subset = c(rep(TRUE, n/2), rep(FALSE, n/2)),
                             method = "glm",
                             method.controls = method.controls)
  res$me0.conditional.var <- var({{Y[1L:{n/2}] - res$me0[1L:{n/2}]}^2})

  res$ml.me0 <- .sieveEstimator(X = X, Y = Y, wgt = wgt,
                                sieve.degree = 2L,
                                subset = c(rep(TRUE, n/2), rep(FALSE, n/2)),
                                method = "glm",
                                method.controls = method.controls)
  res$ml.me0.conditional.var <- var({{Y[1L:{n/2}] - res$ml.me0[1L:{n/2}]}^2})

  res$ml.me1 <- .sieveEstimator(X = X, Y = Y, wgt = wgt,
                                sieve.degree = 2L,
                                subset = c(rep(FALSE, n/2), rep(TRUE, n/2)),
                                method = "glm",
                                method.controls = method.controls)

  res$inv.sig2 <- rep(1.0 / res$me0.conditional.var, nrow(X))
  res$ml.inv.sig2 <- rep(1.0 / res$ml.me0.conditional.var, nrow(X))

  expect_equal(.outcome(X = X, Y = Y, A = A, wgt = wgt, sieve.degree = 2L,
                        method = "glm",
                        method.controls = method.controls,
                        outcome.type = "cont"),
               res)

})
