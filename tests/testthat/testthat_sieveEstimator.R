test_that("`.sieveEstimator()` returns expected errors", {

  expect_error(.sieveEstimator(),
               "`X` must be a named matrix")
  expect_error(.sieveEstimator(X = matrix("A", 10L, 3L)),
               "`X` must be a named matrix")
  expect_error(.sieveEstimator(X = c(1.0, 2.0, 3.0)),
               "`X` must be a named matrix")
  expect_error(.sieveEstimator(X = matrix(1.0, 10L, 3L)),
               "`X` must be a named matrix")
  expect_error(.sieveEstimator(X = as.data.frame(matrix(1.0, 10L, 3L))),
               "`X` must be a named matrix")

  X <- matrix(1, 10L, 3L, dimnames = list(NULL, c("X1", "X2", "X3")))
  expect_error(.sieveEstimator(X = X),
               "`Y` must be a numeric vector")
  expect_error(.sieveEstimator(X = X, Y = rep("a", 10)),
               "`Y` must be a numeric vector")
  expect_error(.sieveEstimator(X = X, Y = matrix(1.0, 10L, 1L)),
               "`Y` must be a numeric vector")
  expect_error(.sieveEstimator(X = X, Y = rep(1.0, 9L)),
               "`Y` must be a numeric vector")

  expect_error(.sieveEstimator(X = X, Y = rep(1.0, 10L)),
               "`wgt` must be a numeric vector")
  expect_error(.sieveEstimator(X = X, Y = rep(1.0, 10L), wgt = rep("a", 10)),
               "`wgt` must be a numeric vector")
  expect_error(.sieveEstimator(X = X, Y = rep(1.0, 10L), wgt = matrix(1.0, 10L, 1L)),
               "`wgt` must be a numeric vector")
  expect_error(.sieveEstimator(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 9L)),
               "`wgt` must be a numeric vector")

  expect_error(.sieveEstimator(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L)),
               "`subset` must be a logical vector")
  expect_error(.sieveEstimator(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                               subset = rep("a", 10)),
               "`subset` must be a logical vector")
  expect_error(.sieveEstimator(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                               subset = rep(1.0, 10)),
               "`subset` must be a logical vector")
  expect_error(.sieveEstimator(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                               subset = matrix(TRUE, 10L, 1L)),
               "`subset` must be a logical vector")
  expect_error(.sieveEstimator(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                               subset = rep(TRUE, 9L)),
               "`subset` must be a logical vector")

  expect_error(.sieveEstimator(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                               subset = rep(TRUE, 10L)),
               "`sieve.degree` must be a scalar numeric")
  expect_error(.sieveEstimator(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                               subset = rep(TRUE, 10L), sieve.degree = "a"),
               "`sieve.degree` must be a scalar numeric")
  expect_error(.sieveEstimator(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                               subset = rep(TRUE, 10L), sieve.degree = TRUE),
               "`sieve.degree` must be a scalar numeric")
  expect_error(.sieveEstimator(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                               subset = rep(TRUE, 10L), sieve.degree = c(1.0, 2.0)),
               "`sieve.degree` must be a scalar numeric")
  expect_error(.sieveEstimator(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                               subset = rep(TRUE, 10L),
                               sieve.degree = matrix(1, 1L, 1L)),
               "`sieve.degree` must be a scalar numeric")

  expect_error(.sieveEstimator(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                               subset = rep(TRUE, 10L),
                               sieve.degree = 2L),
               "`method` must be one of {'glm', 'sl'}", fixed = TRUE)
  expect_error(.sieveEstimator(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                               subset = rep(TRUE, 10L),
                               sieve.degree = 2L, method = 1.0),
               "`method` must be one of {'glm', 'sl'}", fixed = TRUE)
  expect_error(.sieveEstimator(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                               subset = rep(TRUE, 10L),
                               sieve.degree = 2L, method = c("glm", "sl")),
               "`method` must be one of {'glm', 'sl'}", fixed = TRUE)
  expect_error(.sieveEstimator(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                               subset = rep(TRUE, 10L),
                               sieve.degree = 2L, method = c("sl", "glm")),
               "`method` must be one of {'glm', 'sl'}", fixed = TRUE)
  expect_error(.sieveEstimator(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                               subset = rep(TRUE, 10L),
                               sieve.degree = 2L, method = "Glm"),
               "`method` must be one of {'glm', 'sl'}", fixed = TRUE)
  expect_error(.sieveEstimator(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                               subset = rep(TRUE, 10L),
                               sieve.degree = 2L, method = "SL"),
               "`method` must be one of {'glm', 'sl'}", fixed = TRUE)

  expect_error(.sieveEstimator(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                               subset = rep(TRUE, 10L),
                               sieve.degree = 2L, method = "sl"),
               "`method.controls` must be a list")
  expect_error(.sieveEstimator(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                               subset = rep(TRUE, 10L),
                               sieve.degree = 2L, method = "sl",
                               method.controls = c("family" = "binomial")),
               "`method.controls` must be a list")
  expect_error(.sieveEstimator(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                               subset = rep(TRUE, 10L),
                               sieve.degree = 2L, method = "sl",
                               method.controls = 1.0),
               "`method.controls` must be a list")
})

test_that("`sieveEstimator()` returns expected results for glm", {

  expect_error(.sieveEstimator(X = matrix(1.0, 10, 3, dimnames = list(NULL, c("x1", "x2", "x3"))),
                               Y = rep(1, 10), wgt = rep(1.0, 10L),
                               subset = rep(TRUE, 10L),
                               sieve.degree = 2L, method = "glm",
                               method.controls = c("family" = binomial())),
               "`method.controls` is not properly defined")

  expect_error(.sieveEstimator(X = matrix(1.0, 10, 3, dimnames = list(NULL, c("x1", "x2", "x3"))),
                               Y = rep(1, 10), wgt = rep(1.0, 10L),
                               subset = rep(TRUE, 10L),
                               sieve.degree = 2L, method = "glm",
                               method.controls = list("NA.action" = TRUE)),
               "`method.controls` is not properly defined")

  X <- withr::with_seed(1234L, matrix(runif(300), 100, 3))
  X <- cbind(X, withr::with_seed(3456L, rbinom(100, 1, 0.3)))
  colnames(X) <- c("X1", "X2", "X3", "X4")
  Y <- withr::with_seed(2345L, runif(100))
  wgt <- withr::with_seed(4567L, runif(100))

  X2 <- stats::poly(X, degree = 2, raw = TRUE)[, 1L:13L]
  colnames(X2) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9",
                    "x10", "x11", "x12", "x13")

  form <- Y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13
  df <- as.data.frame(X2)
  df$Y <- Y
  fit <- stats::glm(form, df, weights = wgt, family = "gaussian")
  expected <- stats::predict.glm(fit, newdata = as.data.frame(X2), type = "response")

  expect_equal(.sieveEstimator(X = X, Y = Y, wgt = wgt, subset = rep(TRUE, 100),
                               sieve.degree = 2, method = "glm",
                               method.controls = list("family" = "gaussian")),
               expected)

  expect_warning(.sieveEstimator(X = X, Y = Y, wgt = wgt, subset = rep(TRUE, 100),
                                 sieve.degree = 2, method = "glm",
                                 method.controls = list("family" = "gaussian",
                                                        "weights" = wgt)),
                 "Element(s) weights cannot be provided as input; input overwritten", fixed = TRUE)

  fit <- stats::glm(form, df[1:50, ], weights = wgt[1:50], family = "gaussian")
  expected <- stats::predict.glm(fit, newdata = as.data.frame(X2), type = "response")

  expect_equal(.sieveEstimator(X = X, Y = Y, wgt = wgt,
                               subset = c(rep(TRUE, 50), rep(FALSE, 50)),
                               sieve.degree = 2, method = "glm",
                               method.controls = list("family" = "gaussian")),
               expected)
})

test_that("`sieveEstimator()` returns expected results for SL", {

  expect_error(.sieveEstimator(X = matrix(1.0, 10, 3, dimnames = list(NULL, c("x1", "x2", "x3"))),
                               Y = rep(1, 10), wgt = rep(1.0, 10L),
                               subset = rep(TRUE, 10L),
                               sieve.degree = 2L, method = "sl",
                               method.controls = c("family" = binomial())),
               "`method.controls` is not properly defined")

  expect_error(.sieveEstimator(X = matrix(1.0, 10, 3, dimnames = list(NULL, c("x1", "x2", "x3"))),
                               Y = rep(1, 10), wgt = rep(1.0, 10L),
                               subset = rep(TRUE, 10L),
                               sieve.degree = 2L, method = "sl",
                               method.controls = list("NA.action" = TRUE)),
               "`method.controls` is not properly defined")

  n <- 1000
  X <- withr::with_seed(1234L, matrix(runif(3*n), n, 3))
  X <- cbind(X, withr::with_seed(3456L, rbinom(n, 1, 0.3)))
  colnames(X) <- c("X1", "X2", "X3", "X4")
  Y <- withr::with_seed(2345L, runif(n))
  wgt <- withr::with_seed(4567L, runif(n))

  X2 <- stats::poly(X, degree = 2, raw = TRUE)[, 1L:13L]
  colnames(X2) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9",
                    "x10", "x11", "x12", "x13")

  form <- Y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13
  fit <- suppressWarnings(SuperLearner::SuperLearner(Y = Y, X = as.data.frame(cbind(1, X2)),
                                    newX = as.data.frame(cbind(1, X2)),
                                    obsWeights = wgt,
                                    family = gaussian(),
                                    SL.library = "SL.glm"))
  expected <- drop(fit$SL.predict)

  expect_equal(.sieveEstimator(X = X, Y = Y, wgt = wgt, subset = rep(TRUE, n),
                               sieve.degree = 2, method = "sl",
                               method.controls = list("family" = gaussian(),
                                                      "SL.library" = "SL.glm")), expected)

  expect_warning(.sieveEstimator(X = X, Y = Y, wgt = wgt, subset = rep(TRUE, n),
                                 sieve.degree = 2, method = "sl",
                                 method.controls = list("family" = "gaussian",
                                                        "obsWeights" = wgt,
                                                        "SL.library" = "SL.glm")),
                 "Element(s) obsWeights cannot be provided as input; input overwritten", fixed = TRUE)

  fit <- suppressWarnings(SuperLearner::SuperLearner(Y = Y[1:{n/2}],
                                                     X = cbind(1, X2[1:{n/2}, ]),
                                                     newX = cbind(1, X2),
                                                     obsWeights = wgt[1:{n/2}],
                                                     family = gaussian(),
                                                     SL.library = "SL.glm"))
  expected <- drop(fit$SL.predict)

  expect_equal(.sieveEstimator(X = X, Y = Y, wgt = wgt,
                               subset = c(rep(TRUE, n/2), rep(FALSE, n/2)),
                               sieve.degree = 2, method = "sl",
                               method.controls = list("family" = "gaussian",
                                                      "SL.library" = "SL.glm")), expected)
})


test_that("`sieveEstimator()` returns expected results for glm; single covariate", {

  expect_error(.sieveEstimator(X = matrix(1.0, 10, 3, dimnames = list(NULL, c("x1", "x2", "x3"))),
                               Y = rep(1, 10), wgt = rep(1.0, 10L),
                               subset = rep(TRUE, 10L),
                               sieve.degree = 2L, method = "glm",
                               method.controls = c("family" = binomial())),
               "`method.controls` is not properly defined")

  expect_error(.sieveEstimator(X = matrix(1.0, 10, 3, dimnames = list(NULL, c("x1", "x2", "x3"))),
                               Y = rep(1, 10), wgt = rep(1.0, 10L),
                               subset = rep(TRUE, 10L),
                               sieve.degree = 2L, method = "glm",
                               method.controls = list("NA.action" = TRUE)),
               "`method.controls` is not properly defined")

  n <- 1000
  X <- withr::with_seed(1234L, matrix(runif(n), n, 1))
  colnames(X) <- c("X1")
  Y <- withr::with_seed(2345L, runif(n))
  wgt <- withr::with_seed(4567L, runif(n))

  X2 <- stats::poly(X, degree = 2, raw = TRUE)
  colnames(X2) <- c("x1", "x2")

  form <- Y ~ x1 + x2
  df <- as.data.frame(X2)
  df$Y <- Y
  fit <- stats::glm(form, df, weights = wgt, family = "gaussian")
  expected <- stats::predict.glm(fit, newdata = as.data.frame(X2), type = "response")

  expect_equal(.sieveEstimator(X = X, Y = Y, wgt = wgt, subset = rep(TRUE, n),
                               sieve.degree = 2, method = "glm",
                               method.controls = list("family" = "gaussian")),
               expected)

  expect_warning(.sieveEstimator(X = X, Y = Y, wgt = wgt, subset = rep(TRUE, n),
                                 sieve.degree = 2, method = "glm",
                                 method.controls = list("family" = "gaussian",
                                                        "weights" = wgt)),
                 "Element(s) weights cannot be provided as input; input overwritten", fixed = TRUE)

  fit <- stats::glm(form, df[1:{n/2}, ], weights = wgt[1:{n/2}], family = "gaussian")
  expected <- stats::predict.glm(fit, newdata = as.data.frame(X2), type = "response")

  expect_equal(.sieveEstimator(X = X, Y = Y, wgt = wgt,
                               subset = c(rep(TRUE, n/2), rep(FALSE, n/2)),
                               sieve.degree = 2, method = "glm",
                               method.controls = list("family" = "gaussian")),
               expected)
})

test_that("`sieveEstimator()` returns expected results for SL; no covariate", {
  n <- 1000
  X <- matrix(NA_real_, n, 0L)
  Y <- withr::with_seed(2345L, runif(n))
  wgt <- withr::with_seed(4567L, runif(n))
  X2 <- X

  form <- Y ~ 1
  fit <- suppressWarnings(SuperLearner::SuperLearner(Y = Y, X = as.data.frame(cbind(1, X2)),
                                                     newX = as.data.frame(cbind(1, X2)),
                                                     obsWeights = wgt,
                                                     family = gaussian(),
                                                     SL.library = "SL.glm"))
  expected <- drop(fit$SL.predict)

  expect_equal(.sieveEstimator(X = X, Y = Y, wgt = wgt, subset = rep(TRUE, n),
                               sieve.degree = 2, method = "sl",
                               method.controls = list("family" = gaussian(),
                                                      "SL.library" = "SL.glm")), expected)

  expect_warning(.sieveEstimator(X = X, Y = Y, wgt = wgt, subset = rep(TRUE, n),
                                 sieve.degree = 2, method = "sl",
                                 method.controls = list("family" = "gaussian",
                                                        "obsWeights" = wgt,
                                                        "SL.library" = "SL.glm")),
                 "Element(s) obsWeights cannot be provided as input; input overwritten", fixed = TRUE)

  fit <- suppressWarnings(SuperLearner::SuperLearner(Y = Y[1:{n/2}], X = matrix(1.0, n/2, 1),
                                                     newX = matrix(1.0, n, 1),
                                                     obsWeights = wgt[1:{n/2}],
                                                     family = gaussian(),
                                                     SL.library = "SL.glm"))
  expected <- drop(fit$SL.predict)

  expect_equal(.sieveEstimator(X = X, Y = Y, wgt = wgt,
                               subset = c(rep(TRUE, n/2), rep(FALSE, n/2)),
                               sieve.degree = 2, method = "sl",
                               method.controls = list("family" = "gaussian",
                                                      "SL.library" = "SL.glm")), expected)
})

test_that("`sieveEstimator()` returns expected results for SL; single covariate", {
  expect_error(.sieveEstimator(X = matrix(1.0, 10, 3, dimnames = list(NULL, c("x1", "x2", "x3"))),
                               Y = rep(1, 10), wgt = rep(1.0, 10L),
                               subset = rep(TRUE, 10L),
                               sieve.degree = 2L, method = "sl",
                               method.controls = c("family" = binomial())),
               "`method.controls` is not properly defined")

  expect_error(.sieveEstimator(X = matrix(1.0, 10, 3, dimnames = list(NULL, c("x1", "x2", "x3"))),
                               Y = rep(1, 10), wgt = rep(1.0, 10L),
                               subset = rep(TRUE, 10L),
                               sieve.degree = 2L, method = "sl",
                               method.controls = list("NA.action" = TRUE)),
               "`method.controls` is not properly defined")

  n <- 1000
  X <- withr::with_seed(1234L, matrix(runif(1*n), n, 1))
  colnames(X) <- c("X1")
  Y <- withr::with_seed(2345L, runif(n))
  wgt <- withr::with_seed(4567L, runif(n))

  X2 <- stats::poly(X, degree = 2, raw = TRUE)
  colnames(X2) <- c("x1", "x2")

  form <- Y ~ x1 + x2
  fit <- suppressWarnings(SuperLearner::SuperLearner(Y = Y, X = as.data.frame(cbind(1, X2)),
                                                     newX = as.data.frame(cbind(1, X2)),
                                                     obsWeights = wgt,
                                                     family = gaussian(),
                                                     SL.library = "SL.glm"))
  expected <- drop(fit$SL.predict)

  expect_equal(.sieveEstimator(X = X, Y = Y, wgt = wgt, subset = rep(TRUE, n),
                               sieve.degree = 2, method = "sl",
                               method.controls = list("family" = gaussian(),
                                                      "SL.library" = "SL.glm")), expected)

  expect_warning(.sieveEstimator(X = X, Y = Y, wgt = wgt, subset = rep(TRUE, n),
                                 sieve.degree = 2, method = "sl",
                                 method.controls = list("family" = "gaussian",
                                                        "obsWeights" = wgt,
                                                        "SL.library" = "SL.glm")),
                 "Element(s) obsWeights cannot be provided as input; input overwritten", fixed = TRUE)

  fit <- suppressWarnings(SuperLearner::SuperLearner(Y = Y[1:{n/2}],
                                                     X = cbind(1, X2[1:{n/2}, ]),
                                                     newX = cbind(1, X2),
                                                     obsWeights = wgt[1:{n/2}],
                                                     family = gaussian(),
                                                     SL.library = "SL.glm"))
  expected <- drop(fit$SL.predict)

  expect_equal(.sieveEstimator(X = X, Y = Y, wgt = wgt,
                               subset = c(rep(TRUE, n/2), rep(FALSE, n/2)),
                               sieve.degree = 2, method = "sl",
                               method.controls = list("family" = "gaussian",
                                                      "SL.library" = "SL.glm")), expected)
})
