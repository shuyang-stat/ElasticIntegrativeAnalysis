test_that("`.score.cont()` returns expected errors", {
  expect_error(.score.cont(),
               "`psi` must be a numeric vector")
  expect_error(.score.cont(psi = "a"),
               "`psi` must be a numeric vector")
  expect_error(.score.cont(psi = matrix(1, 10, 1)),
               "`psi` must be a numeric vector")
  expect_error(.score.cont(psi = list(1:10)),
               "`psi` must be a numeric vector")
  psi <- c("int" = 1.0, "X1" = 2.0, "X2" = 3.0, "X3" = 4.0)

  expect_error(.score.cont(psi = psi),
               "`X` must be a named numeric matrix")
  expect_error(.score.cont(psi = psi, X = matrix("A", 10L, 3L)),
               "`X` must be a named numeric matrix")
  expect_error(.score.cont(psi = psi, X = c(1.0, 2.0, 3.0)),
               "`X` must be a named numeric matrix")
  expect_error(.score.cont(psi = psi, X = matrix(1.0, 10L, 3L)),
               "`X` must be a named numeric matrix")
  expect_error(.score.cont(psi = psi, X = as.data.frame(matrix(1.0, 10L, 3L))),
               "`X` must be a named numeric matrix")
  expect_error(.score.cont(psi = psi,
                           X = as.data.frame(matrix(1.0, 10L, 3L,
                                                    dimnames = list(NULL, c("x1", "x2", "x3"))))),
               "`X` must be a named numeric matrix")
  X <- matrix(1, 10L, 3L, dimnames = list(NULL, c("X1", "X2", "X3")))

  expect_error(.score.cont(psi = psi, X = X),
               "`Y` must be a numeric vector")
  expect_error(.score.cont(psi = psi, X = X, Y = rep("a", 10)),
               "`Y` must be a numeric vector")
  expect_error(.score.cont(psi = psi, X = X, Y = matrix(1.0, 10L, 1L)),
               "`Y` must be a numeric vector")
  expect_error(.score.cont(psi = psi, X = X, Y = rep(1.0, 9L)),
               "`Y` must be a numeric vector")
  Y <- rep(1, 10L)

  expect_error(.score.cont(psi = psi, X = X, Y = Y),
               "`A` must be a binary vector")
  expect_error(.score.cont(psi = psi, X = X, Y = Y, A = rep("a", 10)),
               "`A` must be a binary vector")
  expect_error(.score.cont(psi = psi, X = X, Y = Y, A = matrix(1.0, 10L, 1L)),
               "`A` must be a binary vector")
  expect_error(.score.cont(psi = psi, X = X, Y = Y, A = rep(1.0, 9L)),
               "`A` must be a binary vector")
  expect_error(.score.cont(psi = psi, X = X, Y = Y, A = c(1, 2, 3, 1, 2)),
               "`A` must be a binary vector")
  expect_error(.score.cont(psi = psi, X = X, Y = Y, A = c(1, 2, 1, 2, NA_real_)),
               "`A` must be a binary vector")
  A <- rep(1L, 10L)

  expect_error(.score.cont(psi = psi, X = X, Y = Y, A = A),
               "`wgt` must be a numeric vector")
  expect_error(.score.cont(psi = psi, X = X, Y = Y, A = A, wgt = rep("a", 10)),
               "`wgt` must be a numeric vector")
  expect_error(.score.cont(psi = psi, X = X, Y = Y, A = A, wgt = matrix(1.0, 10L, 1L)),
               "`wgt` must be a numeric vector")
  expect_error(.score.cont(psi = psi, X = X, Y = Y, A = A, wgt = rep(1.0, 9L)),
               "`wgt` must be a numeric vector")
  wgt <- rep(1.0, 10L)

  expect_error(.score.cont(psi = psi, X = X, Y = Y, A = A, wgt = wgt),
               "`ps` must be a numeric vector")
  expect_error(.score.cont(psi = psi, X = X, Y = Y, A = A, wgt = wgt,
                           ps = rep("a", 10)),
               "`ps` must be a numeric vector")
  expect_error(.score.cont(psi = psi, X = X, Y = Y, A = A, wgt = wgt,
                           ps = matrix(1.0, 10L, 1L)),
               "`ps` must be a numeric vector")
  expect_error(.score.cont(psi = psi, X = X, Y = Y, A = A, wgt = wgt,
                           ps = rep(1.0, 9L)),
               "`ps` must be a numeric vector")
  ps <- rep(1.0, 10L)

  expect_error(.score.cont(psi = psi, X = X, Y = Y, A = A, wgt = wgt, ps = ps),
               "`mu0` must be a numeric vector")
  expect_error(.score.cont(psi = psi, X = X, Y = Y, A = A, wgt = wgt, ps = ps,
                           mu0 = rep("a", 10)),
               "`mu0` must be a numeric vector")
  expect_error(.score.cont(psi = psi, X = X, Y = Y, A = A, wgt = wgt, ps = ps,
                           mu0 = matrix(1.0, 10L, 1L)),
               "`mu0` must be a numeric vector")
  expect_error(.score.cont(psi = psi, X = X, Y = Y, A = A, wgt = wgt, ps = ps,
                           mu0 = rep(1.0, 9L)),
               "`mu0` must be a numeric vector")
})


test_that("`.score.cont()` returns expected results", {
  n <- 1000L

  withr::with_seed(1234L, {
    X <- matrix(stats::runif(3*n), n, 3)
    X <- cbind(X, stats::rbinom(n, 1, 0.3))
    colnames(X) <- c("X1", "X2", "X3", "X4")
    A <- c(rep(0L, n/2), rep(1L, n/2))
    wgt <- rep(1.0, n)
    Y <- stats::rnorm(n)
    ps <- stats::runif(n)
    mu0 <- stats::rnorm(n)
  })

  psi <- c("aa" = 0.1, "X2" = 1.0, "X1" = 2.0, "X3" = 3.0, "X4" = 4.0)

  H <- {Y - mu0 - A * drop({0.1 + 2.0 * X[, "X1"] + X[, "X2"] + 3.0 * X[, "X3"] + 4.0 * X[, "X4"]})} *
    {A - ps} * wgt

  expected <- c(sum(H), sum(X[, "X2"] * H), sum(X[, "X1"] * H),
                sum(X[, "X3"] * H), sum(X[, "X4"] * H)) / n

  expect_equal(.score.cont(psi = psi, X = X, Y = Y, A = A, wgt = wgt, ps = ps, mu0 = mu0),
               expected)

})

test_that("`.score.cont()` returns expected results; single covariate", {
  n <- 1000L

  withr::with_seed(1234L, {
    X <- matrix(stats::runif(1*n), n, 1)
    colnames(X) <- c("X1")
    A <- c(rep(0L, n/2), rep(1L, n/2))
    wgt <- rep(1.0, n)
    Y <- stats::rnorm(n)
    ps <- stats::runif(n)
    mu0 <- stats::rnorm(n)
  })

  psi <- c("aa" = 0.1, "X1" = 1.0)

  H <- {Y - mu0 - A * drop({0.1 + X[, "X1"]})} *
    {A - ps} * wgt

  expected <- c(sum(H), sum(X[, "X1"] * H)) / n

  expect_equal(.score.cont(psi = psi, X = X, Y = Y, A = A, wgt = wgt, ps = ps, mu0 = mu0),
               expected)

})

test_that("`.score.cont()` returns expected results; no covariate", {
  n <- 1000L

  withr::with_seed(1234L, {
    X <- matrix(NA_real_, n, 0L)
    A <- c(rep(0L, n/2), rep(1L, n/2))
    wgt <- rep(1.0, n)
    Y <- stats::rnorm(n)
    ps <- stats::runif(n)
    mu0 <- stats::rnorm(n)
  })

  psi <- c("aa" = 0.1)

  H <- {Y - mu0 - A * 0.1} * {A - ps} * wgt

  expected <- c(sum(H)) / n

  expect_equal(.score.cont(psi = psi, X = X, Y = Y, A = A, wgt = wgt, ps = ps, mu0 = mu0),
               expected)

})

test_that("`.score.binary()` returns expected errors", {
  expect_error(.score.binary(),
               "`psi` must be a numeric vector")
  expect_error(.score.binary(psi = "a"),
               "`psi` must be a numeric vector")
  expect_error(.score.binary(psi = matrix(1, 10, 1)),
               "`psi` must be a numeric vector")
  expect_error(.score.binary(psi = list(1:10)),
               "`psi` must be a numeric vector")
  psi <- c("int" = 1.0, "X1" = 2.0, "X2" = 3.0, "X3" = 4.0)

  expect_error(.score.binary(psi = psi),
               "`X` must be a named numeric matrix")
  expect_error(.score.binary(psi = psi, X = matrix("A", 10L, 3L)),
               "`X` must be a named numeric matrix")
  expect_error(.score.binary(psi = psi, X = c(1.0, 2.0, 3.0)),
               "`X` must be a named numeric matrix")
  expect_error(.score.binary(psi = psi, X = matrix(1.0, 10L, 3L)),
               "`X` must be a named numeric matrix")
  expect_error(.score.binary(psi = psi, X = as.data.frame(matrix(1.0, 10L, 3L))),
               "`X` must be a named numeric matrix")
  expect_error(.score.binary(psi = psi,
                           X = as.data.frame(matrix(1.0, 10L, 3L,
                                                    dimnames = list(NULL, c("x1", "x2", "x3"))))),
               "`X` must be a named numeric matrix")
  X <- matrix(1, 10L, 3L, dimnames = list(NULL, c("X1", "X2", "X3")))

  expect_error(.score.binary(psi = psi, X = X),
               "`Y` must be a binary vector")
  expect_error(.score.binary(psi = psi, X = X, Y = rep("a", 10)),
               "`Y` must be a binary vector")
  expect_error(.score.binary(psi = psi, X = X, Y = matrix(1.0, 10L, 1L)),
               "`Y` must be a binary vector")
  expect_error(.score.binary(psi = psi, X = X, Y = rep(1.0, 9L)),
               "`Y` must be a binary vector")
  expect_error(.score.binary(psi = psi, X = X, Y = c(1, rep(1:3, 3L))),
               "`Y` must be a binary vector")
  Y <- rep(1, 10L)

  expect_error(.score.binary(psi = psi, X = X, Y = Y),
               "`A` must be a binary vector")
  expect_error(.score.binary(psi = psi, X = X, Y = Y, A = rep("a", 10)),
               "`A` must be a binary vector")
  expect_error(.score.binary(psi = psi, X = X, Y = Y, A = matrix(1.0, 10L, 1L)),
               "`A` must be a binary vector")
  expect_error(.score.binary(psi = psi, X = X, Y = Y, A = rep(1.0, 9L)),
               "`A` must be a binary vector")
  expect_error(.score.binary(psi = psi, X = X, Y = Y, A = c(1, 2, 3, 1, 2)),
               "`A` must be a binary vector")
  expect_error(.score.binary(psi = psi, X = X, Y = Y, A = c(1, 2, 1, 2, NA_real_)),
               "`A` must be a binary vector")
  A <- rep(1L, 10L)

  expect_error(.score.binary(psi = psi, X = X, Y = Y, A = A),
               "`wgt` must be a numeric vector")
  expect_error(.score.binary(psi = psi, X = X, Y = Y, A = A, wgt = rep("a", 10)),
               "`wgt` must be a numeric vector")
  expect_error(.score.binary(psi = psi, X = X, Y = Y, A = A, wgt = matrix(1.0, 10L, 1L)),
               "`wgt` must be a numeric vector")
  expect_error(.score.binary(psi = psi, X = X, Y = Y, A = A, wgt = rep(1.0, 9L)),
               "`wgt` must be a numeric vector")
  wgt <- rep(1.0, 10L)

  expect_error(.score.binary(psi = psi, X = X, Y = Y, A = A, wgt = wgt),
               "`ps` must be a numeric vector")
  expect_error(.score.binary(psi = psi, X = X, Y = Y, A = A, wgt = wgt,
                           ps = rep("a", 10)),
               "`ps` must be a numeric vector")
  expect_error(.score.binary(psi = psi, X = X, Y = Y, A = A, wgt = wgt,
                           ps = matrix(1.0, 10L, 1L)),
               "`ps` must be a numeric vector")
  expect_error(.score.binary(psi = psi, X = X, Y = Y, A = A, wgt = wgt,
                           ps = rep(1.0, 9L)),
               "`ps` must be a numeric vector")
  ps <- rep(1.0, 10L)

  expect_error(.score.binary(psi = psi, X = X, Y = Y, A = A, wgt = wgt, ps = ps),
               "`mu0` must be a numeric vector")
  expect_error(.score.binary(psi = psi, X = X, Y = Y, A = A, wgt = wgt, ps = ps,
                           mu0 = rep("a", 10)),
               "`mu0` must be a numeric vector")
  expect_error(.score.binary(psi = psi, X = X, Y = Y, A = A, wgt = wgt, ps = ps,
                           mu0 = matrix(1.0, 10L, 1L)),
               "`mu0` must be a numeric vector")
  expect_error(.score.binary(psi = psi, X = X, Y = Y, A = A, wgt = wgt, ps = ps,
                           mu0 = rep(1.0, 9L)),
               "`mu0` must be a numeric vector")
})


test_that("`.score.binary()` returns expected results", {
  n <- 1000L

  withr::with_seed(1234L, {
    X <- matrix(stats::runif(3*n), n, 3)
    X <- cbind(X, stats::rbinom(n, 1, 0.3))
    colnames(X) <- c("X1", "X2", "X3", "X4")
    A <- c(rep(0L, n/2), rep(1L, n/2))
    wgt <- rep(1.0, n)
    Y <- stats::rbinom(n, 1, 0.4)
    ps <- stats::runif(n)
    mu0 <- stats::rnorm(n)
  })

  psi <- c("aa" = 0.1, "X2" = 1.0, "X1" = 2.0, "X3" = 3.0, "X4" = 4.0)

  eXpsi <- pmin(drop({0.1 + 2.0 * X[, "X1"] + X[, "X2"] + 3.0 * X[, "X3"] + 4.0 * X[, "X4"]}) |> exp(), 1e8)

  wgt2 <- wgt * 2.0 * eXpsi / {eXpsi + 1.0}^2 / {mu0 * (1.0 - mu0)}

  H <- {Y - mu0 - A * {eXpsi - 1.0} / {eXpsi + 1.0}} * {A - ps} * wgt2

  expected <- c(sum(H), sum(X[, "X2"] * H), sum(X[, "X1"] * H),
                sum(X[, "X3"] * H), sum(X[, "X4"] * H)) / n

  expect_equal(.score.binary(psi = psi, X = X, Y = Y, A = A, wgt = wgt, ps = ps, mu0 = mu0),
               expected)

})

test_that("`.score.binary()` returns expected results; single covariate", {
  n <- 1000L

  withr::with_seed(1234L, {
    X <- matrix(stats::runif(1*n), n, 1)
    colnames(X) <- c("X1")
    A <- c(rep(0L, n/2), rep(1L, n/2))
    wgt <- rep(1.0, n)
    Y <- stats::rbinom(n, 1, 0.8)
    ps <- stats::runif(n)
    mu0 <- stats::rnorm(n)
  })

  psi <- c("aa" = 0.1, "X1" = 1.0)

  eXpsi <- pmin(drop({0.1 + X[, "X1"]}) |> exp(), 1e8)

  wgt2 <- wgt * 2.0 * eXpsi / {eXpsi + 1.0}^2 / {mu0 * (1.0 - mu0)}

  H <- {Y - mu0 - A * {eXpsi - 1.0} / {eXpsi + 1.0}} * {A - ps} * wgt2

  expected <- c(sum(H), sum(X[, "X1"] * H)) / n

  expect_equal(.score.binary(psi = psi, X = X, Y = Y, A = A, wgt = wgt, ps = ps, mu0 = mu0),
               expected)

})

test_that("`.score.binary()` returns expected results; no covariate", {
  n <- 1000L

  withr::with_seed(1234L, {
    X <- matrix(NA_real_, n, 0L)
    A <- c(rep(0L, n/2), rep(1L, n/2))
    wgt <- rep(1.0, n)
    Y <- stats::rbinom(n, 1, 0.8)
    ps <- stats::runif(n)
    mu0 <- stats::rnorm(n)
  })

  psi <- c("aa" = 0.1)

  eXpsi <- pmin(drop({0.1}) |> exp(), 1e8)

  wgt2 <- wgt * 2.0 * eXpsi / {eXpsi + 1.0}^2 / {mu0 * (1.0 - mu0)}

  H <- {Y - mu0 - A * {eXpsi - 1.0} / {eXpsi + 1.0}} * {A - ps} * wgt2

  expected <- c(sum(H)) / n

  expect_equal(.score.binary(psi = psi, X = X, Y = Y, A = A, wgt = wgt, ps = ps, mu0 = mu0),
               expected)

})

test_that("`.evaluatedScore()` returns expected errors", {
  expect_error(.evaluatedScore(),
               "`data` must be a list containing {X, Y, A, q, ml.mu0, ml.ps, ml.sigma0}",
               fixed = TRUE)
  expect_error(.evaluatedScore(data = c("a", "b")),
               "`data` must be a list containing {X, Y, A, q, ml.mu0, ml.ps, ml.sigma0}",
               fixed = TRUE)
  expect_error(.evaluatedScore(data = list()),
               "`data` must be a list containing {X, Y, A, q, ml.mu0, ml.ps, ml.sigma0}",
               fixed = TRUE)
  expect_error(.evaluatedScore(data = list("x" = 1, "Y" = 1, "A" = 1, "q" = 1,
                                           "ml.mu0" = 1, "ml.ps" = 1, "ml.sigma0" = 1)),
               "`data` must be a list containing {X, Y, A, q, ml.mu0, ml.ps, ml.sigma0}",
               fixed = TRUE)
  expect_error(.evaluatedScore(data = list("X" = 1, "y" = 1, "A" = 1, "q" = 1,
                                           "ml.mu0" = 1, "ml.ps" = 1, "ml.sigma0" = 1)),
               "`data` must be a list containing {X, Y, A, q, ml.mu0, ml.ps, ml.sigma0}",
               fixed = TRUE)
  expect_error(.evaluatedScore(data = list("X" = 1, "Y" = 1, "a" = 1, "q" = 1,
                                           "ml.mu0" = 1, "ml.ps" = 1, "ml.sigma0" = 1)),
               "`data` must be a list containing {X, Y, A, q, ml.mu0, ml.ps, ml.sigma0}",
               fixed = TRUE)
  expect_error(.evaluatedScore(data = list("X" = 1, "Y" = 1, "A" = 1, "Q" = 1,
                                           "ml.mu0" = 1, "ml.ps" = 1, "ml.sigma0" = 1)),
               "`data` must be a list containing {X, Y, A, q, ml.mu0, ml.ps, ml.sigma0}",
               fixed = TRUE)
  expect_error(.evaluatedScore(data = list("X" = 1, "Y" = 1, "A" = 1, "q" = 1,
                                           "Ml.mu0" = 1, "ml.ps" = 1, "ml.sigma0" = 1)),
               "`data` must be a list containing {X, Y, A, q, ml.mu0, ml.ps, ml.sigma0}",
               fixed = TRUE)
  expect_error(.evaluatedScore(data = list("X" = 1, "Y" = 1, "A" = 1, "q" = 1,
                                           "ml.mu0" = 1, "mL.ps" = 1, "ml.sigma0" = 1)),
               "`data` must be a list containing {X, Y, A, q, ml.mu0, ml.ps, ml.sigma0}",
               fixed = TRUE)
  expect_error(.evaluatedScore(data = list("X" = 1, "Y" = 1, "A" = 1, "q" = 1,
                                           "ml.mu0" = 1, "ml.ps" = 1, "ml_sigma0" = 1)),
               "`data` must be a list containing {X, Y, A, q, ml.mu0, ml.ps, ml.sigma0}",
               fixed = TRUE)
  data <- list("X" = 1, "Y" = 1, "A" = 1, "q" = 1,
               "ml.mu0" = 1, "ml.ps" = 1, "ml.sigma0" = 1)

  data$ml.sigma0 <- "a"
  expect_error(.evaluatedScore(data = data),
               "`data$ml.sigma0` must be a scalar numeric",
               fixed = TRUE)
  data$ml.sigma0 <- matrix(1, 1, 1)
  expect_error(.evaluatedScore(data = data),
               "`data$ml.sigma0` must be a scalar numeric",
               fixed = TRUE)
  data$ml.sigma0 <- c(1.0, 2.0)
  expect_error(.evaluatedScore(data = data),
               "`data$ml.sigma0` must be a scalar numeric",
               fixed = TRUE)
  data$ml.sigma0 <- 1.5

  data$X <- "a"
  expect_error(.evaluatedScore(data = data),
               "`data$X must be a matrix",
               fixed = TRUE)
  data$X <- c(1.0, 2.0)
  expect_error(.evaluatedScore(data = data),
               "`data$X must be a matrix",
               fixed = TRUE)
  data$X <- matrix(1.0, 10L, 3L, dimnames = list(NULL, c("X1", "X2", "X3")))

  expect_error(.evaluatedScore(data = data),
               "`psi` must be a named numeric vector of length ncol(X) + 1",
               fixed = TRUE)
  expect_error(.evaluatedScore(data = data, psi = "a"),
               "`psi` must be a named numeric vector of length ncol(X) + 1",
               fixed = TRUE)
  expect_error(.evaluatedScore(data = data, psi = matrix(1, 1, 3)),
               "`psi` must be a named numeric vector of length ncol(X) + 1",
               fixed = TRUE)
  expect_error(.evaluatedScore(data = data, psi = c(1.0, 2.0)),
               "`psi` must be a named numeric vector of length ncol(X) + 1",
               fixed = TRUE)
  expect_error(.evaluatedScore(data = data, psi = c(1.0, 2.0, 3.0, 4.0)),
               "`psi` must be a named numeric vector of length ncol(X) + 1",
               fixed = TRUE)
  psi <- c("aa" = 1, "X1" = 1.1, "X2" = 1.2, "X3" = 1.3)

  expect_error(.evaluatedScore(data = data, psi = psi),
               "`outcome.type` must be a character")
  expect_error(.evaluatedScore(data = data, psi = psi, outcome.type = 1),
               "`outcome.type` must be a character")
  expect_error(.evaluatedScore(data = data, psi = psi, outcome.type = "Cont"),
               "`outcome.type` must be a character")
  expect_error(.evaluatedScore(data = data, psi = psi, outcome.type = "binary"),
               "`outcome.type` must be a character")
  expect_error(.evaluatedScore(data = data, psi = psi, outcome.type = c("cont", "bin")),
               "`outcome.type` must be a character")

  expect_error(.evaluatedScore(data = data, psi = psi, outcome.type = "cont"),
               "`contName` must be a character vector")
  expect_error(.evaluatedScore(data = data, psi = psi, outcome.type = "cont",
                             contName = NA),
               "`contName` must be a character vector")
  expect_error(.evaluatedScore(data = data, psi = psi, outcome.type = "cont",
                             contName = 1),
               "`contName` must be a character vector")
  expect_error(.evaluatedScore(data = data, psi = psi, outcome.type = "cont",
                             contName = matrix("A", 1, 1)),
               "`contName` must be a character vector")
  expect_error(.evaluatedScore(data = data, psi = psi, outcome.type = "cont",
                               contName = c("X1", "x2")),
               "`contName` must be a character vector")

  expect_error(.evaluatedScore(data = data, psi = psi, outcome.type = "cont",
                               contName = c("X1", "X2")),
               "`psi` must be of length(contName) + 1", fixed = TRUE)
})

test_that("`.evaluatedScore()` returns expected results", {

  n <- 1000L

  withr::with_seed(1234L, {
    X <- matrix(stats::runif(3*n), n, 3)
    X <- cbind(X, stats::rbinom(n, 1, 0.3))
    colnames(X) <- c("X1", "X2", "X3", "X4")
    A <- c(rep(0L, n/2), rep(1L, n/2))
    wgt <- rep(1.0, n)
    Y <- stats::rnorm(n)
    ps <- stats::runif(n)
    mu0 <- stats::rnorm(n)
  })

  psi <- c("aa" = 0.1, "X2" = 1.0, "X1" = 2.0, "X3" = 3.0, "X4" = 4.0)

  expected <- .score.cont(psi = psi, X = X, Y = Y, A = A, wgt = wgt, ps = ps, mu0 = mu0)
  expected <- expected * 2.0

  data <- list("X" = X, "Y" = Y, "A" = A, "q" = wgt, "ml.ps" = ps, "ml.mu0" = mu0,
               "ml.sigma0" = 0.5)

  expect_equal(.evaluatedScore(data = data, psi = psi, outcome.type = "cont",
                               contName = c("X1", "X2", "X3", "X4")),
               expected)

  withr::with_seed(1234L, {
    X <- matrix(stats::runif(3*n), n, 3)
    X <- cbind(X, stats::rbinom(n, 1, 0.3))
    colnames(X) <- c("X1", "X2", "X3", "X4")
    A <- c(rep(0L, n/2), rep(1L, n/2))
    wgt <- rep(1.0, n)
    Y <- stats::rbinom(n, 1, 0.6)
    ps <- stats::runif(n)
    mu0 <- stats::rnorm(n)
  })

  psi <- c("aa" = 0.1, "X2" = 1.0, "X1" = 2.0, "X3" = 3.0, "X4" = 4.0)

  expected <- .score.binary(psi = psi, X = X, Y = Y, A = A, wgt = wgt, ps = ps, mu0 = mu0)

  data <- list("X" = X, "Y" = Y, "A" = A, "q" = wgt, "ml.ps" = ps, "ml.mu0" = mu0,
               "ml.sigma0" = 0.5)

  expect_equal(.evaluatedScore(data = data, psi = psi, outcome.type = "bin",
                               contName = c("X1", "X2", "X3", "X4")),
               expected)


})


test_that("`.evaluatedScore()` returns expected results; single covariate", {

  n <- 1000L

  withr::with_seed(1234L, {
    X <- matrix(stats::runif(3*n), n, 3)
    X <- cbind(X, stats::rbinom(n, 1, 0.3))
    colnames(X) <- c("X1", "X2", "X3", "X4")
    A <- c(rep(0L, n/2), rep(1L, n/2))
    wgt <- rep(1.0, n)
    Y <- stats::rnorm(n)
    ps <- stats::runif(n)
    mu0 <- stats::rnorm(n)
  })

  psi <- c("aa" = 0.1, "X2" = 1.0)

  expected <- .score.cont(psi = psi, X = X[, "X2", drop = FALSE],
                          Y = Y, A = A, wgt = wgt, ps = ps, mu0 = mu0)
  expected <- expected * 2.0

  data <- list("X" = X, "Y" = Y, "A" = A, "q" = wgt, "ml.ps" = ps, "ml.mu0" = mu0,
               "ml.sigma0" = 0.5)

  expect_equal(.evaluatedScore(data = data, psi = psi, outcome.type = "cont",
                               contName = c("X2")),
               expected)

  withr::with_seed(1234L, {
    X <- matrix(stats::runif(3*n), n, 3)
    X <- cbind(X, stats::rbinom(n, 1, 0.3))
    colnames(X) <- c("X1", "X2", "X3", "X4")
    A <- c(rep(0L, n/2), rep(1L, n/2))
    wgt <- rep(1.0, n)
    Y <- stats::rbinom(n, 1, 0.6)
    ps <- stats::runif(n)
    mu0 <- stats::rnorm(n)
  })

  psi <- c("aa" = 0.1, "X2" = 1.0)

  expected <- .score.binary(psi = psi, X = X[, "X2", drop = FALSE],
                            Y = Y, A = A, wgt = wgt, ps = ps, mu0 = mu0)

  data <- list("X" = X, "Y" = Y, "A" = A, "q" = wgt, "ml.ps" = ps, "ml.mu0" = mu0,
               "ml.sigma0" = 0.5)

  expect_equal(.evaluatedScore(data = data, psi = psi, outcome.type = "bin",
                               contName = c("X2")),
               expected)


})


test_that("`.evaluatedScore()` returns expected results; no covariate", {

  n <- 1000L

  withr::with_seed(1234L, {
    X <- matrix(stats::runif(3*n), n, 3)
    X <- cbind(X, stats::rbinom(n, 1, 0.3))
    colnames(X) <- c("X1", "X2", "X3", "X4")
    A <- c(rep(0L, n/2), rep(1L, n/2))
    wgt <- rep(1.0, n)
    Y <- stats::rnorm(n)
    ps <- stats::runif(n)
    mu0 <- stats::rnorm(n)
  })

  psi <- c("aa" = 0.1)

  expected <- .score.cont(psi = psi, X = matrix(0, n, 0),
                          Y = Y, A = A, wgt = wgt, ps = ps, mu0 = mu0)
  expected <- expected * 2.0

  data <- list("X" = X, "Y" = Y, "A" = A, "q" = wgt, "ml.ps" = ps, "ml.mu0" = mu0,
               "ml.sigma0" = 0.5)

  expect_equal(.evaluatedScore(data = data, psi = psi, outcome.type = "cont",
                               contName = NULL),
               expected)

  withr::with_seed(1234L, {
    X <- matrix(stats::runif(3*n), n, 3)
    X <- cbind(X, stats::rbinom(n, 1, 0.3))
    colnames(X) <- c("X1", "X2", "X3", "X4")
    A <- c(rep(0L, n/2), rep(1L, n/2))
    wgt <- rep(1.0, n)
    Y <- stats::rbinom(n, 1, 0.6)
    ps <- stats::runif(n)
    mu0 <- stats::rnorm(n)
  })

  psi <- c("aa" = 0.1)

  expected <- .score.binary(psi = psi, X = matrix(0, n, 0L),
                            Y = Y, A = A, wgt = wgt, ps = ps, mu0 = mu0)

  data <- list("X" = X, "Y" = Y, "A" = A, "q" = wgt, "ml.ps" = ps, "ml.mu0" = mu0,
               "ml.sigma0" = 0.5)

  expect_equal(.evaluatedScore(data = data, psi = psi, outcome.type = "bin",
                               contName = NULL),
               expected)


})


test_that("`.evaluatedScore()` returns expected results; no covariate", {

  n <- 1000L

  withr::with_seed(1234L, {
    X <- matrix(NA_real_, n, 0L)
    A <- c(rep(0L, n/2), rep(1L, n/2))
    wgt <- rep(1.0, n)
    Y <- stats::rnorm(n)
    ps <- stats::runif(n)
    mu0 <- stats::rnorm(n)
  })

  psi <- c("aa" = 0.1)

  expected <- .score.cont(psi = psi, X = matrix(0, n, 0),
                          Y = Y, A = A, wgt = wgt, ps = ps, mu0 = mu0)
  expected <- expected * 2.0

  data <- list("X" = X, "Y" = Y, "A" = A, "q" = wgt, "ml.ps" = ps, "ml.mu0" = mu0,
               "ml.sigma0" = 0.5)

  expect_equal(.evaluatedScore(data = data, psi = psi, outcome.type = "cont",
                               contName = NULL),
               expected)

  withr::with_seed(1234L, {
    X <- matrix(NA_real_, n, 0L)
    A <- c(rep(0L, n/2), rep(1L, n/2))
    wgt <- rep(1.0, n)
    Y <- stats::rbinom(n, 1, 0.6)
    ps <- stats::runif(n)
    mu0 <- stats::rnorm(n)
  })

  psi <- c("aa" = 0.1)

  expected <- .score.binary(psi = psi, X = matrix(0, n, 0L),
                            Y = Y, A = A, wgt = wgt, ps = ps, mu0 = mu0)

  data <- list("X" = X, "Y" = Y, "A" = A, "q" = wgt, "ml.ps" = ps, "ml.mu0" = mu0,
               "ml.sigma0" = 0.5)

  expect_equal(.evaluatedScore(data = data, psi = psi, outcome.type = "bin",
                               contName = NULL),
               expected)


})
