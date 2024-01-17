test_that("`.rootsOfScore()` returns expected errors", {

  expect_error(.rootsOfScore(),
               "`X` must be a named matrix")
  expect_error(.rootsOfScore(X = "a"),
               "`X` must be a named matrix")
  expect_error(.rootsOfScore(X = c(1.0, 2.0, 3.0)),
               "`X` must be a named matrix")
  expect_error(.rootsOfScore(X = matrix("a", 10, 3)),
               "`X` must be a named matrix")
  expect_error(.rootsOfScore(X = matrix(1.0, 10, 3)),
               "`X` must be a named matrix")
  X <- matrix(1, 10, 3L, dimnames = list(NULL, c("X1", "X2", "X3")))

  expect_error(.rootsOfScore(X = X), "`Y` must be provided")
  expect_error(.rootsOfScore(X = X, Y = 1), "`A` must be provided")
  expect_error(.rootsOfScore(X = X, Y = 1, A = 1), "`wgt` must be provided")
  expect_error(.rootsOfScore(X = X, Y = 1, A = 1, wgt = 1),
               "`mu0` must be provided")
  expect_error(.rootsOfScore(X = X, Y = 1, A = 1, wgt = 1, mu0 = 1),
               "`ps` must be provided")

  expect_error(.rootsOfScore(X = X, Y = 1, A = 1, wgt = 1, mu0 = 1, ps = 1),
               "`initial.guess` must be a named numeric vector")
  expect_error(.rootsOfScore(X = X, Y = 1, A = 1, wgt = 1, mu0 = 1, ps = 1,
                             initial.guess = "a"),
               "`initial.guess` must be a named numeric vector")
  expect_error(.rootsOfScore(X = X, Y = 1, A = 1, wgt = 1, mu0 = 1, ps = 1,
                             initial.guess = c(1.0, 2.0, 3.0)),
               "`initial.guess` must be a named numeric vector")
  expect_error(.rootsOfScore(X = X, Y = 1, A = 1, wgt = 1, mu0 = 1, ps = 1,
                             initial.guess = c(1.0, 2.0, 3.0, 4.0)),
               "`initial.guess` must be a named numeric vector")
  initial.guess <- c("(Int)" = 1.0, "X1" = 2.0, "X2" = 3.0, "X3" = 4.0)

  expect_error(.rootsOfScore(X = X, Y = 1, A = 1, wgt = 1, mu0 = 1, ps = 1,
                             initial.guess = initial.guess, fit.name = 1),
               "`fit.name` must be a character object")
  expect_error(.rootsOfScore(X = X, Y = 1, A = 1, wgt = 1, mu0 = 1, ps = 1,
                             initial.guess = initial.guess,
                             fit.name = c("a", "b")),
               "`fit.name` must be a character object")
  expect_error(.rootsOfScore(X = X, Y = 1, A = 1, wgt = 1, mu0 = 1, ps = 1,
                             initial.guess = initial.guess,
                             fit.name = matrix("a", 1L, 1L)),
               "`fit.name` must be a character object")

  expect_error(.rootsOfScore(X = X, Y = 1, A = 1, wgt = 1, mu0 = 1, ps = 1,
                             initial.guess = initial.guess,
                             fit.name = "a"),
               "`outcome.type` must be one of 'cont' or 'bin'")
  expect_error(.rootsOfScore(X = X, Y = 1, A = 1, wgt = 1, mu0 = 1, ps = 1,
                             initial.guess = initial.guess,
                             fit.name = "a", outcome.type = 1.0),
               "`outcome.type` must be one of 'cont' or 'bin'")
  expect_error(.rootsOfScore(X = X, Y = 1, A = 1, wgt = 1, mu0 = 1, ps = 1,
                             initial.guess = initial.guess,
                             fit.name = "a", outcome.type = c("cont", "bin")),
               "`outcome.type` must be one of 'cont' or 'bin'")
  expect_error(.rootsOfScore(X = X, Y = 1, A = 1, wgt = 1, mu0 = 1, ps = 1,
                             initial.guess = initial.guess,
                             fit.name = "a", outcome.type = "binary"),
               "`outcome.type` must be one of 'cont' or 'bin'")

})

test_that("`.rootsOfScore()` returns expected results; cont", {

  n <- 1000L

  for (p in c(3L, 1L, 0L)) {
    withr::with_seed(1234L, {
      if (p > 0L) {
        X <- matrix(stats::rnorm(n*p), n, p,
                    dimnames = list(NULL, paste0("X", 1L:p)))
      } else {
        X <- matrix(NA_real_, n, 0L)
      }
      beta <- stats::runif(p+1)
      A <- stats::rbinom(n, 1, 0.3)

      Y <- {beta[1L] + X %*% beta[-1L] * {1 + A} + stats::rnorm(n)} |> drop()

      wgt <- rep(1.0, n)
      ps <- rep(0.5, n)
      mu0 <- Y + stats::runif(n, -0.25, 0.25)
      initial.guess <- stats::runif(p+1L)
    })

    if (p > 0L) {
      names(initial.guess) <- c("(Intercept)", paste0("X", 1L:p))
    } else {
      names(initial.guess) <- c("(Intercept)")
    }

    expected <- rootSolve::multiroot(f = .score.cont,
                                     start = initial.guess,
                                     X = X,
                                     Y = Y,
                                     A = A,
                                     wgt = wgt,
                                     ps = ps,
                                     mu0 = mu0)$root

    expect_equal(.rootsOfScore(X = X, Y = Y, A = A, wgt = wgt, mu0 = mu0, ps = ps,
                               initial.guess = initial.guess,
                               fit.name = "testing", outcome.type = "cont"),
                 expected)
  }


})

test_that("`.rootsOfScore()` returns expected results; binary", {

  n <- 1000L

  for (p in c(3L, 1L, 0L)) {
    withr::with_seed(1234L, {
      if (p > 0L) {
        X <- matrix(stats::rnorm(n*p), n, p,
                    dimnames = list(NULL, paste0("X", 1L:p)))
      } else {
        X <- matrix(NA_real_, n, 0L)
      }
      beta <- runif(p+1)
      A <- stats::rbinom(n, 1, 0.3)

      Y <- stats::rbinom(n, 1, 0.7)

      wgt <- rep(1.0, n)
      ps <- rep(0.5, n)
      mu0 <- Y + stats::runif(n, -0.25, 0.25)
      initial.guess <- stats::runif(p+1L)
    })

    if (p > 0L) {
      names(initial.guess) <- c("(Intercept)", paste0("X", 1L:p))
    } else {
      names(initial.guess) <- c("(Intercept)")
    }

    expected <- rootSolve::multiroot(f = .score.binary,
                                     start = initial.guess,
                                     X = X,
                                     Y = Y,
                                     A = A,
                                     wgt = wgt,
                                     ps = ps,
                                     mu0 = mu0)$root

    expect_equal(.rootsOfScore(X = X, Y = Y, A = A, wgt = wgt, mu0 = mu0, ps = ps,
                               initial.guess = initial.guess,
                               fit.name = "testing", outcome.type = "bin"),
                 expected)
  }


})

test_that("`.psiEstDataPrep()` returns expected errors", {

  expect_error(.psiEstDataPrep(),
               "`data` must be a named list containing elements 'X', 'Y', 'A', 'q', and 'est.ps'")
  expect_error(.psiEstDataPrep(data = matrix(1, 10, 10)),
               "`data` must be a named list containing elements 'X', 'Y', 'A', 'q', and 'est.ps'")
  expect_error(.psiEstDataPrep(data = list()),
               "`data` must be a named list containing elements 'X', 'Y', 'A', 'q', and 'est.ps'")
  expect_error(.psiEstDataPrep(data = list("X" = 1)),
               "`data` must be a named list containing elements 'X', 'Y', 'A', 'q', and 'est.ps'")
  expect_error(.psiEstDataPrep(data = list("X" = 1, "Y" = 1)),
               "`data` must be a named list containing elements 'X', 'Y', 'A', 'q', and 'est.ps'")
  expect_error(.psiEstDataPrep(data = list("X" = 1, "Y" = 1, "A" = 1)),
               "`data` must be a named list containing elements 'X', 'Y', 'A', 'q', and 'est.ps'")
  expect_error(.psiEstDataPrep(data = list("X" = 1, "Y" = 1, "A" = 1, "q" = 1)),
               "`data` must be a named list containing elements 'X', 'Y', 'A', 'q', and 'est.ps'")
  expect_error(.psiEstDataPrep(data = list("x" = 1, "Y" = 1, "A" = 1, "q" = 1, "est.ps" = 1)),
               "`data` must be a named list containing elements 'X', 'Y', 'A', 'q', and 'est.ps'")
  data <- list("X" = matrix(1, 10, 3, dimnames = list(NULL, c("X1", "X2", "X3"))),
               "Y" = 1, "A" = 1, "q" = 1, "est.ps" = 1)

  expect_error(.psiEstDataPrep(data = data),
               "`sieve.degree` must be provided")

  expect_error(.psiEstDataPrep(data = data, sieve.degree = 1),
               "`mainName` must be NULL or a character vector")
  expect_error(.psiEstDataPrep(data = data, sieve.degree = 1,
                               mainName = NA),
               "`mainName` must be NULL or a character vector")
  expect_error(.psiEstDataPrep(data = data, sieve.degree = 1,
                               mainName = c("x1", "X2")),
               "`mainName` must be NULL or a character vector")
  expect_error(.psiEstDataPrep(data = data, sieve.degree = 1,
                               mainName = matrix(c("X1", "X2"), 2, 1L)),
               "`mainName` must be NULL or a character vector")
  mainName <- c("X1", "X2", "X3")

  expect_error(.psiEstDataPrep(data = data, sieve.degree = 1,
                               mainName = mainName),
               "`contName` must be NULL or a character vector")
  expect_error(.psiEstDataPrep(data = data, sieve.degree = 1,
                               mainName = mainName, contName = NA),
               "`contName` must be NULL or a character vector")
  expect_error(.psiEstDataPrep(data = data, sieve.degree = 1,
                               mainName = mainName, contName = c("x1", "X2")),
               "`contName` must be NULL or a character vector")
  expect_error(.psiEstDataPrep(data = data, sieve.degree = 1,
                               mainName = mainName, contName = matrix(c("X1", "X2"), 2, 1L)),
               "`contName` must be NULL or a character vector")
  contName <- c("X1", "X2", "X3")

  expect_error(.psiEstDataPrep(data = data, sieve.degree = 1,
                               mainName = mainName, contName = contName),
               "`outcome.controls` must be provided")

  expect_error(.psiEstDataPrep(data = data, sieve.degree = 1,
                               mainName = mainName, contName = contName,
                               outcome.controls = list()),
               "`psName` must be NULL or a character vector")
  expect_error(.psiEstDataPrep(data = data, sieve.degree = 1,
                               mainName = mainName, contName = contName,
                               outcome.controls = list(),
                               psName = NA),
               "`psName` must be NULL or a character vector")
  expect_error(.psiEstDataPrep(data = data, sieve.degree = 1,
                               mainName = mainName, contName = contName,
                               outcome.controls = list(),
                               psName = c("x1", "X2")),
               "`psName` must be NULL or a character vector")
  expect_error(.psiEstDataPrep(data = data, sieve.degree = 1,
                               mainName = mainName, contName = contName,
                               outcome.controls = list(),
                               psName = matrix(c("X1", "X2"), 2, 1L)),
               "`psName` must be NULL or a character vector")
  psName <- c("X1", "X2", "X3")

  expect_error(.psiEstDataPrep(data = data, sieve.degree = 1,
                               mainName = mainName, contName = contName,
                               outcome.controls = list(),
                               psName = psName),
               "`ps.controls` must be provided")

  expect_error(.psiEstDataPrep(data = data, sieve.degree = 1,
                               mainName = mainName, contName = contName,
                               outcome.controls = list(),
                               psName = psName,
                               ps.controls = list()),
               "`fit.name` must be a character object")
  expect_error(.psiEstDataPrep(data = data, sieve.degree = 1,
                               mainName = mainName, contName = contName,
                               outcome.controls = list(),
                               psName = psName,
                               ps.controls = list(),
                               fit.name = 1),
               "`fit.name` must be a character object")
  expect_error(.psiEstDataPrep(data = data, sieve.degree = 1,
                               mainName = mainName, contName = contName,
                               outcome.controls = list(),
                               psName = psName,
                               ps.controls = list(),
                               fit.name = c("a", "b")),
               "`fit.name` must be a character object")
  expect_error(.psiEstDataPrep(data = data, sieve.degree = 1,
                               mainName = mainName, contName = contName,
                               outcome.controls = list(),
                               psName = psName,
                               ps.controls = list(),
                               fit.name = matrix("a", nrow = 1L, ncol = 1L)),
               "`fit.name` must be a character object")
})

test_that("`psiEstDataPrep()` returns expected results", {

  n <- 1000L
  p <- 3L

  withr::with_seed(1234L, {
    beta <- stats::runif(p + 1L, -1.0, 1.0)
    data <- list("X" = matrix(stats::rnorm(n*p), n, p,
                              dimnames = list(NULL, paste0("X", seq_len(p)))),
                 "A" = stats::rbinom(n, 1, 0.5),
                 "q" = rep(1, n))
    data$Y <- drop(data$X %*% beta[-1L]) + beta[1L] + stats::rnorm(n)
  })

  # do not estimate propensity
  data$est.ps <- FALSE

  expect_error(.psiEstDataPrep(data = data, sieve.degree = 2,
                               mainName = c("X1", "X2"), contName = c("X2", "X3"),
                               outcome.controls = list("family" = "gaussian"),
                               psName = c("X1", "X2", "X3"),
                               ps.controls = list("family" = "binomial"),
                               fit.name = "testing"),
               "ps must be provided for testing")

  data$ps <- rep(0.5, n)
  expected <- data
  expected$est.ps <- NULL
  expected$ps <- data$ps
  expected$ml.ps <- data$ps

  outcome <- .outcome(X = data$X[, c("X1", "X2"), drop = FALSE],
                      Y = data$Y,
                      A = data$A, wgt = data$q,
                      sieve.degree = 2L,
                      method.controls = list("family" = gaussian(),
                                             "SL.library" = "SL.glm"))
  expected[names(outcome)] <- outcome

  expect_equal(.psiEstDataPrep(data = data, sieve.degree = 2,
                               mainName = c("X1", "X2"), contName = c("X2", "X3"),
                               outcome.controls = list("family" = "gaussian",
                                                       "SL.library" = "SL.glm"),
                               psName = c("X1", "X2", "X3"),
                               ps.controls = list("family" = "binomial",
                                                  "SL.library" = "SL.glm"),
                               fit.name = "testing"),
               expected)

  # Estimate Propensity

  ps <- .propensityScore(X = data$X,
                         A = data$A,
                         wgt = data$q,
                         sieve.degree = 2L,
                         method.controls = list("family" = binomial,
                                                "SL.library" = "SL.glm"),
                         models = c("ps", "ml.ps"))
  expected[names(ps)] <- ps

  data$est.ps <- TRUE
  expect_equal(.psiEstDataPrep(data = data, sieve.degree = 2,
                               mainName = c("X1", "X2"), contName = c("X2", "X3"),
                               outcome.controls = list("family" = "gaussian",
                                                       "SL.library" = "SL.glm"),
                               psName = c("X1", "X2", "X3"),
                               ps.controls = list("family" = "binomial",
                                                  "SL.library" = "SL.glm"),
                               fit.name = "testing"),
               expected)


})


test_that("`psiEstDataPrep()` returns expected results; intercept only models", {

  n <- 1000L
  withr::with_seed(1234L, {
    beta <- stats::runif(1L, -1.0, 1.0)
    data <- list("X" = matrix(NA, n, 0L),
                 "Y" = rep(beta, n) + stats::rnorm(n),
                 "A" = stats::rbinom(n, 1, 0.5),
                 "q" = rep(1, n))
  })

  # do not estimate propensity

  data$est.ps <- FALSE
  expect_error(.psiEstDataPrep(data = data, sieve.degree = 2,
                               mainName = NULL, contName = NULL,
                               outcome.controls = list("family" = "gaussian",
                                                       "SL.library" = "SL.glm"),
                               psName = NULL,
                               ps.controls = list("family" = "binomial",
                                                  "SL.library" = "SL.glm"),
                               fit.name = "testing"),
               "ps must be provided for testing")

  expected <- data
  data$ps <-  rep(0.5, n)
  expected$ps <- data$ps
  expected$ml.ps <- data$ps

  outcome <- .outcome(X = data$X,
                      Y = data$Y,
                      A = data$A, wgt = data$q,
                      sieve.degree = 2L,
                      method.controls = list("family" = gaussian(),
                                             "SL.library" = "SL.glm"))
  expected[names(outcome)] <- outcome

  expected$est.ps <- NULL
  expect_equal(.psiEstDataPrep(data = data, sieve.degree = 2,
                               mainName = NULL, contName = NULL,
                               outcome.controls = list("family" = "gaussian",
                                                       "SL.library" = "SL.glm"),
                               psName = NULL,
                               ps.controls = list("family" = "binomial",
                                                  "SL.library" = "SL.glm"),
                               fit.name = "testing"),
               expected)

  # Estimate Propensity

  ps <- .propensityScore(X = data$X,
                         A = data$A,
                         wgt = data$q,
                         sieve.degree = 2L,
                         method.controls = list("family" = binomial,
                                                "SL.library" = "SL.glm"),
                         models = c("ps", "ml.ps"))
  expected[names(ps)] <- ps

  data$est.ps <- TRUE
  expect_equal(.psiEstDataPrep(data = data, sieve.degree = 2L,
                               mainName = NULL, contName = NULL,
                               outcome.controls = list("family" = "gaussian",
                                                       "SL.library" = "SL.glm"),
                               psName = NULL,
                               ps.controls = list("family" = "binomial",
                                                  "SL.library" = "SL.glm"),
                               fit.name = "testing"),
               expected)


})

test_that("`.psiEst()` returns expected errors", {
    expect_error(.psiEst(),
                 "`data.rct` must be a named list containing elements 'X', 'Y', 'A', 'q', and 'est.ps'")
    expect_error(.psiEst(data.rct = matrix(1, 10, 10)),
                 "`data.rct` must be a named list containing elements 'X', 'Y', 'A', 'q', and 'est.ps'")
    expect_error(.psiEst(data.rct = list()),
                 "`data.rct` must be a named list containing elements 'X', 'Y', 'A', 'q', and 'est.ps'")
    expect_error(.psiEst(data.rct = list("X" = 1)),
                 "`data.rct` must be a named list containing elements 'X', 'Y', 'A', 'q', and 'est.ps'")
    expect_error(.psiEst(data.rct = list("X" = 1, "Y" = 1)),
                 "`data.rct` must be a named list containing elements 'X', 'Y', 'A', 'q', and 'est.ps'")
    expect_error(.psiEst(data.rct = list("X" = 1, "Y" = 1, "A" = 1)),
                 "`data.rct` must be a named list containing elements 'X', 'Y', 'A', 'q', and 'est.ps'")
    expect_error(.psiEst(data.rct = list("X" = 1, "Y" = 1, "A" = 1, "q" = 1)),
                 "`data.rct` must be a named list containing elements 'X', 'Y', 'A', 'q', and 'est.ps'")
    expect_error(.psiEst(data.rct = list("x" = 1, "Y" = 1, "A" = 1, "q" = 1, "est.ps" = 1)),
                 "`data.rct` must be a named list containing elements 'X', 'Y', 'A', 'q', and 'est.ps'")
    data.rct <- list("X" = matrix(1, 10, 3, dimnames = list(NULL, c("X1", "X2", "X3"))),
                     "Y" = 1, "A" = 1, "q" = 1, "est.ps" = 1)

    expect_error(.psiEst(data.rct = data.rct),
                 "`data.rwe` must be a named list containing elements 'X', 'Y', 'A', and 'q'")
    expect_error(.psiEst(data.rct = data.rct, data.rwe = matrix(1, 10, 10)),
                 "`data.rwe` must be a named list containing elements 'X', 'Y', 'A', and 'q'")
    expect_error(.psiEst(data.rct = data.rct, data.rwe = list()),
                 "`data.rwe` must be a named list containing elements 'X', 'Y', 'A', and 'q'")
    expect_error(.psiEst(data.rct = data.rct, data.rwe = list("X" = 1)),
                 "`data.rwe` must be a named list containing elements 'X', 'Y', 'A', and 'q'")
    expect_error(.psiEst(data.rct = data.rct, data.rwe = list("X" = 1, "Y" = 1)),
                 "`data.rwe` must be a named list containing elements 'X', 'Y', 'A', and 'q'")
    expect_error(.psiEst(data.rct = data.rct, data.rwe = list("X" = 1, "Y" = 1, "A" = 1)),
                 "`data.rwe` must be a named list containing elements 'X', 'Y', 'A', and 'q'")
    expect_error(.psiEst(data.rct = data.rct, data.rwe = list("x" = 1, "Y" = 1, "A" = 1, "q" = 1)),
                 "`data.rwe` must be a named list containing elements 'X', 'Y', 'A', and 'q'")
    data.rwe <- list("X" = matrix(1, 10, 3, dimnames = list(NULL, c("X1A", "X2", "X3"))),
                     "Y" = 1, "A" = 1, "q" = 1, "est.ps" = 1)


    expect_error(.psiEst(data.rct = data.rct, data.rwe = data.rwe),
                 "`sieve.degree` must be provided")

    expect_error(.psiEst(data.rct = data.rct, data.rwe = data.rwe, sieve.degree = 2L),
                 "`outcome.type` must be one of 'cont' or 'bin'")
    expect_error(.psiEst(data.rct = data.rct, data.rwe = data.rwe, sieve.degree = 2L,
                         outcome.type = 1.0),
                 "`outcome.type` must be one of 'cont' or 'bin'")
    expect_error(.psiEst(data.rct = data.rct, data.rwe = data.rwe, sieve.degree = 2L,
                         outcome.type = c("cont", "bin")),
                 "`outcome.type` must be one of 'cont' or 'bin'")
    expect_error(.psiEst(data.rct = data.rct, data.rwe = data.rwe, sieve.degree = 2L,
                         outcome.type = matrix("bin", 1L, 1L)),
                 "`outcome.type` must be one of 'cont' or 'bin'")

    expect_error(.psiEst(data.rct = data.rct, data.rwe = data.rwe, sieve.degree = 2L,
                         outcome.type = "cont"),
                 "`mainName` must be NULL or a character vector")
    expect_error(.psiEst(data.rct = data.rct, data.rwe = data.rwe, sieve.degree = 2L,
                         outcome.type = "cont", mainName = NA),
                 "`mainName` must be NULL or a character vector")
    expect_error(.psiEst(data.rct = data.rct, data.rwe = data.rwe, sieve.degree = 2L,
                         outcome.type = "cont", mainName = c("x1", "X2")),
                 "`mainName` must be NULL or a character vector")
    expect_error(.psiEst(data.rct = data.rct, data.rwe = data.rwe, sieve.degree = 2L,
                         outcome.type = "cont", mainName = matrix(c("X1", "X2"), 2, 1L)),
                 "`mainName` must be NULL or a character vector")
    expect_error(.psiEst(data.rct = data.rct, data.rwe = data.rwe, sieve.degree = 2L,
                         outcome.type = "cont", mainName = c("X1", "X2", "X3")),
                 "`mainName` must be NULL or a character vector")
    mainName <- c("X2", "X3")

    expect_error(.psiEst(data.rct = data.rct, data.rwe = data.rwe, sieve.degree = 2L,
                         outcome.type = "cont", mainName = mainName),
                 "`contName` must be NULL or a character vector")
    expect_error(.psiEst(data.rct = data.rct, data.rwe = data.rwe, sieve.degree = 2L,
                         outcome.type = "cont", mainName = mainName, contName = NA),
                 "`contName` must be NULL or a character vector")
    expect_error(.psiEst(data.rct = data.rct, data.rwe = data.rwe, sieve.degree = 2L,
                         outcome.type = "cont", mainName = mainName,
                         contName = c("x1", "X2")),
                 "`contName` must be NULL or a character vector")
    expect_error(.psiEst(data.rct = data.rct, data.rwe = data.rwe, sieve.degree = 2L,
                         outcome.type = "cont", mainName = mainName,
                         contName = matrix(c("X1", "X2"), 2, 1L)),
                 "`contName` must be NULL or a character vector")
    expect_error(.psiEst(data.rct = data.rct, data.rwe = data.rwe, sieve.degree = 2L,
                         outcome.type = "cont", mainName = mainName,
                         contName = c("X1", "X2", "X3")),
                 "`contName` must be NULL or a character vector")
    contName <- c("X2", "X3")

    expect_error(.psiEst(data.rct = data.rct, data.rwe = data.rwe, sieve.degree = 2L,
                         outcome.type = "cont", mainName = mainName, contName = contName),
                 "`outcome.controls` must be provided")

    expect_error(.psiEst(data.rct = data.rct, data.rwe = data.rwe, sieve.degree = 2L,
                         outcome.type = "cont", mainName = mainName,
                         contName = contName,
                         outcome.controls = list()),
                 "`psName` must be NULL or a character vector")
    expect_error(.psiEst(data.rct = data.rct, data.rwe = data.rwe, sieve.degree = 2L,
                         outcome.type = "cont", mainName = mainName,
                         contName = contName,
                         outcome.controls = list(),
                         psName = NA),
                 "`psName` must be NULL or a character vector")
    expect_error(.psiEst(data.rct = data.rct, data.rwe = data.rwe, sieve.degree = 2L,
                         outcome.type = "cont", mainName = mainName,
                         contName = contName,
                         outcome.controls = list(),
                         psName = c("X1", "X2", "X3")),
                 "`psName` must be NULL or a character vector")
    expect_error(.psiEst(data.rct = data.rct, data.rwe = data.rwe, sieve.degree = 2L,
                         outcome.type = "cont", mainName = mainName,
                         contName = contName,
                         outcome.controls = list(),
                         psName = matrix(c("X1", "X2"), 2, 1L)),
                 "`psName` must be NULL or a character vector")
    psName <- c("X2", "X3")

    expect_error(.psiEst(data.rct = data.rct, data.rwe = data.rwe, sieve.degree = 2L,
                         outcome.type = "cont", mainName = mainName,
                         contName = contName,
                         outcome.controls = list(),
                         psName = psName),
                 "`ps.controls` must be provided")

})

test_that("`.psiEst()` returns expected results", {

  n <- 1000L
  p <- 3L

  withr::with_seed(1234L, {
    beta <- stats::runif(p + 1L, -1.0, 1.0)
    data.rct <- list("X" = matrix(stats::rnorm(n*p), n, p,
                                  dimnames = list(NULL, paste0("X", seq_len(p)))),
                     "A" = stats::rbinom(n, 1, 0.5),
                     "q" = rep(1, n))
    data.rct$Y <- drop(data.rct$X %*% beta[-1L]) + beta[1L] + stats::rnorm(n)
  })

  data.rct$est.ps <- TRUE
  data.rct <- .psiEstDataPrep(data = data.rct,
                              sieve.degree = 2L,
                              mainName = c("X2", "X3"),
                              contName = c("X2", "X3"),
                              outcome.controls = list("family" = gaussian(),
                                                      "SL.library" = "SL.glm"),
                              psName = c("X2", "X3"),
                              ps.controls = list("family" = binomial(),
                                                 "SL.library" = "SL.glm"),
                              fit.name = "RWE")
  data.rwe <- data.rct

  data.integ <- list("X" = rbind(data.rct$X, data.rwe$X),
                     "Y" = c(data.rct$Y, data.rwe$Y),
                     "A" = c(data.rct$A, data.rwe$A),
                     "q" = c(data.rct$q, data.rwe$q),
                     "ps" = c(data.rct$ps, data.rwe$ps),
                     "ml.ps" = c(data.rct$ml.ps, data.rwe$ml.ps),
                     "mu0" = c(data.rct$mu0, data.rwe$mu0),
                     "ml.mu0" = c(data.rct$ml.mu0, data.rwe$ml.mu0),
                     "ml.sigma0" = c(data.rct$ml.sigma0, data.rwe$ml.sigma0),
                     "ml.mu1" = c(data.rct$ml.mu1, data.rwe$ml.mu1))

  psi <- matrix(NA, nrow = 3L, ncol = 3L,
                dimnames = c(list(c("p", "eff", "rt"),
                                  c("(Intercept)", "X2", "X3"))))

  par <- numeric(3L)
  names(par) <- c("(Intercept)", "X2", "X3")

  psi["p", ] <- .rootsOfScore(X = data.integ$X[, c("X2", "X3")],
                              Y = data.integ$Y,
                              A = data.integ$A,
                              wgt = data.integ$q,
                              mu0 = data.integ$mu0,
                              ps = data.integ$ps,
                              initial.guess = par,
                              fit.name = "Preliminary Estimator psi_p",
                              outcome.type = "cont")
  par[] <- psi["p", ]

  psi["eff", ] <- .rootsOfScore(X = data.integ$X[, c("X2", "X3")],
                                Y = data.integ$Y,
                                A = data.integ$A,
                                wgt = data.integ$q,
                                mu0 = data.integ$ml.mu0,
                                ps = data.integ$ml.ps,
                                initial.guess = par,
                                fit.name = "Efficient Integrative Estimator psi_eff",
                                outcome.type = "cont")

  psi["rt", ] <- .rootsOfScore(X = data.rct$X[, c("X2", "X3")],
                               Y = data.rct$Y,
                               A = data.rct$A,
                               wgt = data.rct$q,
                               mu0 = data.rct$ml.mu0,
                               ps = data.rct$ml.ps,
                               initial.guess = par,
                               fit.name = "Initial Estimator psi_rt",
                               outcome.type = "cont")
  par[] <- psi["rt", ]
  weighted_score <- .evaluatedScore(data = data.rwe, psi = par,
                                    outcome.type = "cont",
                                    contName = c("X2", "X3"))

  expected <- list("psi" = psi, "weighted.score" = weighted_score)
  withr::with_seed(1234L, {
    beta <- stats::runif(p + 1L, -1.0, 1.0)
    data.rct <- list("X" = matrix(stats::rnorm(n*p), n, p,
                                  dimnames = list(NULL, paste0("X", seq_len(p)))),
                     "A" = stats::rbinom(n, 1, 0.5),
                     "q" = rep(1, n))
    data.rct$Y <- drop(data.rct$X %*% beta[-1L]) + beta[1L] + stats::rnorm(n)
  })
  data.rwe <- data.rct
  data.rct$est.ps <- TRUE

  expect_equal(.psiEst(data.rct = data.rct, data.rwe = data.rwe, sieve.degree = 2L,
                       outcome.type = "cont",
                       mainName = c("X2", "X3"), contName = c("X2", "X3"),
                       outcome.controls = list("family" = gaussian(),
                                               "SL.library" = "SL.glm"),
                       psName = c("X2", "X3"),
                       ps.controls = list("family" = binomial(),
                                          "SL.library" = "SL.glm")),
               expected)

})


test_that("`.psiEst()` returns expected results; one covariate", {

  n <- 1000L
  p <- 1L

  withr::with_seed(1234L, {
    beta <- stats::runif(p + 1L, -1.0, 1.0)
    data.rct <- list("X" = matrix(stats::rnorm(n*p), n, p,
                                  dimnames = list(NULL, paste0("X", seq_len(p)))),
                     "A" = stats::rbinom(n, 1, 0.5),
                     "q" = rep(1, n))
    data.rct$Y <- drop(data.rct$X %*% beta[-1L]) + beta[1L] + stats::rnorm(n)
  })

  data.rct$est.ps <- TRUE
  data.rct <- .psiEstDataPrep(data = data.rct,
                              sieve.degree = 2L,
                              mainName = c("X1"),
                              contName = c("X1"),
                              outcome.controls = list("family" = gaussian(),
                                                      "SL.library" = "SL.glm"),
                              psName = c("X1"),
                              ps.controls = list("family" = binomial(),
                                                 "SL.library" = "SL.glm"),
                              fit.name = "RWE")
  data.rwe <- data.rct

  data.integ <- list("X" = rbind(data.rct$X, data.rwe$X),
                     "Y" = c(data.rct$Y, data.rwe$Y),
                     "A" = c(data.rct$A, data.rwe$A),
                     "q" = c(data.rct$q, data.rwe$q),
                     "ps" = c(data.rct$ps, data.rwe$ps),
                     "ml.ps" = c(data.rct$ml.ps, data.rwe$ml.ps),
                     "mu0" = c(data.rct$mu0, data.rwe$mu0),
                     "ml.mu0" = c(data.rct$ml.mu0, data.rwe$ml.mu0),
                     "ml.sigma0" = c(data.rct$ml.sigma0, data.rwe$ml.sigma0),
                     "ml.mu1" = c(data.rct$ml.mu1, data.rwe$ml.mu1))

  psi <- matrix(NA, nrow = 3L, ncol = 2,
                dimnames = c(list(c("p", "eff", "rt"),
                                  c("(Intercept)", "X1"))))

  par <- numeric(2L)
  names(par) <- c("(Intercept)", "X1")

  psi["p", ] <- .rootsOfScore(X = data.integ$X[, c("X1"), drop = FALSE],
                              Y = data.integ$Y,
                              A = data.integ$A,
                              wgt = data.integ$q,
                              mu0 = data.integ$mu0,
                              ps = data.integ$ps,
                              initial.guess = par,
                              fit.name = "Preliminary Estimator psi_p",
                              outcome.type = "cont")
  par[] <- psi["p", ]

  psi["eff", ] <- .rootsOfScore(X = data.integ$X[, c("X1"), drop = FALSE],
                                Y = data.integ$Y,
                                A = data.integ$A,
                                wgt = data.integ$q,
                                mu0 = data.integ$ml.mu0,
                                ps = data.integ$ml.ps,
                                initial.guess = par,
                                fit.name = "Efficient Integrative Estimator psi_eff",
                                outcome.type = "cont")

  psi["rt", ] <- .rootsOfScore(X = data.rct$X[, c("X1"), drop = FALSE],
                               Y = data.rct$Y,
                               A = data.rct$A,
                               wgt = data.rct$q,
                               mu0 = data.rct$ml.mu0,
                               ps = data.rct$ml.ps,
                               initial.guess = par,
                               fit.name = "Initial Estimator psi_rt",
                               outcome.type = "cont")
  par[] <- psi["rt", ]
  weighted_score <- .evaluatedScore(data = data.rwe, psi = par,
                                    outcome.type = "cont",
                                    contName = c("X1"))

  expected <- list("psi" = psi, "weighted.score" = weighted_score)
  withr::with_seed(1234L, {
    beta <- stats::runif(p + 1L, -1.0, 1.0)
    data.rct <- list("X" = matrix(stats::rnorm(n*p), n, p,
                                  dimnames = list(NULL, paste0("X", seq_len(p)))),
                     "A" = stats::rbinom(n, 1, 0.5),
                     "q" = rep(1, n))
    data.rct$Y <- drop(data.rct$X %*% beta[-1L]) + beta[1L] + stats::rnorm(n)
  })
  data.rwe <- data.rct
  data.rct$est.ps <- TRUE

  expect_equal(.psiEst(data.rct = data.rct, data.rwe = data.rwe, sieve.degree = 2L,
                       outcome.type = "cont",
                       mainName = c("X1"), contName = c("X1"),
                       outcome.controls = list("family" = gaussian(),
                                               "SL.library" = "SL.glm"),
                       psName = c("X1"),
                       ps.controls = list("family" = binomial(),
                                          "SL.library" = "SL.glm")),
               expected)

})


test_that("`.psiEst()` returns expected results; no covariate", {

  n <- 1000L

  withr::with_seed(1234L, {
    data.rct <- list("X" = matrix(NA, n, 0),
                     "Y" = 0.5 + stats::rnorm(n),
                     "A" = stats::rbinom(n, 1, 0.5),
                     "q" = rep(1, n))
  })

  data.rct$est.ps <- TRUE
  data.rct <- .psiEstDataPrep(data = data.rct,
                              sieve.degree = 2L,
                              mainName = NULL,
                              contName = NULL,
                              outcome.controls = list("family" = gaussian(),
                                                      "SL.library" = "SL.glm"),
                              psName = NULL,
                              ps.controls = list("family" = binomial(),
                                                 "SL.library" = "SL.glm"),
                              fit.name = "RWE")
  data.rwe <- data.rct

  data.integ <- list("X" = rbind(data.rct$X, data.rwe$X),
                     "Y" = c(data.rct$Y, data.rwe$Y),
                     "A" = c(data.rct$A, data.rwe$A),
                     "q" = c(data.rct$q, data.rwe$q),
                     "ps" = c(data.rct$ps, data.rwe$ps),
                     "ml.ps" = c(data.rct$ml.ps, data.rwe$ml.ps),
                     "mu0" = c(data.rct$mu0, data.rwe$mu0),
                     "ml.mu0" = c(data.rct$ml.mu0, data.rwe$ml.mu0),
                     "ml.sigma0" = c(data.rct$ml.sigma0, data.rwe$ml.sigma0),
                     "ml.mu1" = c(data.rct$ml.mu1, data.rwe$ml.mu1))

  psi <- matrix(NA, nrow = 3L, ncol = 1,
                dimnames = c(list(c("p", "eff", "rt"),
                                  c("(Intercept)"))))

  par <- numeric(1L)
  names(par) <- c("(Intercept)")

  psi["p", ] <- .rootsOfScore(X = data.integ$X,
                              Y = data.integ$Y,
                              A = data.integ$A,
                              wgt = data.integ$q,
                              mu0 = data.integ$mu0,
                              ps = data.integ$ps,
                              initial.guess = par,
                              fit.name = "Preliminary Estimator psi_p",
                              outcome.type = "cont")
  par[] <- psi["p", ]

  psi["eff", ] <- .rootsOfScore(X = data.integ$X,
                                Y = data.integ$Y,
                                A = data.integ$A,
                                wgt = data.integ$q,
                                mu0 = data.integ$ml.mu0,
                                ps = data.integ$ml.ps,
                                initial.guess = par,
                                fit.name = "Efficient Integrative Estimator psi_eff",
                                outcome.type = "cont")

  psi["rt", ] <- .rootsOfScore(X = data.rct$X,
                               Y = data.rct$Y,
                               A = data.rct$A,
                               wgt = data.rct$q,
                               mu0 = data.rct$ml.mu0,
                               ps = data.rct$ml.ps,
                               initial.guess = par,
                               fit.name = "Initial Estimator psi_rt",
                               outcome.type = "cont")
  par[] <- psi["rt", ]
  weighted_score <- .evaluatedScore(data = data.rwe, psi = par,
                                    outcome.type = "cont",
                                    contName = NULL)

  expected <- list("psi" = psi, "weighted.score" = weighted_score)
  withr::with_seed(1234L, {
    data.rct <- list("X" = matrix(NA, n, 0L),
                     "Y" = 0.5 + stats::rnorm(n),
                     "A" = stats::rbinom(n, 1, 0.5),
                     "q" = rep(1, n))
  })
  data.rwe <- data.rct
  data.rct$est.ps <- TRUE

  expect_equal(.psiEst(data.rct = data.rct, data.rwe = data.rwe, sieve.degree = 2L,
                       outcome.type = "cont",
                       mainName = NULL, contName = NULL,
                       outcome.controls = list("family" = gaussian(),
                                               "SL.library" = "SL.glm"),
                       psName = NULL,
                       ps.controls = list("family" = binomial(),
                                          "SL.library" = "SL.glm")),
               expected)

})
