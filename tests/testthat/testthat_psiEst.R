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
               "`outcome.method` must be provided")

  expect_error(.psiEstDataPrep(data = data, sieve.degree = 1,
                               mainName = mainName,
                               outcome.method = "glm"),
               "`outcome.controls` must be provided")

  expect_error(.psiEstDataPrep(data = data, sieve.degree = 1,
                               mainName = mainName,
                               outcome.method = "glm",
                               outcome.controls = list()),
               "`psName` must be NULL or a character vector")
  expect_error(.psiEstDataPrep(data = data, sieve.degree = 1,
                               mainName = mainName,
                               outcome.method = "glm",
                               outcome.controls = list(),
                               psName = NA),
               "`psName` must be NULL or a character vector")
  expect_error(.psiEstDataPrep(data = data, sieve.degree = 1,
                               mainName = mainName,
                               outcome.method = "glm",
                               outcome.controls = list(),
                               psName = c("x1", "X2")),
               "`psName` must be NULL or a character vector")
  expect_error(.psiEstDataPrep(data = data, sieve.degree = 1,
                               mainName = mainName,
                               outcome.method = "glm",
                               outcome.controls = list(),
                               psName = matrix(c("X1", "X2"), 2, 1L)),
               "`psName` must be NULL or a character vector")
  psName <- c("X1", "X2", "X3")

  expect_error(.psiEstDataPrep(data = data, sieve.degree = 1,
                               mainName = mainName,
                               outcome.method = "glm",
                               outcome.controls = list(),
                               psName = psName),
               "`ps.method` must be provided")

  expect_error(.psiEstDataPrep(data = data, sieve.degree = 1,
                               mainName = mainName,
                               outcome.method = "glm",
                               outcome.controls = list(),
                               psName = psName,
                               ps.method = "glm"),
               "`ps.controls` must be provided")

  expect_error(.psiEstDataPrep(data = data, sieve.degree = 1,
                               mainName = mainName,
                               outcome.method = "glm",
                               outcome.controls = list(),
                               psName = psName,
                               ps.method = "glm",
                               ps.controls = list()),
               "`fit.name` must be a character object")
  expect_error(.psiEstDataPrep(data = data, sieve.degree = 1,
                               mainName = mainName,
                               outcome.method = "glm",
                               outcome.controls = list(),
                               psName = psName,
                               ps.method = "glm",
                               ps.controls = list(),
                               fit.name = 1),
               "`fit.name` must be a character object")
  expect_error(.psiEstDataPrep(data = data, sieve.degree = 1,
                               mainName = mainName,
                               outcome.method = "glm",
                               outcome.controls = list(),
                               psName = psName,
                               ps.method = "glm",
                               ps.controls = list(),
                               fit.name = c("a", "b")),
               "`fit.name` must be a character object")
  expect_error(.psiEstDataPrep(data = data, sieve.degree = 1,
                               mainName = mainName,
                               outcome.method = "glm",
                               outcome.controls = list(),
                               psName = psName,
                               ps.method = "glm",
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
                               mainName = c("X1", "X2"),
                               outcome.method = "glm",
                               outcome.controls = list("family" = "gaussian"),
                               psName = c("X1", "X2", "X3"),
                               ps.method = "glm",
                               ps.controls = list("family" = "binomial"),
                               fit.name = "testing",
                               outcome.type = "cont"),
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
                      method = "glm",
                      method.controls = list("family" = gaussian()),
                      outcome.type = "cont")
  expected[names(outcome)] <- outcome

  expect_equal(.psiEstDataPrep(data = data, sieve.degree = 2,
                               mainName = c("X1", "X2"),
                               outcome.method = "glm",
                               outcome.controls = list("family" = "gaussian"),
                               psName = c("X1", "X2", "X3"),
                               ps.method = "glm",
                               ps.controls = list("family" = "binomial"),
                               fit.name = "testing",
                               outcome.type = "cont"),
               expected)

  # Estimate Propensity

  ps <- .propensityScore(X = data$X,
                         A = data$A,
                         wgt = data$q,
                         sieve.degree = 2L,
                         method = "glm",
                         method.controls = list("family" = binomial),
                         models = c("ps", "ml.ps"))
  expected[names(ps)] <- ps

  data$est.ps <- TRUE
  expect_equal(.psiEstDataPrep(data = data, sieve.degree = 2,
                               mainName = c("X1", "X2"),
                               outcome.method = "glm",
                               outcome.controls = list("family" = "gaussian"),
                               psName = c("X1", "X2", "X3"),
                               ps.method = "glm",
                               ps.controls = list("family" = "binomial"),
                               fit.name = "testing",
                               outcome.type = "cont"),
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
                               mainName = NULL,
                               outcome.method = "glm",
                               outcome.controls = list("family" = "gaussian"),
                               psName = NULL,
                               ps.method = "glm",
                               ps.controls = list("family" = "binomial"),
                               fit.name = "testing",
                               outcome.type = "cont"),
               "ps must be provided for testing")

  expected <- data
  data$ps <-  rep(0.5, n)
  expected$ps <- data$ps
  expected$ml.ps <- data$ps

  outcome <- .outcome(X = data$X,
                      Y = data$Y,
                      A = data$A, wgt = data$q,
                      sieve.degree = 2L,
                      method = "glm",
                      method.controls = list("family" = gaussian()),
                      outcome.type = "cont")
  expected[names(outcome)] <- outcome

  expected$est.ps <- NULL
  expect_equal(.psiEstDataPrep(data = data, sieve.degree = 2,
                               mainName = NULL,
                               outcome.method = "glm",
                               outcome.controls = list("family" = "gaussian"),
                               psName = NULL,
                               ps.method = "glm",
                               ps.controls = list("family" = "binomial"),
                               fit.name = "testing",
                               outcome.type = "cont"),
               expected)

  # Estimate Propensity

  ps <- .propensityScore(X = data$X,
                         A = data$A,
                         wgt = data$q,
                         sieve.degree = 2L,
                         method = "glm",
                         method.controls = list("family" = binomial),
                         models = c("ps", "ml.ps"))
  expected[names(ps)] <- ps

  data$est.ps <- TRUE
  expect_equal(.psiEstDataPrep(data = data, sieve.degree = 2L,
                               mainName = NULL,
                               outcome.method = "glm",
                               outcome.controls = list("family" = "gaussian"),
                               psName = NULL,
                               ps.method = "glm",
                               ps.controls = list("family" = "binomial"),
                               fit.name = "testing",
                               outcome.type = "cont"),
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
                 "`outcome.type` must be one of 'cont' or 'bin'")
    expect_error(.psiEst(data.rct = data.rct, data.rwe = data.rwe,
                         outcome.type = 1.0),
                 "`outcome.type` must be one of 'cont' or 'bin'")
    expect_error(.psiEst(data.rct = data.rct, data.rwe = data.rwe,
                         outcome.type = c("cont", "bin")),
                 "`outcome.type` must be one of 'cont' or 'bin'")
    expect_error(.psiEst(data.rct = data.rct, data.rwe = data.rwe,
                         outcome.type = matrix("bin", 1L, 1L)),
                 "`outcome.type` must be one of 'cont' or 'bin'")

    expect_error(.psiEst(data.rct = data.rct, data.rwe = data.rwe,
                         outcome.type = "cont"),
                 "`models` must be a list with elements 'RWE', 'RCT', 'outcome', 'ps', 'sieve.degree', and 'contName'")
    expect_error(.psiEst(data.rct = data.rct, data.rwe = data.rwe,
                         outcome.type = "cont", models = NA),
                 "`models` must be a list with elements 'RWE', 'RCT', 'outcome', 'ps', 'sieve.degree', and 'contName'")
    expect_error(.psiEst(data.rct = data.rct, data.rwe = data.rwe,
                         outcome.type = "cont",
                         models = list("RWE" = 1, "RCT" = 2, "outcome" = 3,
                                       "ps" = 4, "sieve.degree" = 5)),
                 "`models` must be a list with elements 'RWE', 'RCT', 'outcome', 'ps', 'sieve.degree', and 'contName'")
    expect_error(.psiEst(data.rct = data.rct, data.rwe = data.rwe,
                         outcome.type = "cont",
                         models = list("RWE" = 1, "RCT" = 2, "outcome" = 3,
                                       "ps" = 4, "sieve.degree" = 5, "contName" = 6)),
                 "elements 'RWE' and 'RCT' of 'models' must be lists with elements 'ME' and 'PS'")
    expect_error(.psiEst(data.rct = data.rct, data.rwe = data.rwe,
                         outcome.type = "cont",
                         models = list("RWE" = c("ME" = 1, "PS" = 2), "RCT" = 2, "outcome" = 3,
                                       "ps" = 4, "sieve.degree" = 5, "contName" = 6)),
                 "elements 'RWE' and 'RCT' of 'models' must be lists with elements 'ME' and 'PS'")

    expect_error(.psiEst(data.rct = data.rct, data.rwe = data.rwe,
                         outcome.type = "cont",
                         models = list("RWE" = list("ME" = 1, "PS" = 2), "RCT" = 2, "outcome" = 3,
                                       "ps" = 4, "sieve.degree" = 5, "contName" = 6)),
                 "elements 'RWE' and 'RCT' of 'models' must be lists with elements 'ME' and 'PS'")

    expect_error(.psiEst(data.rct = data.rct, data.rwe = data.rwe,
                         outcome.type = "cont",
                         models = list("RWE" = list("ME" = 1, "PS" = 2), "RCT" = c("ME" = 1, "PS" = 2), "outcome" = 3,
                                       "ps" = 4, "sieve.degree" = 5, "contName" = 6)),
                 "elements 'RWE' and 'RCT' of 'models' must be lists with elements 'ME' and 'PS'")

        expect_error(.psiEst(data.rct = data.rct, data.rwe = data.rwe,
                         outcome.type = "cont",
                         models = list("RWE" = list("ME" = 1, "PS" = 2), "RCT" = list("ME" = 1, "PS" = 2), "outcome" = 3,
                                       "ps" = 4, "sieve.degree" = 5, "contName" = 6)),
                     "elements 'outcome' and 'ps' of 'models' must be lists with elements 'method' and 'controls'")

        expect_error(.psiEst(data.rct = data.rct, data.rwe = data.rwe,
                             outcome.type = "cont",
                             models = list("RWE" = list("ME" = 1, "PS" = 2),
                                           "RCT" = list("ME" = 1, "PS" = 2),
                                           "outcome" = c("method" = 1, "controls" = 2),
                                           "ps" = 4, "sieve.degree" = 5, "contName" = 6)),
                     "elements 'outcome' and 'ps' of 'models' must be lists with elements 'method' and 'controls'")
        expect_error(.psiEst(data.rct = data.rct, data.rwe = data.rwe,
                             outcome.type = "cont",
                             models = list("RWE" = list("ME" = 1, "PS" = 2),
                                           "RCT" = list("ME" = 1, "PS" = 2),
                                           "outcome" = list("method" = 1, "controls" = 2),
                                           "ps" = 4, "sieve.degree" = 5, "contName" = 6)),
                     "elements 'outcome' and 'ps' of 'models' must be lists with elements 'method' and 'controls'")
        expect_error(.psiEst(data.rct = data.rct, data.rwe = data.rwe,
                             outcome.type = "cont",
                             models = list("RWE" = list("ME" = 1, "PS" = 2),
                                           "RCT" = list("ME" = 1, "PS" = 2),
                                           "outcome" = list("method" = 1, "controls" = 2),
                                           "ps" = c("method" = 1, "controls" = 2),
                                           "sieve.degree" = 5, "contName" = 6)),
                     "elements 'outcome' and 'ps' of 'models' must be lists with elements 'method' and 'controls'")
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
                              outcome.method = "glm",
                              outcome.controls = list("family" = gaussian()),
                              psName = c("X2", "X3"),
                              ps.method = "glm",
                              ps.controls = list("family" = binomial()),
                              fit.name = "RWE",
                              outcome.type = "cont")
  data.rwe <- data.rct

  data.integ <- list("X" = rbind(data.rct$X, data.rwe$X),
                     "Y" = c(data.rct$Y, data.rwe$Y),
                     "A" = c(data.rct$A, data.rwe$A),
                     "q" = c(data.rct$q, data.rwe$q),
                     "ps" = c(data.rct$ps, data.rwe$ps),
                     "ml.ps" = c(data.rct$ml.ps, data.rwe$ml.ps),
                     "mu0" = c(data.rct$me0, data.rwe$me0),
                     "ml.mu0" = c(data.rct$ml.me0, data.rwe$ml.me0),
                     "ml.sigma0" = c(rep(data.rct$ml.me0.conditional.var, nrow(data.rct$X)),
                                     rep(data.rwe$ml.me0.conditional.var, nrow(data.rwe$X))),
                     "ml.mu1" = c(data.rct$ml.me1, data.rwe$ml.me1))

  psi <- matrix(NA, nrow = 3L, ncol = 3L,
                dimnames = c(list(c("p", "eff", "rt"),
                                  c("(Intercept)", "X2", "X3"))))

  par <- numeric(3L)
  names(par) <- c("(Intercept)", "X2", "X3")

  psi["p", ] <- .rootsOfScore(X = data.integ$X[, c("X2", "X3")],
                              initial.guess = par,
                              fit.name = "Preliminary Estimator psi_p",
                              score.func = "basic",
                              Y = data.integ$Y,
                              A = data.integ$A,
                              outcome.type = "cont",
                              ps = data.integ$ps,
                              inv.sig2 = 1.0 / data.integ$ml.sigma0,
                              wgt = data.integ$q,
                              mu = data.integ$mu0)
  par[] <- psi["p", ]

  psi["eff", ] <- .rootsOfScore(X = data.integ$X[, c("X2", "X3")],
                                initial.guess = par,
                                fit.name = "Efficient Integrative Estimator psi_eff",
                                score.func = "basic",
                                Y = data.integ$Y,
                                A = data.integ$A,
                                outcome.type = "cont",
                                ps = data.integ$ml.ps,
                                inv.sig2 = 1.0 / data.integ$ml.sigma0,
                                wgt = data.integ$q,
                                mu = data.integ$ml.mu0)

  psi["rt", ] <- .rootsOfScore(X = data.rct$X[, c("X2", "X3")],
                               initial.guess = par,
                               fit.name = "Initial Estimator psi_rt",
                               score.func = "basic",
                               Y = data.rct$Y,
                               A = data.rct$A,
                               outcome.type = "cont",
                               ps = data.rct$ml.ps,
                               inv.sig2 = 1.0 / rep(data.rct$ml.me0.conditional.var, nrow(data.rct$X)),
                               wgt = data.rct$q,
                               mu = data.rct$ml.me0)
  par[] <- psi["rt", ]
  weighted_score <- .score.no.confounding(psi = par,
                                          X = data.rwe$X[, c("X2", "X3")],
                                          Y = data.rwe$Y,
                                          A = data.rwe$A,
                                          outcome.type = "cont",
                                          mu = data.rwe$ml.me0,
                                          ps = data.rwe$ml.ps,
                                          inv.sig2 = data.rwe$ml.inv.sig2,
                                          wgt = data.rwe$q)
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

  models <- list("RCT" = list("ME" = c("X2", "X3"), "PS" = c("X2", "X3")),
                 "RWE" = list("ME" = c("X2", "X3"), "PS" = c("X2", "X3")),
                 "sieve.degree" = 2L,
                 "contName" = c("X2", "X3"),
                 "outcome" = list(method = "glm", controls = list("family" = gaussian())),
                 "ps" = list(method = "glm", controls = list("family" = binomial())))

  expect_equal(.psiEst(data.rct = data.rct, data.rwe = data.rwe,
                       outcome.type = "cont",
                       models = models),
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
                              outcome.method = "glm",
                              outcome.controls = list("family" = gaussian()),
                              psName = c("X1"),
                              ps.method = "glm",
                              ps.controls = list("family" = binomial()),
                              fit.name = "RWE",
                              outcome.type = "cont")
  data.rwe <- data.rct

  data.integ <- list("X" = rbind(data.rct$X, data.rwe$X),
                     "Y" = c(data.rct$Y, data.rwe$Y),
                     "A" = c(data.rct$A, data.rwe$A),
                     "q" = c(data.rct$q, data.rwe$q),
                     "ps" = c(data.rct$ps, data.rwe$ps),
                     "ml.ps" = c(data.rct$ml.ps, data.rwe$ml.ps),
                     "mu0" = c(data.rct$me0, data.rwe$me0),
                     "ml.mu0" = c(data.rct$ml.me0, data.rwe$ml.me0),
                     "ml.sigma0" = c(rep(data.rct$ml.me0.conditional.var, nrow(data.rct$X)),
                                     rep(data.rwe$ml.me0.conditional.var, nrow(data.rwe$X))),
                     "ml.mu1" = c(data.rct$ml.me1, data.rwe$ml.me1))


  psi <- matrix(NA, nrow = 3L, ncol = 2,
                dimnames = c(list(c("p", "eff", "rt"),
                                  c("(Intercept)", "X1"))))

  par <- numeric(2L)
  names(par) <- c("(Intercept)", "X1")

  psi["p", ] <- .rootsOfScore(X = data.integ$X[, c("X1"), drop = FALSE],
                              initial.guess = par,
                              fit.name = "Preliminary Estimator psi_p",
                              score.func = "basic",
                              Y = data.integ$Y,
                              A = data.integ$A,
                              outcome.type = "cont",
                              ps = data.integ$ps,
                              inv.sig2 = 1.0 / data.integ$ml.sigma0,
                              wgt = data.integ$q,
                              mu = data.integ$mu0)
  par[] <- psi["p", ]

  psi["eff", ] <- .rootsOfScore(X = data.integ$X[, c("X1"), drop = FALSE],
                                initial.guess = par,
                                fit.name = "Efficient Integrative Estimator psi_eff",
                                score.func = "basic",
                                Y = data.integ$Y,
                                A = data.integ$A,
                                outcome.type = "cont",
                                ps = data.integ$ml.ps,
                                inv.sig2 = 1.0 / data.integ$ml.sigma0,
                                wgt = data.integ$q,
                                mu = data.integ$ml.mu0)

  psi["rt", ] <- .rootsOfScore(X = data.rct$X[, c("X1"), drop = FALSE],
                               initial.guess = par,
                               fit.name = "Initial Estimator psi_rt",
                               score.func = "basic",
                               Y = data.rct$Y,
                               A = data.rct$A,
                               outcome.type = "cont",
                               ps = data.rct$ml.ps,
                               inv.sig2 = 1.0 / rep(data.rct$ml.me0.conditional.var, nrow(data.rct$X)),
                               wgt = data.rct$q,
                               mu = data.rct$ml.me0)
  par[] <- psi["rt", ]
  weighted_score <- .score.no.confounding(psi = par,
                                          X = data.rwe$X[, c("X1"), drop = FALSE],
                                          Y = data.rwe$Y,
                                          A = data.rwe$A,
                                          outcome.type = "cont",
                                          mu = data.rwe$ml.me0,
                                          ps = data.rwe$ml.ps,
                                          inv.sig2 = data.rwe$ml.inv.sig2,
                                          wgt = data.rwe$q)

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

  models = list("RCT" = list("ME" = "X1", "PS" = "X1"),
                "RWE" = list("ME" = "X1", "PS" = "X1"),
                "contName" = "X1",
                sieve.degree = 2L,
                "outcome" = list("method" = "glm", controls = list("family" = gaussian())),
                "ps" = list("method" = "glm", controls = list("family" = binomial())))

  expect_equal(.psiEst(data.rct = data.rct, data.rwe = data.rwe,
                       outcome.type = "cont",
                       models = models),
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
                              outcome.method = "glm",
                              outcome.controls = list("family" = gaussian()),
                              psName = NULL,
                              ps.method = "glm",
                              ps.controls = list("family" = binomial()),
                              fit.name = "RWE",
                              outcome.type = "cont")
  data.rwe <- data.rct

  data.integ <- list("X" = rbind(data.rct$X, data.rwe$X),
                     "Y" = c(data.rct$Y, data.rwe$Y),
                     "A" = c(data.rct$A, data.rwe$A),
                     "q" = c(data.rct$q, data.rwe$q),
                     "ps" = c(data.rct$ps, data.rwe$ps),
                     "ml.ps" = c(data.rct$ml.ps, data.rwe$ml.ps),
                     "mu0" = c(data.rct$me0, data.rwe$me0),
                     "ml.mu0" = c(data.rct$ml.me0, data.rwe$ml.me0),
                     "ml.sigma0" = c(rep(data.rct$ml.me0.conditional.var, nrow(data.rct$X)),
                                     rep(data.rwe$ml.me0.conditional.var, nrow(data.rwe$X))),
                     "ml.mu1" = c(data.rct$ml.me1, data.rwe$ml.me1))


  psi <- matrix(NA, nrow = 3L, ncol = 1,
                dimnames = c(list(c("p", "eff", "rt"),
                                  c("(Intercept)"))))

  par <- numeric(1L)
  names(par) <- c("(Intercept)")

  psi["p", ] <- .rootsOfScore(X = data.integ$X,
                              initial.guess = par,
                              fit.name = "Preliminary Estimator psi_p",
                              score.func = "basic",
                              Y = data.integ$Y,
                              A = data.integ$A,
                              outcome.type = "cont",
                              ps = data.integ$ps,
                              inv.sig2 = 1.0 / data.integ$ml.sigma0,
                              wgt = data.integ$q,
                              mu = data.integ$mu0)
  par[] <- psi["p", ]

  psi["eff", ] <- .rootsOfScore(X = data.integ$X,
                                initial.guess = par,
                                fit.name = "Efficient Integrative Estimator psi_eff",
                                score.func = "basic",
                                Y = data.integ$Y,
                                A = data.integ$A,
                                outcome.type = "cont",
                                ps = data.integ$ml.ps,
                                inv.sig2 = 1.0 / data.integ$ml.sigma0,
                                wgt = data.integ$q,
                                mu = data.integ$ml.mu0)

  psi["rt", ] <- .rootsOfScore(X = data.rct$X,
                               initial.guess = par,
                               fit.name = "Initial Estimator psi_rt",
                               score.func = "basic",
                               Y = data.rct$Y,
                               A = data.rct$A,
                               outcome.type = "cont",
                               ps = data.rct$ml.ps,
                               inv.sig2 = 1.0 / rep(data.rct$ml.me0.conditional.var, nrow(data.rct$X)),
                               wgt = data.rct$q,
                               mu = data.rct$ml.me0)
  par[] <- psi["rt", ]
  weighted_score <- .score.no.confounding(psi = par,
                                          X = data.rwe$X,
                                          Y = data.rwe$Y,
                                          A = data.rwe$A,
                                          outcome.type = "cont",
                                          mu = data.rwe$ml.me0,
                                          ps = data.rwe$ml.ps,
                                          inv.sig2 = data.rwe$ml.inv.sig2,
                                          wgt = data.rwe$q)

  expected <- list("psi" = psi, "weighted.score" = weighted_score)
  withr::with_seed(1234L, {
    data.rct <- list("X" = matrix(NA, n, 0L),
                     "Y" = 0.5 + stats::rnorm(n),
                     "A" = stats::rbinom(n, 1, 0.5),
                     "q" = rep(1, n))
  })
  data.rwe <- data.rct
  data.rct$est.ps <- TRUE

  models = list("RCT" = list("ME" = NULL, "PS" = NULL),
                "RWE" = list("ME" = NULL, "PS" = NULL),
                "contName" = NULL,
                sieve.degree = 2L,
                "outcome" = list("method" = "glm", controls = list("family" = gaussian())),
                "ps" = list("method" = "glm", controls = list("family" = binomial())))

  expect_equal(.psiEst(data.rct = data.rct, data.rwe = data.rwe,
                       outcome.type = "cont",
                       models = models),
               expected)

})
