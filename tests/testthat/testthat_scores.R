test_that("`.score.no.confounding()` returns expected errors", {
  expect_error(.score.no.confounding(),
               "`psi` must be a named numeric vector")
  expect_error(.score.no.confounding(psi = list(1:3)),
               "`psi` must be a named numeric vector")
  expect_error(.score.no.confounding(psi = 1:3),
               "`psi` must be a named numeric vector")
  psi <- c("(Intercept)" = 1, "A" = 1, "B" = 2, "C" = 3)

  expect_error(.score.no.confounding(psi = psi),
               "`X` must be a named numeric matrix")
  expect_error(.score.no.confounding(psi = psi, X = list(1:3)),
               "`X` must be a named numeric matrix")
  expect_error(.score.no.confounding(psi = psi, X = psi),
               "`X` must be a named numeric matrix")
  expect_error(.score.no.confounding(psi = psi,
                                     X = matrix(1, 10L, 3L,
                                                dimnames = list(NULL, c("a", "B", "C")))),
               "`X` must be a named numeric matrix")
  expect_error(.score.no.confounding(psi = psi,
                                     X = matrix(1, 10L, 4L,
                                                dimnames = list(NULL, c("A", "B", "C", "(Intercept)")))),
               "`X` must be a named numeric matrix")
  X <- matrix(1.0, 10L, 3L)
  colnames(X) <- c("A", "B", "C")

  expect_error(.score.no.confounding(psi = psi, X = X),
               "`Y` must be a numeric vector")
  expect_error(.score.no.confounding(psi = psi, X = X, Y = list(1L:10L)),
               "`Y` must be a numeric vector")
  expect_error(.score.no.confounding(psi = psi, X = X, Y = rep("A", 10L)),
               "`Y` must be a numeric vector")
  expect_error(.score.no.confounding(psi = psi, X = X, Y = 1L:9L),
               "`Y` must be a numeric vector")
  Y <- rep(1.0, 10L)

  expect_error(.score.no.confounding(psi = psi, X = X, Y = Y),
               "`A` must be a binary vector")
  expect_error(.score.no.confounding(psi = psi, X = X, Y = Y, A = list(1L:10L)),
               "`A` must be a binary vector")
  expect_error(.score.no.confounding(psi = psi, X = X, Y = Y, A = rep("A", 10L)),
               "`A` must be a binary vector")
  expect_error(.score.no.confounding(psi = psi, X = X, Y = Y, A = c(rep(1.0, 5L), rep(0.0, 5L))),
               "`A` must be a binary vector")
  expect_error(.score.no.confounding(psi = psi, X = X, Y = Y, A = c(rep(1L, 2L), rep(0L, 5L), rep(3L, 3L))),
               "`A` must be a binary vector")
  A <- rep(1L, 10L)

  expect_error(.score.no.confounding(psi = psi, X = X, Y = Y, A = A),
               "`outcome.type` must be provided")

  expect_error(.score.no.confounding(psi = psi, X = X, Y = Y, A = A, outcome.type = "cont"),
               "`mu` must be a numeric vector")
  expect_error(.score.no.confounding(psi = psi, X = X, Y = Y, A = A, outcome.type = "cont",
                                     mu = list(1L:10L)),
               "`mu` must be a numeric vector")
  expect_error(.score.no.confounding(psi = psi, X = X, Y = Y, A = A, outcome.type = "cont",
                                     mu  = rep("A", 10L)),
               "`mu` must be a numeric vector")
  expect_error(.score.no.confounding(psi = psi, X = X, Y = Y, A = A, outcome.type = "cont",
                                     mu  = 1L:9L),
               "`mu` must be a numeric vector")

  expect_error(.score.no.confounding(psi = psi, X = X, Y = Y, A = A, outcome.type = "cont",
                                     mu = 1:nrow(X)),
               "`ps` must be a numeric vector")
  expect_error(.score.no.confounding(psi = psi, X = X, Y = Y, A = A, outcome.type = "cont",
                                     mu = 1:nrow(X), ps = list(1L:10L)),
               "`ps` must be a numeric vector")
  expect_error(.score.no.confounding(psi = psi, X = X, Y = Y, A = A, outcome.type = "cont",
                                     mu  = 1:nrow(X), ps = rep("A", 10L)),
               "`ps` must be a numeric vector")
  expect_error(.score.no.confounding(psi = psi, X = X, Y = Y, A = A, outcome.type = "cont",
                                     mu  = 1:nrow(X), ps = 1L:9L),
               "`ps` must be a numeric vector")

  expect_error(.score.no.confounding(psi = psi, X = X, Y = Y, A = A, outcome.type = "cont",
                                     mu = 1:nrow(X), ps = 1:nrow(X)),
               "`inv.sig2` must be a numeric vector")
  expect_error(.score.no.confounding(psi = psi, X = X, Y = Y, A = A, outcome.type = "cont",
                                     mu = 1:nrow(X), ps = 1:nrow(X), inv.sig2 = list(1L:10L)),
               "`inv.sig2` must be a numeric vector")
  expect_error(.score.no.confounding(psi = psi, X = X, Y = Y, A = A, outcome.type = "cont",
                                     mu  = 1:nrow(X), ps = 1:nrow(X), inv.sig2 = rep("A", 10L)),
               "`inv.sig2` must be a numeric vector")
  expect_error(.score.no.confounding(psi = psi, X = X, Y = Y, A = A, outcome.type = "cont",
                                     mu  = 1:nrow(X), ps =1:nrow(X), inv.sig2 =  1L:9L),
               "`inv.sig2` must be a numeric vector")

  expect_error(.score.no.confounding(psi = psi, X = X, Y = Y, A = A, outcome.type = "cont",
                                     mu = 1:nrow(X), ps = 1:nrow(X), inv.sig2 = 1:nrow(X)),
               "`wgt` must be a numeric vector")
  expect_error(.score.no.confounding(psi = psi, X = X, Y = Y, A = A, outcome.type = "cont",
                                     mu = 1:nrow(X), ps = 1:nrow(X), inv.sig2 = 1:nrow(X), wgt = list(1L:10L)),
               "`wgt` must be a numeric vector")
  expect_error(.score.no.confounding(psi = psi, X = X, Y = Y, A = A, outcome.type = "cont",
                                     mu  = 1:nrow(X), ps = 1:nrow(X), inv.sig2 = 1:nrow(X), wgt = rep("A", 10L)),
               "`wgt` must be a numeric vector")
  expect_error(.score.no.confounding(psi = psi, X = X, Y = Y, A = A, outcome.type = "cont",
                                     mu  = 1:nrow(X), ps =1:nrow(X), inv.sig2 =  1:nrow(X), wgt = 1L:9L),
               "`wgt` must be a numeric vector")

})

test_that("`.score.no.confounding()` returns expected result", {

  X <- matrix(1.0, 10L, 3L)
  colnames(X) <- c("A", "B", "C")
  Y <- rep(2.0, 10L)
  A <- rep(1L, 10L)
  mu0 <- rep(0.5, 10L)
  psi <- c("Intercept" = 0.5, "A" = 1.5, "B" = 2.5, "C" = 3.5)

  hte <- .HTE(psi = psi, X = X, outcome.type = "cont")
  H <- (2 - 0.5 - hte) * {0.5}

  expected <- rep(mean(H), 4)

  expect_equal(.score.no.confounding(psi = psi, X = X, Y = Y, A = A, outcome.type = "cont",
                                     mu = mu0, ps = rep(0.5, nrow(X)), inv.sig2 = rep(1, nrow(X)),
                                     wgt = rep(1, nrow(X))),
               expected)

  X[, 2L] <- 2.0
  X[, 3L] <- 3.0

  hte <- .HTE(psi = psi, X = X, outcome.type = "cont")
  H <- (2 - 0.5 - hte) * {0.5}
  expected <- c(rep(mean(H), 2), 2.0*mean(H), 3.0*mean(H))

  expect_equal(.score.no.confounding(psi = psi, X = X, Y = Y, A = A, outcome.type = "cont",
                                     mu = mu0, ps = rep(0.5, nrow(X)), inv.sig2 = rep(1, nrow(X)),
                                     wgt = rep(1, nrow(X))),
               expected)
})

test_that("`.score.no.confounding()` returns expected result; 1 covariate", {

  X <- matrix(1.0, 10L, 1L)
  colnames(X) <- c("A")
  Y <- rep(2.0, 10L)
  A <- rep(1L, 10L)
  mu0 <- rep(0.5, 10L)
  psi <- c("Intercept" = 0.5, "A" = 1.5)

  hte <- .HTE(psi = psi, X = X, outcome.type = "cont")
  H <- (2 - 0.5 - hte) * {0.5}

  expected <- rep(mean(H), 2)

  expect_equal(.score.no.confounding(psi = psi, X = X, Y = Y, A = A, outcome.type = "cont",
                                     mu = mu0, ps = rep(0.5, nrow(X)), inv.sig2 = rep(1, nrow(X)),
                                     wgt = rep(1, nrow(X))),
               expected)

})

test_that("`.score.no.confounding()` returns expected result; no covariate", {

  X <- matrix(1.0, 10L, 0L)
  Y <- rep(2.0, 10L)
  A <- rep(1L, 10L)
  mu0 <- rep(0.5, 10L)
  psi <- c("Intercept" = 0.5)

  hte <- .HTE(psi = psi, X = X, outcome.type = "cont")
  H <- (2 - 0.5 - hte) * {0.5}

  expected <- mean(H)

  expect_equal(.score.no.confounding(psi = psi, X = X, Y = Y, A = A, outcome.type = "cont",
                                     mu = mu0, ps = rep(0.5, nrow(X)), inv.sig2 = rep(1, nrow(X)),
                                     wgt = rep(1, nrow(X))),
               expected)

})


test_that("`.rootsOfScore()` returns expected errors", {

  expect_error(.rootsOfScore(),
               "`X` must be a named numeric matrix")
  expect_error(.rootsOfScore(X = list(1:3)),
               "`X` must be a named numeric matrix")
  expect_error(.rootsOfScore(X = matrix("a", 10, 3,
                                        dimnames = list(NULL, c("A", "B", "C")))),
               "`X` must be a named numeric matrix")
  expect_error(.rootsOfScore(X = matrix(1, 10, 3)),
               "`X` must be a named numeric matrix")
  X <- matrix(1.0, 10L, 3L)
  colnames(X) <- c("A", "B", "C")

  expect_error(.rootsOfScore(X = X),
               "`initial.guess` must be a named numeric vector")
  expect_error(.rootsOfScore(X = X, initial.guess = list("A" = 1, "B" = 2)),
               "`initial.guess` must be a named numeric vector")
  expect_error(.rootsOfScore(X = X, initial.guess = 1L:6L),
               "`initial.guess` must be a named numeric vector")
  initial.guess <- c("A" = 1, "B" = 2, "C" = 3)

  expect_error(.rootsOfScore(X = X, initial.guess = initial.guess),
               "`fit.name` must be a character object")
  expect_error(.rootsOfScore(X = X, initial.guess = initial.guess,
                             fit.name = 1),
               "`fit.name` must be a character object")
  expect_error(.rootsOfScore(X = X, initial.guess = initial.guess,
                             fit.name = list("a", "b")),
               "`fit.name` must be a character object")
  expect_error(.rootsOfScore(X = X, initial.guess = initial.guess,
                             fit.name = c("a", "b")),
               "`fit.name` must be a character object")

  expect_error(.rootsOfScore(X = X, initial.guess = initial.guess,
                             fit.name = "test"),
               "`score.func` must be one of 'basic' 'integ'")
  expect_error(.rootsOfScore(X = X, initial.guess = initial.guess,
                             fit.name = "test", score.func = 1),
               "`score.func` must be one of 'basic' 'integ'")
  expect_error(.rootsOfScore(X = X, initial.guess = initial.guess,
                             fit.name = "test", score.func = matrix("basic", 1, 1)),
               "`score.func` must be one of 'basic' 'integ'")
  expect_error(.rootsOfScore(X = X, initial.guess = initial.guess,
                             fit.name = "test", score.func = c("basic", "basic")),
               "`score.func` must be one of 'basic' 'integ'")
  expect_error(.rootsOfScore(X = X, initial.guess = initial.guess,
                             fit.name = "test", score.func = "bas"),
               "`score.func` must be one of 'basic' 'integ'")

  expect_error(.rootsOfScore(X = X, initial.guess = initial.guess,
                             fit.name = "test", score.func = "basic"),
               "`...` must contain additional inputs")


})

test_that("`rootsOfScore()` returns expected results", {
  n <- 10L
  X <- matrix(withr::with_seed(2345L, rnorm(30)), 10L, 3L)
  colnames(X) <- c("A", "B", "C")
  Y <- withr::with_seed(3456L, stats::rnorm(10, 0.6, 1.0))
  A <- withr::with_seed(4567L, stats::rbinom(10, 1, 0.3))
  mu0 <- rep(0.5, 10L)
  psi <- c("Intercept" = 0.5, "A" = 1.5, "B" = 2.5, "C" = 3.5)

  expected <- withr::with_seed(1234L,
                               rootSolve::multiroot(f = .score.no.confounding,
                                                    start = psi, X = X, Y = Y,
                                                    A = A, outcome.type = "cont",
                                                    mu = mu0, ps = rep(0.5, n),
                                                    wgt = rep(1.0, n),
                                                    inv.sig2 = rep(1.0, n))$root)

  expect_equal(withr::with_seed(1234L,
                                .rootsOfScore(X = X, initial.guess = psi,
                                              fit.name = "test",
                                              score.func = "basic",
                                              Y = Y, A = A, outcome.type = "cont",
                                              mu = mu0, ps = rep(0.5, n),
                                              wgt = rep(1.0, n),
                                              inv.sig2 = rep(1.0, n))),
               expected)

})
