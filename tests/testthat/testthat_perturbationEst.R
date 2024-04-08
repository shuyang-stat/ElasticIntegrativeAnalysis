test_that("`.perturbationEstIteration()` returns expected errors", {

    expect_error(.perturbationEstIteration(),
                 "`data.rct` must be a list containing X")
    expect_error(.perturbationEstIteration(data.rct = matrix(1, 10L, 3L)),
                 "`data.rct` must be a list containing X")
    expect_error(.perturbationEstIteration(data.rct = list(Y = 1:3L)),
                 "`data.rct` must be a list containing X")
    data.rct <- list("X" = matrix(1, 10L, 3L))

    expect_error(.perturbationEstIteration(data.rct = data.rct),
                 "`data.rwe` must be a list containing X")
    expect_error(.perturbationEstIteration(data.rct = data.rct,
                                           data.rwe = matrix(1, 10L, 3L)),
                 "`data.rwe` must be a list containing X")
    expect_error(.perturbationEstIteration(data.rct = data.rct,
                                           data.rwe = list(Y = 1:3L)),
                 "`data.rwe` must be a list containing X")
    data.rwe <- list("X" = matrix(1, 20L, 3L))

    expect_error(.perturbationEstIteration(data.rct = data.rct,
                                           data.rwe = data.rwe),
                 "`data.rct$X` must be a named numeric matrix", fixed = TRUE)
    colnames(data.rct$X) <- c("X1", "X2", "X3")

    expect_error(.perturbationEstIteration(data.rct = data.rct,
                                           data.rwe = data.rwe),
                 "`data.rwe$X` must be a named numeric matrix", fixed = TRUE)
    colnames(data.rwe$X) <- c("X1", "X2", "X3")

    expect_error(.perturbationEstIteration(data.rct = data.rct,
                                           data.rwe = data.rwe),
                 "`outcome.type` must be provided")
    expect_error(.perturbationEstIteration(data.rct = data.rct,
                                           data.rwe = data.rwe,
                                           outcome.type = "bin"),
                 "`models` must be provided")
})

test_that("`.perturbationEstIteration()` returns expected results", {

  n <- 1000L
  p <- 3L

  withr::with_seed(1234L, {
    data.rct <- list("X" = matrix(stats::rnorm(n*p), n, p,
                                  dimnames = list(NULL, paste0("X", seq_len(p)))),
                     "Y" = stats::rnorm(n, 1),
                     "A" = stats::rbinom(n, 1, 0.5),
                     "q" = rep(1, n))
  })
  data.rwe <- data.rct
  data.rct$est.ps <- TRUE

  withr::with_seed(2345L, {
    data.rct$q <- stats::rexp(nrow(data.rct$X))
    data.rwe$q <- stats::rexp(nrow(data.rwe$X))
  })

  models <- list("RCT" = list("ME" = c("X1", "X2"),
                              "PS" = c("X1", "X2", "X3")),
                 "RWE" = list("ME" = c("X1"),
                              "PS" = c("X1", "X2")),
                 "outcome" = list("method" = "glm",
                                  "controls" = list("family" = "gaussian")),
                 "ps" = list("method" = "glm",
                             "controls" = list("family" = "quasibinomial")),
                 "sieve.degree" = 2L,
                 "contName" = c("X2", "X3"))

  psi_list_p <- .psiEst(data.rct = data.rct, data.rwe = data.rwe,
                        outcome.type = "cont", models = models)
  rownms <- rownames(psi_list_p$psi)
  colnms <- colnames(psi_list_p$psi)
  nms <- NULL
  values <- NULL
  for (i in seq_along(rownms)) {
    for (j in seq_along(colnms)) {
      nms <- c(nms, paste(rownms[i], colnms[j], sep = "."))
    }
    values <- c(values, psi_list_p$psi[i, ])
  }
  names(values) <- nms
  psi_list_p$psi <- values

  data.rct$q <- rep(1.0, n)
  data.rwe$q <- rep(1.0, n)

  expect_equal(withr::with_seed(2345L,
                                .perturbationEstIteration(data.rwe = data.rwe,
                                                          data.rct = data.rct,
                                                          outcome.type = "cont",
                                                          models = models)),
               psi_list_p)

})

test_that("`.perturbationEstIteration()` returns expected results; one covariate", {

  n <- 1000L
  p <- 3L

  withr::with_seed(1234L, {
    data.rct <- list("X" = matrix(stats::rnorm(n*p), n, p,
                                  dimnames = list(NULL, c("X1", "X2", "X3"))),
                     "Y" = stats::rnorm(n, 1),
                     "A" = stats::rbinom(n, 1, 0.5),
                     "q" = rep(1, n))
  })
  data.rwe <- data.rct
  data.rct$est.ps <- TRUE

  withr::with_seed(2345L, {
    data.rct$q <- stats::rexp(nrow(data.rct$X))
    data.rwe$q <- stats::rexp(nrow(data.rwe$X))
  })

  models <- list("RCT" = list("ME" = c("X1"),
                              "PS" = c("X3")),
                 "RWE" = list("ME" = c("X2"),
                              "PS" = c("X1")),
                 "outcome" = list("method" = "glm",
                                  "controls" = list("family" = "gaussian")),
                 "ps" = list("method" = "glm",
                             "controls" = list("family" = "quasibinomial")),
                 "sieve.degree" = 2L,
                 "contName" = c("X2"))

  psi_list_p <- .psiEst(data.rct = data.rct, data.rwe = data.rwe,
                        outcome.type = "cont",
                        models = models)
  rownms <- rownames(psi_list_p$psi)
  colnms <- colnames(psi_list_p$psi)
  nms <- NULL
  values <- NULL
  for (i in seq_along(rownms)) {
    for (j in seq_along(colnms)) {
      nms <- c(nms, paste(rownms[i], colnms[j], sep = "."))
    }
    values <- c(values, psi_list_p$psi[i, ])
  }
  names(values) <- nms
  psi_list_p$psi <- values

  data.rct$q <- rep(1, n)
  data.rwe$q <- rep(1, n)

  expect_equal(withr::with_seed(2345L,
                                .perturbationEstIteration(data.rwe = data.rwe,
                                                          data.rct = data.rct,
                                                          outcome.type = "cont",
                                                          models = models)),
               psi_list_p)

})

test_that("`.perturbationEstIteration()` returns expected results; no covariates", {

  n <- 1000L

  withr::with_seed(1234L, {
    data.rct <- list("X" = matrix(NA, n, 0L),
                     "Y" = stats::rnorm(n, 1),
                     "A" = stats::rbinom(n, 1, 0.5),
                     "q" = rep(1, n))
  })
  data.rwe <- data.rct
  data.rct$est.ps <- TRUE

  withr::with_seed(2345L, {
    data.rct$q <- stats::rexp(nrow(data.rct$X))
    data.rwe$q <- stats::rexp(nrow(data.rwe$X))
  })

  models <- list("RCT" = list("ME" = NULL,
                              "PS" = NULL),
                 "RWE" = list("ME" = NULL,
                              "PS" = NULL),
                 "outcome" = list("method" = "glm",
                                  "controls" = list("family" = "gaussian")),
                 "ps" = list("method" = "glm",
                             "controls" = list("family" = "quasibinomial")),
                 "sieve.degree" = 2L,
                 "contName" = NULL)



  psi_list_p <- .psiEst(data.rct = data.rct, data.rwe = data.rwe,
                        outcome.type = "cont",
                        models = models)
  rownms <- rownames(psi_list_p$psi)
  colnms <- colnames(psi_list_p$psi)
  nms <- NULL
  values <- NULL
  for (i in seq_along(rownms)) {
    for (j in seq_along(colnms)) {
      nms <- c(nms, paste(rownms[i], colnms[j], sep = "."))
    }
    values <- c(values, psi_list_p$psi[i, ])
  }
  names(values) <- nms
  psi_list_p$psi <- values

  data.rct$q <- rep(1, n)
  data.rwe$q <- rep(1, n)

  expect_equal(withr::with_seed(2345L,
                                .perturbationEstIteration(data.rwe = data.rwe,
                                                          data.rct = data.rct,
                                                          outcome.type = "cont",
                                                          models = models)),
               psi_list_p)

})

test_that("`.perturbationEst()` returns expected errors", {

    expect_error(.perturbationEst(),
                 "`data.rct` must be provided")
    data.rct <- list("X" = matrix(1, 10L, 3L))

    expect_error(.perturbationEst(data.rct = data.rct),
                 "`data.rwe` must be provided")
    data.rwe <- list("X" = matrix(1, 20L, 3L))

    expect_error(.perturbationEst(data.rct = data.rct, data.rwe = data.rwe),
                 "`n.pert` must be a positive integer")
    expect_error(.perturbationEst(data.rct = data.rct, data.rwe = data.rwe,
                                  n.pert = 1.9),
                 "`n.pert` must be a positive integer")
    expect_error(.perturbationEst(data.rct = data.rct, data.rwe = data.rwe,
                                  n.pert = c(100L, 200L)),
                 "`n.pert` must be a positive integer")
    expect_error(.perturbationEst(data.rct = data.rct, data.rwe = data.rwe,
                                  n.pert = 0L),
                 "`n.pert` must be a positive integer")

    expect_error(.perturbationEst(data.rct = data.rct,
                                  data.rwe = data.rwe,
                                  n.pert = 100L),
                 "`outcome.type` must be provided")
    expect_error(.perturbationEst(data.rct = data.rct,
                                  data.rwe = data.rwe,
                                  n.pert = 100L,
                                  outcome.type = "bin"),
                 "`models` must be provided")
})

test_that("`.perturbationEst()` returns expected results", {

  n <- 1000L
  p <- 3L

  withr::with_seed(1234L, {
    data.rct <- list("X" = matrix(stats::rnorm(n*p), n, p,
                                  dimnames = list(NULL, paste0("X", seq_len(p)))),
                     "Y" = stats::rnorm(n, 1),
                     "A" = stats::rbinom(n, 1, 0.5),
                     "q" = rep(1, n))
  })
  data.rwe <- data.rct
  data.rct$est.ps <- TRUE

  ptb <- NULL
  score <- NULL
  models <- list("RCT" = list("ME" = c("X1", "X2"),
                              "PS" = c("X1", "X2", "X3")),
                 "RWE" = list("ME" = c("X2", "X3"),
                              "PS" = c("X1", "X2")),
                 "outcome" = list("method" = "glm",
                                  "controls" = list("family" = "gaussian")),
                 "ps" = list("method" = "glm",
                             "controls" = list("family" = "quasibinomial")),
                 "sieve.degree" = 2L,
                 "contName" = c("X2", "X3"))


  withr::with_seed(2345L, {
    for (i in 1L:10L) {
      res <- .perturbationEstIteration(data.rwe = data.rwe,
                                       data.rct = data.rct,
                                       outcome.type = "cont",
                                       models = models)
      ptb <- rbind(ptb, res$psi)
      score <- rbind(score, res$weighted.score)
    }
    colnames(ptb) <- names(res$psi)
    colnames(score) <- names(res$weighted.score)
  })

  data.rct$q <- rep(1, n)
  data.rwe$q <- rep(1, n)

  expect_equal(withr::with_seed(2345L,
                                .perturbationEst(data.rwe = data.rwe,
                                                 data.rct = data.rct,
                                                 n.pert = 10L,
                                                 outcome.type = "cont",
                                                 models = models)),
               list("ptb" = ptb, "Shat.rw.psihat.rt" = score))

})

test_that("`.perturbationEst()` returns expected results; one covariate", {

  n <- 1000L
  p <- 3L

  withr::with_seed(1234L, {
    data.rct <- list("X" = matrix(stats::rnorm(n*p), n, p,
                                  dimnames = list(NULL, c("X1", "X2", "X3"))),
                     "Y" = stats::rnorm(n, 1),
                     "A" = stats::rbinom(n, 1, 0.5),
                     "q" = rep(1, n))
  })
  data.rwe <- data.rct
  data.rct$est.ps <- TRUE

  models <- list("RCT" = list("ME" = c("X1"),
                              "PS" = c("X3")),
                 "RWE" = list("ME" = c("X2"),
                              "PS" = c("X1")),
                 "outcome" = list("method" = "glm",
                                  "controls" = list("family" = "gaussian")),
                 "ps" = list("method" = "glm",
                             "controls" = list("family" = "quasibinomial")),
                 "sieve.degree" = 2L,
                 "contName" = c("X2", "X3"))

  ptb <- NULL
  score <- NULL
  withr::with_seed(2345L, {
    for (i in 1L:10L) {
      res <- .perturbationEstIteration(data.rwe = data.rwe,
                                       data.rct = data.rct,
                                       outcome.type = "cont",
                                       models = models)
      ptb <- rbind(ptb, res$psi)
      score <- rbind(score, res$weighted.score)
    }
    colnames(ptb) <- names(res$psi)
    colnames(score) <- names(res$weighted.score)
  })

  data.rct$q <- rep(1, n)
  data.rwe$q <- rep(1, n)

  expect_equal(withr::with_seed(2345L,
                                .perturbationEst(data.rwe = data.rwe,
                                                 data.rct = data.rct,
                                                 n.pert = 10L,
                                                 outcome.type = "cont",
                                                 models = models)),
               list("ptb" = ptb, "Shat.rw.psihat.rt" = score))

})

test_that("`.perturbationEst()` returns expected results; no covariates", {

  n <- 1000L

  withr::with_seed(1234L, {
    data.rct <- list("X" = matrix(NA, n, 0L),
                     "Y" = stats::rnorm(n,1),
                     "A" = stats::rbinom(n, 1, 0.5),
                     "q" = rep(1, n))
  })
  data.rwe <- data.rct
  data.rct$est.ps <- TRUE

  models <- list("RCT" = list("ME" = NULL,
                              "PS" = NULL),
                 "RWE" = list("ME" = NULL,
                              "PS" = NULL),
                 "outcome" = list("method" = "glm",
                                  "controls" = list("family" = "gaussian")),
                 "ps" = list("method" = "glm",
                             "controls" = list("family" = "quasibinomial")),
                 "sieve.degree" = 2L,
                 "contName" = NULL)

  ptb <- NULL
  score <- NULL
  withr::with_seed(2345L, {
    for (i in 1L:10L) {
      res <- .perturbationEstIteration(data.rwe = data.rwe,
                                       data.rct = data.rct,
                                       outcome.type = "cont",
                                       models = models)
      ptb <- rbind(ptb, res$psi)
      score <- rbind(score, res$weighted.score)
    }
    colnames(ptb) <- names(res$psi)
    colnames(score) <- names(res$weighted.score)
  })

  data.rct$q <- rep(1, n)
  data.rwe$q <- rep(1, n)

  expect_equal(withr::with_seed(2345L,
                                .perturbationEst(data.rwe = data.rwe,
                                                 data.rct = data.rct,
                                                 n.pert = 10L,
                                                 outcome.type = "cont",
                                                 models = models)),
               list("ptb" = ptb, "Shat.rw.psihat.rt" = score))

})

test_that("`.computeV()` returns expected results", {

  withr::with_seed(1234L,
                   ptb <- matrix(stats::runif(60, 0, 2), 10, 6))
  colnames(ptb) <- c("eff.1", "eff.2", "rt.1", "rt.2", "other.1", "other.2")

  V_eff <- matrix(stats::var(ptb[, 1L:2L]) * 10.0, 2L, 2L)
  V_rt <- matrix(stats::var(ptb[, 3L:4L]) * 10.0, 2L, 2L)

  expected <- list("V.eff" = V_eff, "V.rt" = V_rt)
  expect_equal(.computeV(ptb = ptb, n.rwe = 10L), expected)

  withr::with_seed(2345L,
                   ptb <- matrix(runif(60), 10, 6))
  colnames(ptb) <- c("eff.1", "eff.2", "rt.1", "rt.2", "other.1", "other.2")

  expect_equal(.computeV(ptb = ptb, n.rwe = 10L), FALSE)

})

test_that("`.computeV()` returns expected results; one covariates", {

  withr::with_seed(23456L,
                   ptb <- matrix(runif(30), 10, 3))
  colnames(ptb) <- c("eff.", "rt.", "other.")

  V_eff <- matrix(stats::var(ptb[, 1L]) * 10.0, 1L, 1L)
  V_rt <- matrix(stats::var(ptb[, 2L]) * 10.0, 1L, 1L)

  expected <- list("V.eff" = V_eff, "V.rt" = V_rt)
  expect_equal(.computeV(ptb = ptb, n.rwe = 10L), expected)

  withr::with_seed(1234L,
                   ptb <- matrix(runif(30), 10, 3))
  colnames(ptb) <- c("eff.", "rt.", "other.")

  expect_equal(.computeV(ptb = ptb, n.rwe = 10L), FALSE)

})

test_that("`.perturbationProcedure()` returns expected errors", {

    expect_error(.perturbationProcedure(),
                 "`data.rct` must be a list containing X")
    expect_error(.perturbationProcedure(data.rct = matrix(1, 10L, 3L)),
                 "`data.rct` must be a list containing X")
    expect_error(.perturbationProcedure(data.rct = list(Y = 1:3L)),
                 "`data.rct` must be a list containing X")
    data.rct <- list("X" = matrix(1, 10L, 3L))

    expect_error(.perturbationProcedure(data.rct = data.rct),
                 "`data.rwe` must be a list containing X")
    expect_error(.perturbationProcedure(data.rct = data.rct,
                                        data.rwe = matrix(1, 10L, 3L)),
                 "`data.rwe` must be a list containing X")
    expect_error(.perturbationProcedure(data.rct = data.rct,
                                        data.rwe = list(Y = 1:3L)),
                 "`data.rwe` must be a list containing X")
    data.rwe <- list("X" = matrix(1, 20L, 3L))

    expect_error(.perturbationProcedure(data.rct = data.rct,
                                        data.rwe = data.rwe),
                 "`data.rct$X` must be a named numeric matrix", fixed = TRUE)
    colnames(data.rct$X) <- c("X1", "X2", "X3")

    expect_error(.perturbationProcedure(data.rct = data.rct,
                                        data.rwe = data.rwe),
                 "`data.rwe$X` must be a named numeric matrix", fixed = TRUE)
    colnames(data.rwe$X) <- c("X1", "X2", "X3")

    expect_error(.perturbationProcedure(data.rct = data.rct, data.rwe = data.rwe),
                 "`n.pert` must be provided")

    expect_error(.perturbationProcedure(data.rct = data.rct,
                                        data.rwe = data.rwe,
                                        n.pert = 100L),
                 "`outcome.type` must be provided")
    expect_error(.perturbationProcedure(data.rct = data.rct,
                                        data.rwe = data.rwe,
                                        n.pert = 100L,
                                        outcome.type = "bin"),
                 "`models` must be a list with elements 'RWE', 'RCT', 'outcome', 'ps', 'sieve.degree', and 'contName'")
})

test_that("`.perturbationProcedure()` returns expected results", {

  n <- 1000L
  p <- 3L

  withr::with_seed(2345L, {
    data.rct <- list("X" = matrix(stats::rnorm(n*p), n, p,
                                  dimnames = list(NULL, paste0("X", seq_len(p)))),
                     "Y" = stats::rnorm(n, 2),
                     "A" = stats::rbinom(n, 1, 0.5),
                     "q" = rep(1, n))
  })
  data.rwe <- data.rct
  data.rct$est.ps <- TRUE
  models <- list("RCT" = list("ME" = c("X1", "X2"),
                              "PS" = c("X1", "X2", "X3")),
                 "RWE" = list("ME" = c("X1"),
                              "PS" = c("X1", "X2")),
                 "outcome" = list("method" = "glm",
                                  "controls" = list("family" = "gaussian")),
                 "ps" = list("method" = "glm",
                             "controls" = list("family" = "quasibinomial")),
                 "sieve.degree" = 2L,
                 "contName" = c("X2", "X3"))


  pert_est <- withr::with_seed(3456L,
                               .perturbationEst(data.rwe = data.rwe,
                                                data.rct = data.rct,
                                                n.pert = 10L,
                                                outcome.type = "cont",
                                                models = models))

  Vee <- .computeV(ptb = pert_est$ptb, n.rwe = n)

  sqrt_V_eff <- expm::sqrtm(Vee$V.eff)
  ve <- apply(pert_est$ptb, 2L, stats::var) |> matrix(nrow = 3L) |> t()

  colnames(ve) <- strsplit(colnames(pert_est$ptb), ".", fixed = TRUE) |>
    lapply("[", 2L) |> unique()
  rownames(ve) <- strsplit(colnames(pert_est$ptb), ".", fixed = TRUE) |>
    lapply("[", 1L) |> unique()

  eta <- colMeans(pert_est$Shat.rw.psihat.rt) * sqrt(n)
  names(eta) <- c("(Intercept)", "X2", "X3")

  expected <- list("V.est" = ve,
                   "eta" = eta,
                   "V.rt" = Vee$V.rt,
                   "V.eff" = Vee$V.eff,
                   "sqrt.V.eff" = sqrt_V_eff)

  expect_equal(withr::with_seed(
    3456L,
    .perturbationProcedure(data.rwe = data.rwe,
                           data.rct = data.rct,
                           n.pert = 10L,
                           outcome.type = "cont",
                           models = models)),
    expected)


})

test_that("`.perturbationProcedure()` returns expected results; one covariate", {

  n <- 1000L
  p <- 3L

  withr::with_seed(2345L, {
    data.rct <- list("X" = matrix(stats::rnorm(n*p), n, p,
                                  dimnames = list(NULL, c("X1", "X2", "X3"))),
                     "Y" = stats::rnorm(n, 2),
                     "A" = stats::rbinom(n, 1, 0.5),
                     "q" = rep(1, n))
  })
  data.rwe <- data.rct
  data.rct$est.ps <- TRUE

  models <- list("RCT" = list("ME" = c("X1"),
                              "PS" = c("X3")),
                 "RWE" = list("ME" = c("X2"),
                              "PS" = c("X1")),
                 "outcome" = list("method" = "glm",
                                  "controls" = list("family" = "gaussian")),
                 "ps" = list("method" = "glm",
                             "controls" = list("family" = "quasibinomial")),
                 "sieve.degree" = 2L,
                 "contName" = c("X2"))

  pert_est <- withr::with_seed(3456L,
                               .perturbationEst(data.rwe = data.rwe,
                                                data.rct = data.rct,
                                                n.pert = 10L,
                                                outcome.type = "cont",
                                                models = models))

  Vee <- .computeV(ptb = pert_est$ptb, n.rwe = n)

  sqrt_V_eff <- expm::sqrtm(Vee$V.eff)
  ve <- apply(pert_est$ptb, 2L, stats::var) |> matrix(nrow = 2L) |> t()

  colnames(ve) <- strsplit(colnames(pert_est$ptb), ".", fixed = TRUE) |>
    lapply("[", 2L) |> unlist() |> unique()
  rownames(ve) <- strsplit(colnames(pert_est$ptb), ".", fixed = TRUE) |>
    lapply("[", 1L) |> unlist() |> unique()

  eta <- colMeans(pert_est$Shat.rw.psihat.rt) * sqrt(n)
  names(eta) <- c("(Intercept)", "X2")

  expected <- list("V.est" = ve,
                   "eta" = eta,
                   "V.rt" = Vee$V.rt,
                   "V.eff" = Vee$V.eff,
                   "sqrt.V.eff" = sqrt_V_eff)

  expect_equal(withr::with_seed(
    3456L,
    .perturbationProcedure(data.rwe = data.rwe,
                           data.rct = data.rct,
                           n.pert = 10L,
                           outcome.type = "cont",
                           models = models)),
    expected)


})

test_that("`.perturbationProcedure()` returns expected results; no covariates", {

  n <- 1000L

  withr::with_seed(2345L, {
    data.rct <- list("X" = matrix(NA, n, 0L),
                     "Y" = stats::rnorm(n, 2),
                     "A" = stats::rbinom(n, 1, 0.5),
                     "q" = rep(1, n))
  })
  data.rwe <- data.rct
  data.rct$est.ps <- TRUE

  models <- list("RCT" = list("ME" = NULL,
                              "PS" = NULL),
                 "RWE" = list("ME" = NULL,
                              "PS" = NULL),
                 "outcome" = list("method" = "glm",
                                  "controls" = list("family" = "gaussian")),
                 "ps" = list("method" = "glm",
                             "controls" = list("family" = "quasibinomial")),
                 "sieve.degree" = 2L,
                 "contName" = NULL)

  pert_est <- withr::with_seed(3456L,
                               .perturbationEst(data.rwe = data.rwe,
                                                data.rct = data.rct,
                                                n.pert = 10L,
                                                outcome.type = "cont",
                                                models = models))

  Vee <- .computeV(ptb = pert_est$ptb, n.rwe = n)

  sqrt_V_eff <- expm::sqrtm(Vee$V.eff)
  ve <- apply(pert_est$ptb, 2L, stats::var) |> matrix(nrow = 1L) |> t()

  colnames(ve) <- strsplit(colnames(pert_est$ptb), ".", fixed = TRUE) |>
    lapply("[", 2L) |> unlist() |> unique()
  rownames(ve) <- strsplit(colnames(pert_est$ptb), ".", fixed = TRUE) |>
    lapply("[", 1L) |> unlist() |> unique()

  eta <- colMeans(pert_est$Shat.rw.psihat.rt) * sqrt(n)
  names(eta) <- c("(Intercept)")

  expected <- list("V.est" = ve,
                   "eta" = eta,
                   "V.rt" = Vee$V.rt,
                   "V.eff" = Vee$V.eff,
                   "sqrt.V.eff" = sqrt_V_eff)

  expect_equal(withr::with_seed(
    3456L,
    .perturbationProcedure(data.rwe = data.rwe,
                           data.rct = data.rct,
                           n.pert = 10L,
                           outcome.type = "cont",
                           models = models)),
    expected)


  n <- 1000L
  p <- 3L

  withr::with_seed(2345L, {
    data.rct <- list("X" = matrix(stats::rnorm(n*p), n, p,
                                  dimnames = list(NULL, paste0("X", seq_len(p)))),
                     "Y" = stats::rnorm(n, 2),
                     "A" = stats::rbinom(n, 1, 0.5),
                     "q" = rep(1, n))
  })
  data.rwe <- data.rct
  data.rct$est.ps <- TRUE

  pert_est <- withr::with_seed(3456L,
                               .perturbationEst(data.rwe = data.rwe,
                                                data.rct = data.rct,
                                                n.pert = 10L,
                                                outcome.type = "cont",
                                                models = models))

  Vee <- .computeV(ptb = pert_est$ptb, n.rwe = n)

  sqrt_V_eff <- expm::sqrtm(Vee$V.eff)
  ve <- apply(pert_est$ptb, 2L, stats::var) |> matrix(nrow = 1L) |> t()

  colnames(ve) <- strsplit(colnames(pert_est$ptb), ".", fixed = TRUE) |>
    lapply("[", 2L) |> unlist() |> unique()
  rownames(ve) <- strsplit(colnames(pert_est$ptb), ".", fixed = TRUE) |>
    lapply("[", 1L) |> unlist() |> unique()

  eta <- colMeans(pert_est$Shat.rw.psihat.rt) * sqrt(n)
  names(eta) <- c("(Intercept)")

  expected <- list("V.est" = ve,
                   "eta" = eta,
                   "V.rt" = Vee$V.rt,
                   "V.eff" = Vee$V.eff,
                   "sqrt.V.eff" = sqrt_V_eff)

  expect_equal(withr::with_seed(
    3456L,
    .perturbationProcedure(data.rwe = data.rwe,
                           data.rct = data.rct,
                           n.pert = 10L,
                           outcome.type = "cont",
                           models = models)),
    expected)

})
