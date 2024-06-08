test_that("`elasticHTE()` returns expected errors", {

    expect_error(elasticHTE(),
                 "`data.rct` must be provided")
    data.rct <- list("X" = matrix(1:10, 10, 4, dimnames = list(NULL, c("X1", "X2", "X3", "X4"))),
                     "Y" = 1:10, "A" = rep(0L, 10), "mainName" = 1L, "contName" = 1L, "psName" = 1L)

    expect_error(elasticHTE(data.rct = data.rct),
                 "`data.rwe` must be provided")

    data.rwe <- data.rct
    data.rct$contName <- c("X1")
        expect_error(elasticHTE(data.rct = data.rct,
                                data.rwe = data.rwe),
                 "`data.rct$contName` must match `data.rwe$contName`",
                 fixed = TRUE)
    data.rct$contName <- 1L

    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe, ps.rct = 1L),
                 "`ps.rct` must be NULL or a numeric vector of length = nrow(data.rct$X)", fixed = TRUE)
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe, ps.rct = "1L"),
                 "`ps.rct` must be NULL or a numeric vector of length = nrow(data.rct$X)", fixed = TRUE)

    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            thres.psi = NA_real_),
                 "`thres.psi` must be a positive scalar")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            thres.psi = "a"),
                 "`thres.psi` must be a positive scalar")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            thres.psi = 0.0),
                 "`thres.psi` must be a positive scalar")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            thres.psi = -0.000001),
                 "`thres.psi` must be a positive scalar")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            thres.psi = c(0.1, 0.2)),
                 "`thres.psi` must be a positive scalar")

    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            thres.psi = 0.1,
                            sieve.degree = NULL),
                 "`sieve.degree` must be a positive integer")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            thres.psi = 0.1,
                            sieve.degree = 0.0),
                 "`sieve.degree` must be a positive integer")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            thres.psi = 0.1,
                            sieve.degree = 1.1),
                 "`sieve.degree` must be a positive integer")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            thres.psi = 0.1,
                            sieve.degree = c(1, 2)),
                 "`sieve.degree` must be a positive integer")

    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "Cont"))
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = 1L))
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "binary"))

    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.controls = "a"),
                 "`outcome.controls` must be a named list")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.controls = list("gaussian")),
                 "`outcome.controls` must be a named list")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.controls = list("family" = "gaussian", TRUE)),
                 "`outcome.controls` must be a named list")

    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.controls = list("family" = "gaussian"),
                            ps.controls = "a"),
                 "`ps.controls` must be a named list")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.controls = list("family" = "gaussian"),
                            ps.controls = list("gaussian")),
                 "`ps.controls` must be a named list")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.controls = list("family" = "gaussian"),
                            ps.controls = list("family" = "gaussian", TRUE)),
                 "`ps.controls` must be a named list")

    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.controls = list("family" = "gaussian"),
                            ps.controls = list("family" = "gaussian"),
                            fixed = 0),
                 "`fixed` must be a logical")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.controls = list("family" = "gaussian"),
                            ps.controls = list("family" = "gaussian"),
                            fixed = c(TRUE, FALSE)),
                 "`fixed` must be a logical")

    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.controls = list("family" = "gaussian"),
                            ps.controls = list("family" = "gaussian"),
                            fixed = TRUE,
                            n.pert = "a"),
                 "`n.pert` must be a positive integer")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.controls = list("family" = "gaussian"),
                            ps.controls = list("family" = "gaussian"),
                            fixed = TRUE,
                            n.pert = c(1, 2)),
                 "`n.pert` must be a positive integer")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.controls = list("family" = "gaussian"),
                            ps.controls = list("family" = "gaussian"),
                            fixed = TRUE,
                            n.pert = 0),
                 "`n.pert` must be a positive integer")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.controls = list("family" = "gaussian"),
                            ps.controls = list("family" = "gaussian"),
                            fixed = TRUE,
                            n.pert = -1L),
                 "`n.pert` must be a positive integer")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.controls = list("family" = "gaussian"),
                            ps.controls = list("family" = "gaussian"),
                            fixed = TRUE,
                            n.pert = 100.1),
                 "`n.pert` must be a positive integer")

    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.controls = list("family" = "gaussian"),
                            ps.controls = list("family" = "gaussian"),
                            fixed = TRUE,
                            n.pert = 10L,
                            n.boot = "a"),
                 "`n.boot` must be a positive integer")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.controls = list("family" = "gaussian"),
                            ps.controls = list("family" = "gaussian"),
                            fixed = TRUE,
                            n.pert = 10L,
                            n.boot = c(1, 2)),
                 "`n.boot` must be a positive integer")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.controls = list("family" = "gaussian"),
                            ps.controls = list("family" = "gaussian"),
                            fixed = TRUE,
                            n.pert = 10L,
                            n.boot = -1),
                 "`n.boot` must be a positive integer")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.controls = list("family" = "gaussian"),
                            ps.controls = list("family" = "gaussian"),
                            fixed = TRUE,
                            n.pert = 10L,
                            n.boot = -1L),
                 "`n.boot` must be a positive integer")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.controls = list("family" = "gaussian"),
                            ps.controls = list("family" = "gaussian"),
                            fixed = TRUE,
                            n.pert = 10L,
                            n.boot = 100.1),
                 "`n.boot` must be a positive integer")

    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.controls = list("family" = "gaussian"),
                            ps.controls = list("family" = "gaussian"),
                            fixed = TRUE,
                            n.pert = 10L,
                            n.boot = 10L,
                            n.gamma = "a"),
                 "`n.gamma` must be a positive integer")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.controls = list("family" = "gaussian"),
                            ps.controls = list("family" = "gaussian"),
                            fixed = TRUE,
                            n.pert = 10L,
                            n.boot = 10L,
                            n.gamma = c(1, 2)),
                 "`n.gamma` must be a positive integer")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.controls = list("family" = "gaussian"),
                            ps.controls = list("family" = "gaussian"),
                            fixed = TRUE,
                            n.pert = 10L,
                            n.boot = 10L,
                            n.gamma = 0),
                 "`n.gamma` must be a positive integer")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.controls = list("family" = "gaussian"),
                            ps.controls = list("family" = "gaussian"),
                            fixed = TRUE,
                            n.pert = 10L,
                            n.boot = 10L,
                            n.gamma = -1L),
                 "`n.gamma` must be a positive integer")
    expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe,
                            thres.psi = 0.1,
                            sieve.degree = 2L,
                            outcome.type = "cont",
                            outcome.controls = list("family" = "gaussian"),
                            ps.controls = list("family" = "gaussian"),
                            fixed = TRUE,
                            n.pert = 10L,
                            n.boot = 10L,
                            n.gamma = 100.1),
                 "`n.gamma` must be a positive integer")
})

test_that("`elasticHTE()` results in expected internal errors", {

  data.rct <- list("X" = matrix(1, 10L, 3L, dimnames = list(NULL, c("X 1", "X.1", "X3"))),
                   "Y" = 1:10, "A" = c(rep(0L, 5), rep(1L, 5)),
                   mainName = c("X 1", "X.1", "X3"), contName = 1L, psName = 1L)
  data.rwe <- list("X" = matrix(1, 100L, 3L, dimnames = list(NULL, c("X1", "X2", "X3"))),
                   "Y" = 1:100, "A" = c(rep(0L, 50), rep(1L, 50)),
                   mainName = c("X1", "X2", "X3"), contName = 1L, psName = 1L)

  expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe),
               paste("duplicate column headers found in X,",
                     "possibly due to required removal of spaces",
                     "please eliminate spaces from column header names in `data.rct$X`"), fixed = TRUE)

  data.rwe <- list("X" = matrix(1, 10L, 3L, dimnames = list(NULL, c("X 1", "X.1", "X3"))),
                   "Y" = 1:10, "A" = c(rep(0L, 5), rep(1L, 5)),
                   mainName = c("X 1", "X.1", "X3"), contName = 1L, psName = 1L)
  data.rct <- list("X" = matrix(1, 100L, 3L, dimnames = list(NULL, c("X1", "X2", "X3"))),
                   "Y" = 1:100, "A" = c(rep(0L, 50), rep(1L, 50)),
                   mainName = c("X1", "X2", "X3"), contName = 1L, psName = 1L)

  expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe),
               paste("duplicate column headers found in X,",
                     "possibly due to required removal of spaces",
                     "please eliminate spaces from column header names in `data.rwe$X`"), fixed = TRUE)

  data.rct <- list("X" = matrix(1, 10L, 3L, dimnames = list(NULL, c("X1", "X2", "X3"))),
                   "Y" = 1:10, "A" = c(rep(0L, 5), rep(1L, 5)),
                   mainName = c("X1", "X2", "X3"), contName = 1L, psName = 1L)
  data.rwe <- list("X" = matrix(1, 100L, 3L, dimnames = list(NULL, c("X1", "X2", "X3"))),
                   "Y" = 1:100, "A" = c(rep(0L, 50), rep(1L, 50)),
                   mainName = c("X1", "X2", "X3"), contName = 1L, psName = 1L)

  data.rct$X[1L, 1L] <- NA_real_
  expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe),
               "elements of `data.rct` and `data.rwe` cannot contain missing values")
  data.rct$X[1L, 1L] <- 1

  data.rct$Y[1L] <- NA_real_
  expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe),
               "elements of `data.rct` and `data.rwe` cannot contain missing values")
  data.rct$Y[1L] <- 1

  data.rct$A[1L] <- NA_real_
  expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe),
               "elements of `data.rct` and `data.rwe` cannot contain missing values")
  data.rct$A[1L] <- 0L

  data.rwe$X[1L, 1L] <- NA_real_
  expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe),
               "elements of `data.rct` and `data.rwe` cannot contain missing values")
  data.rwe$X[1L, 1L] <- 1

  data.rwe$Y[1L] <- NA_real_
  expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe),
               "elements of `data.rct` and `data.rwe` cannot contain missing values")
  data.rwe$Y[1L] <- 1

  data.rwe$A[1L] <- NA_real_
  expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe),
               "elements of `data.rct` and `data.rwe` cannot contain missing values")
  data.rwe$A[1L] <- 0L

  data.rwe$ps <- rep(1, 100)
  expect_error(tryCatch(elasticHTE(data.rct = data.rct, data.rwe = data.rwe),
                        warning = function(e) {
                          expect_equal(e$message, "`ps` cannot be provided in `data.rwe`; input ignored")
                          data.rwe$ps <- NULL
                          data.rwe$A <- c(rep(0L, 98), 1L, 2L)
                          elasticHTE(data.rct = data.rct, data.rwe = data.rwe)
                          }),
           "more than 2 treatments found in data.rct$A and data.rwe$A", fixed = TRUE)
  data.rwe$A <- c(rep(0L, 50), rep(1L, 50))
  data.rwe$ps <- NULL

  data.rct$A[1L] <- 2L
  expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe),
               "more than 2 treatments found in data.rct$A and data.rwe$A", fixed = TRUE)
  data.rct$A[1L] <- 0L

  expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe, outcome.type = "bin"),
               "`data.rct$Y` is not binary",
               fixed = TRUE)

  data.rct$Y <- c(rep(0L, 5), rep(1L,  5))
  expect_error(elasticHTE(data.rct = data.rct, data.rwe = data.rwe, outcome.type = "bin"),
               "`data.rwe$Y` is not binary",
              fixed = TRUE)

  data.rwe$Y <- c(rep(0L, 50), rep(1L,  50))
  tryCatch(elasticHTE(data.rct = data.rct, data.rwe = data.rwe, outcome.type = "cont"),
           message = function(m) {
             expect_equal(m$message,
                          paste0("* * * * * * * * * * WARNING * * * * * * * * * *\n",
                                 "`outcome.type` = 'cont'; however, response provided in data.rct$Y has only 2 unique values\n"),
                          fixed = TRUE)
             NULL
           })

})


test_that("`.elasticHTE()` returns expected results", {

  n <- 1000L
  m <- n * 10L

  withr::with_seed(1234L, {
    data.rct <- list()
    data.rct$X <- matrix(stats::rnorm(n * 3), n, 3,
                         dimnames = list(NULL, c("X1", "X2", "X3")))
    data.rct$Y <- stats::rnorm(n, 1)
    data.rct$A <- stats::rbinom(n, 1, 0.4)

    data.rwe <- list()
    data.rwe$X <- matrix(stats::rnorm(m * 3), m, 3,
                         dimnames = list(NULL, c("X1", "X2", "X3")))
    data.rwe$Y <- stats::rnorm(m, 1.5)
    data.rwe$A <- stats::rbinom(m, 1, 0.4)
  })

  withr::with_seed(2345L, {

    data.rct$est.ps <- TRUE
    data.rct$q <- rep(1.0, n)
    data.rwe$q <- rep(1.0, m)
    n_rct <- n
    n_rwe <- m
    thres.psi <- sqrt(log(m))

    models <- list("RCT" = list("ME" = c("X1", "X2", "X3"), "PS" = c("X1", "X2", "X3")),
                   "RWE" = list("ME" = c("X1", "X2", "X3"), "PS" =  c("X1", "X2", "X3")),
                   sieve.degree = 2L,
                   outcome = list("method" = "glm", controls = list("family" = "gaussian")),
                   ps = list("method" = "glm", controls = list("family" = "quasibinomial")),
                   contName = c("X1", "X2", "X3"))
    psi_list <- .psiEst(data.rwe = data.rwe,
                        data.rct = data.rct,
                        outcome.type = "cont",
                        models = models)

    perm_result <- .perturbationProcedure(data.rwe = data.rwe,
                                          data.rct = data.rct,
                                          n.pert = 10L,
                                          outcome.type = "cont",
                                          models = models)

    Sigma_SS_matrices <- .calculateSigmaSSMatrices(V.rt = perm_result$V.rt,
                                                   V.eff = perm_result$V.eff,
                                                   rho = n_rct / n_rwe)

    sqrt_V_rt_eff <- perm_result$V.eff %*% Sigma_SS_matrices$sqrt.Sigma.SS

    Tstat <- .calculateTstat(eta = perm_result$eta,
                             inv.Sigma.SS = Sigma_SS_matrices$inv.Sigma.SS)

    mu1 <- {Sigma_SS_matrices$sqrt.inv.Sigma.SS %*% perm_result$eta} |> drop()
    mu2 <- {perm_result$sqrt.V.eff %*% perm_result$eta} |> drop()

    nuispar <- .cGamma(mu1, mu2,
                       n.gamma = 10L,
                       sqrt.V.rt_eff = sqrt_V_rt_eff,
                       sqrt.V.eff = perm_result$sqrt.V.eff,
                       Tstat1 = Tstat,
                       n.rwe = n_rwe,
                       fixed = FALSE)
    nuispar$eta <- perm_result$eta

    est_bias <- .bias(Icomb = nuispar$Icomb,
                      psi.rt = psi_list$psi["rt", ],
                      psi.eff = psi_list$psi["eff", ],
                      mu1 = mu1,
                      gamma = nuispar$gamma,
                      eta = nuispar$eta,
                      V.eff = perm_result$V.eff,
                      n.rwe = n_rwe)

    cis <- .bootFunc(mu1 = mu1,
                     mu2 = mu2,
                     c.gamma = nuispar$c.gamma,
                     V.rt = perm_result$V.rt,
                     sqrt.V.rt_eff = sqrt_V_rt_eff,
                     sqrt.V.eff = perm_result$sqrt.V.eff,
                     psi = psi_list$psi, ve = perm_result$V.est,
                     psi.elastic = est_bias,
                     n.rwe = n_rwe, n.boot = 10L,
                     thres.psi = thres.psi,
                     Tstat = Tstat)

    psi <- rbind(psi_list$psi,
                 "elastic" = est_bias)
    ve <- rbind(perm_result$V.est,
                "elastic" = nuispar$V.elastic)
    nuispar$V.elastic <- NULL
    nuispar$eta <- {perm_result$eta %*% Sigma_SS_matrices$sqrt.inv.Sigma.SS} |> drop()

    obj <- c(list("call" = NA,
                  "psi" = psi, "ve" = ve,
                  "nuispar" = nuispar, "Tstat" = Tstat), cis)
    class(obj) <- c("elasticHTE", class(obj))

  })

  data.rct$mainName <- c("X1", "X2", "X3")
  data.rct$contName <- c("X1", "X2", "X3")
  data.rct$psName <- c("X1", "X2", "X3")

  data.rwe$mainName <- c("X1", "X2", "X3")
  data.rwe$contName <- c("X1", "X2", "X3")
  data.rwe$psName <- c("X1", "X2", "X3")

  test_object <- withr::with_seed(2345L, elasticHTE(data.rct, data.rwe,
                                                    n.pert = 10, n.gamma = 10, n.boot = 10))
  test_object$call <- NA

  expect_equal(test_object, obj)

})


test_that("`.elasticHTE()` returns expected results; one covariate", {

  n <- 1000L
  m <- n * 10L

  withr::with_seed(1234L, {
    data.rct <- list()
    data.rct$X <- matrix(stats::rnorm(n*3), n, 3,
                         dimnames = list(NULL, c("X1", "X2", "X3")))
    data.rct$Y <- stats::rnorm(n, 1)
    data.rct$A <- stats::rbinom(n, 1, 0.4)

    data.rwe <- list()
    data.rwe$X <- matrix(stats::rnorm(m * 3), m, 3,
                         dimnames = list(NULL, c("X1", "X2", "X3")))
    data.rwe$Y <- stats::rnorm(m, 1.5)
    data.rwe$A <- stats::rbinom(m, 1, 0.4)
  })

  models <- list("RCT" = list("ME" = c("X1"), "PS" = c("X3")),
                 "RWE" = list("ME" = c("X1"), "PS" = c("X3")),
                 sieve.degree = 2L,
                 outcome = list("method" = "glm", controls = list("family" = "gaussian")),
                 ps = list("method" = "glm", controls = list("family" = "quasibinomial")),
                 contName = c("X2"))

  withr::with_seed(2345L, {

    data.rct$est.ps <- TRUE
    data.rct$q <- rep(1.0, n)
    data.rwe$q <- rep(1.0, m)
    n_rct <- n
    n_rwe <- m
    thres.psi <- sqrt(log(m))
    psi_list <- .psiEst(data.rwe = data.rwe,
                        data.rct = data.rct,
                        outcome.type = "cont",
                        models = models)

    perm_result <- .perturbationProcedure(data.rwe = data.rwe,
                                          data.rct = data.rct,
                                          n.pert = 10L,
                                          outcome.type = "cont",
                                          models = models)

    Sigma_SS_matrices <- .calculateSigmaSSMatrices(V.rt = perm_result$V.rt,
                                                   V.eff = perm_result$V.eff,
                                                   rho = n_rct / n_rwe)

    sqrt_V_rt_eff <- perm_result$V.eff %*% Sigma_SS_matrices$sqrt.Sigma.SS

    Tstat <- .calculateTstat(eta = perm_result$eta,
                             inv.Sigma.SS = Sigma_SS_matrices$inv.Sigma.SS)

    mu1 <- {Sigma_SS_matrices$sqrt.inv.Sigma.SS %*% perm_result$eta} |> drop()
    mu2 <- {perm_result$sqrt.V.eff %*% perm_result$eta} |> drop()

    nuispar <- .cGamma(mu1, mu2,
                       n.gamma = 10L,
                       sqrt.V.rt_eff = sqrt_V_rt_eff,
                       sqrt.V.eff = perm_result$sqrt.V.eff,
                       Tstat1 = Tstat,
                       n.rwe = n_rwe,
                       fixed = FALSE)
    nuispar$eta <- perm_result$eta

    est_bias <- .bias(Icomb = nuispar$Icomb,
                      psi.rt = psi_list$psi["rt", ],
                      psi.eff = psi_list$psi["eff", ],
                      mu1 = mu1,
                      gamma = nuispar$gamma,
                      eta = nuispar$eta,
                      V.eff = perm_result$V.eff,
                      n.rwe = n_rwe)

    cis <- .bootFunc(mu1 = mu1,
                     mu2 = mu2,
                     c.gamma = nuispar$c.gamma,
                     V.rt = perm_result$V.rt,
                     sqrt.V.rt_eff = sqrt_V_rt_eff,
                     sqrt.V.eff = perm_result$sqrt.V.eff,
                     psi = psi_list$psi, ve = perm_result$V.est,
                     psi.elastic = est_bias,
                     n.rwe = n_rwe, n.boot = 10L,
                     thres.psi = thres.psi,
                     Tstat = Tstat)

    psi <- rbind(psi_list$psi,
                 "elastic" = est_bias)
    ve <- rbind(perm_result$V.est,
                "elastic" = nuispar$V.elastic)
    nuispar$V.elastic <- NULL
    nuispar$eta <- {perm_result$eta %*% Sigma_SS_matrices$sqrt.inv.Sigma.SS} |> drop()

    obj <- c(list("call" = NA,
                  "psi" = psi, "ve" = ve,
                  "nuispar" = nuispar, "Tstat" = Tstat), cis)
    class(obj) <- c("elasticHTE", class(obj))

  })

  data.rct$mainName <- "X1"
  data.rwe$mainName <- "X1"
  data.rct$contName <- "X2"
  data.rwe$contName <- "X2"
  data.rct$psName <- "X3"
  data.rwe$psName <- "X3"

  test_object <- withr::with_seed(2345L,
                                  elasticHTE(data.rct, data.rwe,
                                             n.pert = 10, n.gamma = 10, n.boot = 10))
  test_object$call <- NA

  expect_equal(test_object, obj)

})


test_that("`.elasticHTE()` returns expected results; no covariate", {

  n <- 1000L
  m <- 10L * n

  withr::with_seed(1234L, {
    data.rct <- list()
    data.rct$X <- matrix(stats::rnorm(n*3), n, 3,
                         dimnames = list(NULL, c("X1", "X2", "X3")))
    data.rct$Y <- stats::rnorm(n, 1)
    data.rct$A <- stats::rbinom(n, 1, 0.4)

    data.rwe <- list()
    data.rwe$X <- matrix(stats::rnorm(m * 3), n*10, 3,
                         dimnames = list(NULL, c("X1", "X2", "X3")))
    data.rwe$Y <- stats::rnorm(m, 1.5)
    data.rwe$A <- stats::rbinom(m, 1, 0.4)
  })

  withr::with_seed(2345L, {

    data.rct$est.ps <- TRUE
    data.rct$q <- rep(1.0, n)
    data.rwe$q <- rep(1.0, m)
    n_rct <- n
    n_rwe <- m
    thres.psi <- sqrt(log(m))
    models <- list("RCT" = list("ME" = NULL, "PS" = NULL),
                   "RWE" = list("ME" = NULL, "PS" = NULL),
                   sieve.degree = 2L,
                   outcome = list("method" = "glm", controls = list("family" = "gaussian")),
                   ps = list("method" = "glm", controls = list("family" = "quasibinomial")),

                   contName = NULL)
    psi_list <- .psiEst(data.rwe = data.rwe,
                        data.rct = data.rct,
                        outcome.type = "cont",
                        models = models)

    perm_result <- .perturbationProcedure(data.rwe = data.rwe,
                                          data.rct = data.rct,
                                          n.pert = 10L,
                                          outcome.type = "cont",
                                          models = models)

    Sigma_SS_matrices <- .calculateSigmaSSMatrices(V.rt = perm_result$V.rt,
                                                   V.eff = perm_result$V.eff,
                                                   rho = n_rct / n_rwe)

    sqrt_V_rt_eff <- perm_result$V.eff %*% Sigma_SS_matrices$sqrt.Sigma.SS

    Tstat <- .calculateTstat(eta = perm_result$eta,
                             inv.Sigma.SS = Sigma_SS_matrices$inv.Sigma.SS)

    mu1 <- {Sigma_SS_matrices$sqrt.inv.Sigma.SS %*% perm_result$eta} |> drop()
    mu2 <- {perm_result$sqrt.V.eff %*% perm_result$eta} |> drop()

    nuispar <- .cGamma(mu1, mu2,
                       n.gamma = 10L,
                       sqrt.V.rt_eff = sqrt_V_rt_eff,
                       sqrt.V.eff = perm_result$sqrt.V.eff,
                       Tstat1 = Tstat,
                       n.rwe = n_rwe,
                       fixed = FALSE)
    nuispar$eta <- perm_result$eta

    est_bias <- .bias(Icomb = nuispar$Icomb,
                      psi.rt = psi_list$psi["rt", ],
                      psi.eff = psi_list$psi["eff", ],
                      mu1 = mu1,
                      gamma = nuispar$gamma,
                      eta = nuispar$eta,
                      V.eff = perm_result$V.eff,
                      n.rwe = n_rwe)

    cis <- .bootFunc(mu1 = mu1,
                     mu2 = mu2,
                     c.gamma = nuispar$c.gamma,
                     V.rt = perm_result$V.rt,
                     sqrt.V.rt_eff = sqrt_V_rt_eff,
                     sqrt.V.eff = perm_result$sqrt.V.eff,
                     psi = psi_list$psi, ve = perm_result$V.est,
                     psi.elastic = est_bias,
                     n.rwe = n_rwe, n.boot = 10L,
                     thres.psi = thres.psi,
                     Tstat = Tstat)

    psi <- rbind(psi_list$psi,
                 "elastic" = est_bias)
    ve <- rbind(perm_result$V.est,
                "elastic" = nuispar$V.elastic)
    nuispar$V.elastic <- NULL
    nuispar$eta <- {perm_result$eta %*% Sigma_SS_matrices$sqrt.inv.Sigma.SS} |> drop()

    obj <- c(list("call" = NA,
                  "psi" = psi, "ve" = ve,
                  "nuispar" = nuispar, "Tstat" = Tstat), cis)
    class(obj) <- c("elasticHTE", class(obj))

  })

  data.rct$mainName <- 1
  data.rwe$mainName <- 1
  data.rct$contName <- 1
  data.rwe$contName <- 1
  data.rct$psName <- 1
  data.rwe$psName <- 1

  test_object <- withr::with_seed(2345L,
                                  elasticHTE(data.rct, data.rwe,
                                             n.pert = 10, n.gamma = 10, n.boot = 10))
  test_object$call <- NA

  expect_equal(test_object, obj)

  data.rct$X <- matrix(NA, n, 0L)
  data.rwe$X <- matrix(NA, m, 0L)

  test_object <- withr::with_seed(2345L,
                                  elasticHTE(data.rct, data.rwe,
                                             mainName.rct = 1,
                                             contName = 1,
                                             psName.rct = 1,
                                             n.pert = 10, n.gamma = 10, n.boot = 10))
  test_object$call <- NA

  expect_equal(test_object, obj)


})
