test_that("`.chooseGamma()` returns expected errors", {

    expect_error(.chooseGamma(), "`z1tz1` must be a numeric vector")
    expect_error(.chooseGamma(z1tz1 = "a"), "`z1tz1` must be a numeric vector")
    expect_error(.chooseGamma(z1tz1 = matrix(1,1,1)),
                 "`z1tz1` must be a numeric vector")

    expect_error(.chooseGamma(z1tz1 = 1:3),
                 "`n.rt` must be a numeric vector")
    expect_error(.chooseGamma(z1tz1 = 1:3, n.rt = "a"),
                 "`n.rt` must be a numeric vector")
    expect_error(.chooseGamma(z1tz1 = 1:3, n.rt = matrix(1, 1, 3)),
                 "`n.rt` must be a numeric vector")
    expect_error(.chooseGamma(z1tz1 = 1:3, n.rt = matrix(1, 3, 1)),
                 "`n.rt` must be a numeric vector")
    expect_error(.chooseGamma(z1tz1 = 1:3, n.rt = 1:4),
                 "`n.rt` must be a numeric vector")

    expect_error(.chooseGamma(z1tz1 = 1:3, n.rt = 1:3),
                 "`n.eff` must be a numeric vector")
    expect_error(.chooseGamma(z1tz1 = 1:3, n.rt = 1:3, n.eff = "a"),
                 "`n.eff` must be a numeric vector")
    expect_error(.chooseGamma(z1tz1 = 1:3, n.rt = 1:3, n.eff = matrix(1, 1, 3)),
                 "`n.eff` must be a numeric vector")
    expect_error(.chooseGamma(z1tz1 = 1:3, n.rt = 1:3, n.eff = matrix(1, 3, 1)),
                 "`n.eff` must be a numeric vector")
    expect_error(.chooseGamma(z1tz1 = 1:3, n.rt = 1:3, n.eff = 1:4),
                 "`n.eff` must be a numeric vector")

    expect_error(.chooseGamma(z1tz1 = 1:3, n.rt = 1:3, n.eff = 1:3),
                 "`n.cov` must be a positive integer")
    expect_error(.chooseGamma(z1tz1 = 1:3, n.rt = 1:3, n.eff = 1:3, n.cov = "a"),
                 "`n.cov` must be a positive integer")
    expect_error(.chooseGamma(z1tz1 = 1:3, n.rt = 1:3, n.eff = 1:3, n.cov = 1.5),
                 "`n.cov` must be a positive integer")
    expect_error(.chooseGamma(z1tz1 = 1:3, n.rt = 1:3, n.eff = 1:3, n.cov = matrix(1L, 1L, 1L)),
                 "`n.cov` must be a positive integer")
    expect_error(.chooseGamma(z1tz1 = 1:3, n.rt = 1:3, n.eff = 1:3, n.cov = 0L),
                 "`n.cov` must be a positive integer")
    expect_error(.chooseGamma(z1tz1 = 1:3, n.rt = 1:3, n.eff = 1:3, n.cov = 1L:2L),
                 "`n.cov` must be a positive integer")
})

test_that("`.chooseGamma()` returns expected results", {

  allgamma <- seq(1.0 - 1.0e-10 , 1.0e-10, length.out = 50L)
  critmse <- rep(NA_real_, 50L)
  qchi <- stats::qchisq(1.0 - allgamma, df = 3L)

  z1tz1 <- withr::with_seed(1234L, runif(10))
  n_rt <- withr::with_seed(2345L, runif(10))
  n_eff <- withr::with_seed(3456L, runif(10))
  mat_yes <- outer(z1tz1, qchi, "<")
  gen_value <- n_rt * (1.0 - mat_yes) + n_eff * mat_yes
  critmse <- colMeans(gen_value)
  chosen <- which.min(critmse)
  expected <- list("gamma" = allgamma[chosen],
                   "c_gamma" = stats::qchisq(1.0 - allgamma[chosen], df = 3L))

  expect_equal(.chooseGamma(z1tz1, n_rt, n_eff, 3L), expected)
})

test_that("`.veElastic()` returns expected errors", {

  expect_error(.veElastic(), "`z1tz1` must be a numeric vector")
  expect_error(.veElastic(z1tz1 = "a"), "`z1tz1` must be a numeric vector")
  expect_error(.veElastic(z1tz1 = matrix(1, 1, 1)),
               "`z1tz1` must be a numeric vector")

  expect_error(.veElastic(z1tz1 = 1:3), "`c.gamma` must be a scalar numeric")
  expect_error(.veElastic(z1tz1 = 1:3, c.gamma = "a"), "`c.gamma` must be a scalar numeric")
  expect_error(.veElastic(z1tz1 = 1:3, c.gamma = matrix(1, 1, 1)),
               "`c.gamma` must be a scalar numeric")
  expect_error(.veElastic(z1tz1 = 1:3, c.gamma = c(1, 2)),
               "`c.gamma` must be a scalar numeric")

  expect_error(.veElastic(z1tz1 = 1:3, c.gamma = 0.05),
               "`n.rt` must be a numeric matrix")
  expect_error(.veElastic(z1tz1 = 1:3, c.gamma = 0.05, n.rt = "a"),
               "`n.rt` must be a numeric matrix")
  expect_error(.veElastic(z1tz1 = 1:3, c.gamma = 0.05, n.rt = 1:3),
               "`n.rt` must be a numeric matrix")
  expect_error(.veElastic(z1tz1 = 1:3, c.gamma = 0.05, n.rt = matrix(1, 1, 3)),
               "`n.rt` must be a numeric matrix")

  expect_error(.veElastic(z1tz1 = 1:3, c.gamma = 0.05, n.rt = matrix(1, 3, 4)),
               "`n.eff` must be a numeric matrix")
  expect_error(.veElastic(z1tz1 = 1:3, c.gamma = 0.05, n.rt = matrix(1, 3, 4),
                          n.eff = "a"),
               "`n.eff` must be a numeric matrix")
  expect_error(.veElastic(z1tz1 = 1:3, c.gamma = 0.05, n.rt = matrix(1, 3, 4),
                          n.eff = 1:3),
               "`n.eff` must be a numeric matrix")
  expect_error(.veElastic(z1tz1 = 1:3, c.gamma = 0.05, n.rt = matrix(1, 3, 4),
                          n.eff = matrix(1, 1, 3)),
               "`n.eff` must be a numeric matrix")
  expect_error(.veElastic(z1tz1 = 1:3, c.gamma = 0.05, n.rt = matrix(1, 3, 4),
                          n.eff = matrix(1, 3, 5)),
               "`n.eff` must be a numeric matrix")

  expect_error(.veElastic(z1tz1 = 1:3, c.gamma = 0.05, n.rt = matrix(1, 3, 4),
                          n.eff = matrix(1, 3, 4)),
               "`n.rwe` must be a positive integer")
  expect_error(.veElastic(z1tz1 = 1:3, c.gamma = 0.05, n.rt = matrix(1, 3, 4),
                          n.eff = matrix(1, 3, 4), n.rwe = "a"),
               "`n.rwe` must be a positive integer")
  expect_error(.veElastic(z1tz1 = 1:3, c.gamma = 0.05, n.rt = matrix(1, 3, 4),
                          n.eff = matrix(1, 3, 4), n.rwe = 1.5),
               "`n.rwe` must be a positive integer")
  expect_error(.veElastic(z1tz1 = 1:3, c.gamma = 0.05, n.rt = matrix(1, 3, 4),
                          n.eff = matrix(1, 3, 4), n.rwe = matrix(1L, 1L, 1L)),
               "`n.rwe` must be a positive integer")
  expect_error(.veElastic(z1tz1 = 1:3, c.gamma = 0.05, n.rt = matrix(1, 3, 4),
                          n.eff = matrix(1, 3, 4), n.rwe = 0L),
               "`n.rwe` must be a positive integer")
  expect_error(.veElastic(z1tz1 = 1:3, c.gamma = 0.05, n.rt = matrix(1, 3, 4),
                          n.eff = matrix(1, 3, 4), n.rwe = 1L:2L),
               "`n.rwe` must be a positive integer")
})

test_that("`.veElastic()` returns expected results", {
  z1tz1 <- withr::with_seed(1234L, runif(10))
  n_rt <- matrix(withr::with_seed(2345L, runif(30)), 10, 3)
  n_eff <- matrix(withr::with_seed(3456L, runif(30)), 10, 3)

  c_gamma <- 0.05

  vec_yes <- as.numeric(z1tz1 < c_gamma)

  gen_value <- {n_eff * vec_yes  + n_rt * (1.0 - vec_yes)}
  expected <- numeric(3)
  for(i in 1L:3) {
    expected[i] <- stats::var(gen_value[ ,i]) / 10
  }

  expect_equal(.veElastic(z1tz1, c_gamma, n_rt, n_eff, 10L), expected)

  z1tz1 <- withr::with_seed(1234L, runif(10))
  n_rt <- matrix(withr::with_seed(2345L, runif(10)), 10, 1)
  n_eff <- matrix(withr::with_seed(3456L, runif(10)), 10, 1)

  c_gamma <- 0.05

  vec_yes <- as.numeric(z1tz1 < c_gamma)

  gen_value <- {n_eff * vec_yes  + n_rt * (1.0 - vec_yes)}
  expected <- {stats::var(gen_value) / 10} |> drop()

  expect_equal(.veElastic(z1tz1, c_gamma, n_rt, n_eff, 10L), expected)
})


test_that("`.cGamma()` returns expected errors", {

  expect_error(.cGamma(), "`mu1` must be a numeric vector")
  expect_error(.cGamma(mu1 = "a"), "`mu1` must be a numeric vector")
  expect_error(.cGamma(mu1 = matrix(1, 1, 1)), "`mu1` must be a numeric vector")

  expect_error(.cGamma(mu1 = 1:3), "`mu2` must be a numeric vector")
  expect_error(.cGamma(mu1 = 1:3, mu2 = "a"), "`mu2` must be a numeric vector")
  expect_error(.cGamma(mu1 = 1:3, mu2 = matrix(1, 1, 1)), "`mu2` must be a numeric vector")

  expect_error(.cGamma(mu1 = 1:3, mu2 = 1:4), "`mu1` and `mu2` must be of equivalent length")

  expect_error(.cGamma(mu1 = 1:3, mu2 = 1:3), "`n.gamma` must be a positive integer")
  expect_error(.cGamma(mu1 = 1:3, mu2 = 1:3, n.gamma = "a"), "`n.gamma` must be a positive integer")
  expect_error(.cGamma(mu1 = 1:3, mu2 = 1:3, n.gamma = 1.5), "`n.gamma` must be a positive integer")
  expect_error(.cGamma(mu1 = 1:3, mu2 = 1:3, n.gamma = 1:2), "`n.gamma` must be a positive integer")
  expect_error(.cGamma(mu1 = 1:3, mu2 = 1:3, n.gamma = 0), "`n.gamma` must be a positive integer")

  expect_error(.cGamma(mu1 = 1:3, mu2 = 1:3, n.gamma = 10),
               "`sqrt.V.rt_eff` must be a numeric matrix")
  expect_error(.cGamma(mu1 = 1:3, mu2 = 1:3, n.gamma = 10,
                                sqrt.V.rt_eff = "a"),
               "`sqrt.V.rt_eff` must be a numeric matrix")
  expect_error(.cGamma(mu1 = 1:3, mu2 = 1:3, n.gamma = 10,
                                sqrt.V.rt_eff = numeric(9)),
               "`sqrt.V.rt_eff` must be a numeric matrix")
  expect_error(.cGamma(mu1 = 1:3, mu2 = 1:3, n.gamma = 10,
                                sqrt.V.rt_eff = matrix(1, 4, 3)),
               "`sqrt.V.rt_eff` must be a numeric matrix")
  expect_error(.cGamma(mu1 = 1:3, mu2 = 1:3, n.gamma = 10,
                                sqrt.V.rt_eff = matrix(1, 3, 4)),
               "`sqrt.V.rt_eff` must be a numeric matrix")
  expect_error(.cGamma(mu1 = 1:3, mu2 = 1:3, n.gamma = 10,
                                sqrt.V.rt_eff = matrix("a", 3, 3)),
               "`sqrt.V.rt_eff` must be a numeric matrix")

  expect_error(.cGamma(mu1 = 1:3, mu2 = 1:3, n.gamma = 10,
                       sqrt.V.rt_eff = matrix(1, 3, 3)),
               "`sqrt.V.eff` must be a numeric matrix")
  expect_error(.cGamma(mu1 = 1:3, mu2 = 1:3, n.gamma = 10,
                       sqrt.V.rt_eff = matrix(1, 3, 3),
                       sqrt.V.eff = "a"),
               "`sqrt.V.eff` must be a numeric matrix")
  expect_error(.cGamma(mu1 = 1:3, mu2 = 1:3, n.gamma = 10,
                       sqrt.V.rt_eff = matrix(1, 3, 3),
                       sqrt.V.eff = numeric(9)),
               "`sqrt.V.eff` must be a numeric matrix")
  expect_error(.cGamma(mu1 = 1:3, mu2 = 1:3, n.gamma = 10,
                       sqrt.V.rt_eff = matrix(1, 3, 3),
                       sqrt.V.eff = matrix(1, 4, 3)),
               "`sqrt.V.eff` must be a numeric matrix")
  expect_error(.cGamma(mu1 = 1:3, mu2 = 1:3, n.gamma = 10,
                       sqrt.V.rt_eff = matrix(1, 3, 3),
                       sqrt.V.eff = matrix(1, 3, 4)),
               "`sqrt.V.eff` must be a numeric matrix")
  expect_error(.cGamma(mu1 = 1:3, mu2 = 1:3, n.gamma = 10,
                       sqrt.V.rt_eff = matrix(1, 3, 3),
                       sqrt.V.eff = matrix("a", 3, 3)),
               "`sqrt.V.eff` must be a numeric matrix")

  expect_error(.cGamma(mu1 = 1:3, mu2 = 1:3, n.gamma = 10,
                       sqrt.V.rt_eff = matrix(1, 3, 3),
                       sqrt.V.eff = matrix(1, 3, 3)),
               "`Tstat1` must be a scalar numeric")
  expect_error(.cGamma(mu1 = 1:3, mu2 = 1:3, n.gamma = 10,
                       sqrt.V.rt_eff = matrix(1, 3, 3),
                       sqrt.V.eff = matrix(1, 3, 3),
                       Tstat1 = "a"),
               "`Tstat1` must be a scalar numeric")
  expect_error(.cGamma(mu1 = 1:3, mu2 = 1:3, n.gamma = 10,
                       sqrt.V.rt_eff = matrix(1, 3, 3),
                       sqrt.V.eff = matrix(1, 3, 3),
                       Tstat1 = matrix(1,1,1)),
               "`Tstat1` must be a scalar numeric")
  expect_error(.cGamma(mu1 = 1:3, mu2 = 1:3, n.gamma = 10,
                       sqrt.V.rt_eff = matrix(1, 3, 3),
                       sqrt.V.eff = matrix(1, 3, 3),
                       Tstat1 = 1:2),
               "`Tstat1` must be a scalar numeric")


  expect_error(.cGamma(mu1 = 1:3, mu2 = 1:3, n.gamma = 10,
                       sqrt.V.rt_eff = matrix(1, 3, 3),
                       sqrt.V.eff = matrix(1, 3, 3),
                       Tstat1 = 1.5),
               "`n.rwe` must be a positive scalar")
  expect_error(.cGamma(mu1 = 1:3, mu2 = 1:3, n.gamma = 10,
                       sqrt.V.rt_eff = matrix(1, 3, 3),
                       sqrt.V.eff = matrix(1, 3, 3),
                       Tstat1 = 1.5, n.rwe = "a"),
               "`n.rwe` must be a positive scalar")
  expect_error(.cGamma(mu1 = 1:3, mu2 = 1:3, n.gamma = 10,
                       sqrt.V.rt_eff = matrix(1, 3, 3),
                       sqrt.V.eff = matrix(1, 3, 3),
                       Tstat1 = 1.5, n.rwe = 1.5),
               "`n.rwe` must be a positive scalar")
  expect_error(.cGamma(mu1 = 1:3, mu2 = 1:3, n.gamma = 10,
                       sqrt.V.rt_eff = matrix(1, 3, 3),
                       sqrt.V.eff = matrix(1, 3, 3),
                       Tstat1 = 1.5, n.rwe = 1:2),
               "`n.rwe` must be a positive scalar")
  expect_error(.cGamma(mu1 = 1:3, mu2 = 1:3, n.gamma = 10,
                       sqrt.V.rt_eff = matrix(1, 3, 3),
                       sqrt.V.eff = matrix(1, 3, 3),
                       Tstat1 = 1.5, n.rwe = matrix(1, 1, 1)),
               "`n.rwe` must be a positive scalar")
  expect_error(.cGamma(mu1 = 1:3, mu2 = 1:3, n.gamma = 10,
                       sqrt.V.rt_eff = matrix(1, 3, 3),
                       sqrt.V.eff = matrix(1, 3, 3),
                       Tstat1 = 1.5, n.rwe = 0),
               "`n.rwe` must be a positive scalar")

  expect_error(.cGamma(mu1 = 1:3, mu2 = 1:3, n.gamma = 10,
                       sqrt.V.rt_eff = matrix(1, 3, 3),
                       sqrt.V.eff = matrix(1, 3, 3),
                       Tstat1 = 1.5, n.rwe = 10),
               "`fixed` must be a logical")
  expect_error(.cGamma(mu1 = 1:3, mu2 = 1:3, n.gamma = 10,
                       sqrt.V.rt_eff = matrix(1, 3, 3),
                       sqrt.V.eff = matrix(1, 3, 3),
                       Tstat1 = 1.5, n.rwe = 10, fixed = 1),
               "`fixed` must be a logical")
  expect_error(.cGamma(mu1 = 1:3, mu2 = 1:3, n.gamma = 10,
                       sqrt.V.rt_eff = matrix(1, 3, 3),
                       sqrt.V.eff = matrix(1, 3, 3),
                       Tstat1 = 1.5, n.rwe = 10, fixed = "a"),
               "`fixed` must be a logical")
  expect_error(.cGamma(mu1 = 1:3, mu2 = 1:3, n.gamma = 10,
                       sqrt.V.rt_eff = matrix(1, 3, 3),
                       sqrt.V.eff = matrix(1, 3, 3),
                       Tstat1 = 1.5, n.rwe = 10, fixed = c(TRUE, TRUE)),
               "`fixed` must be a logical")
})

test_that("`.cGamma()` returns expected results", {

  ## df = 3; fixed = TRUE

  mu1 <- withr::with_seed(1234L, runif(3))
  mu2 <- withr::with_seed(2345L, runif(3))
  sqrt.V.eff <- matrix(1, 3, 3)
  sqrt.V.rt_eff <- matrix(0.5, 3, 3)
  Tstat1 <- 0.4

  withr::with_seed(1234L, {
    z1_samples <- mvtnorm::rmvnorm(5L, mean = mu1, diag(3))
    z2_samples <- mvtnorm::rmvnorm(5L, mean = mu2, diag(3))

    n_eff_samples <- -matrix(rowSums(z2_samples), 5, 3, byrow = TRUE)
    n_rt_samples <- matrix(rowSums(z1_samples) / 2.0, 5, 3, byrow = TRUE) + n_eff_samples

    z1tz1 <- numeric(5)
    for (i in 1L:5) {
      z1tz1[i] <- sum(z1_samples[i,]^2)
    }

    gamma_selected <- list("gamma" = 0.05,
                           "c_gamma" = stats::qchisq(0.95, df = 3))

    Icomb <- as.integer(gamma_selected$c_gamma  > Tstat1)
    Icomb_pval <- 1.0 - stats::pchisq(Tstat1, df = 3)

    ve_elas <- .veElastic(z1tz1 = z1tz1,
                          c.gamma = gamma_selected$c_gamma,
                          n.rt = n_rt_samples,
                          n.eff = n_eff_samples,
                          n.rwe = 100L)

    expected <- list("gamma" = gamma_selected$gamma,
                     "c.gamma" = gamma_selected$c_gamma,
                     "Icomb" = Icomb,
                     "Icomb.pval" = Icomb_pval,
                     "V.elastic" = ve_elas,
                     "settings" = list("n.gamma" = 5L,
                                       "fixed" = TRUE))
  })

  expect_equal(withr::with_seed(1234L,
                                .cGamma(mu1 = mu1, mu2 = mu2, n.gamma = 5,
                                        sqrt.V.rt_eff = sqrt.V.rt_eff,
                                        sqrt.V.eff = sqrt.V.eff,
                                        Tstat1 = Tstat1,
                                        n.rwe = 100L, fixed = TRUE)),
               expected)

  ### df = 3, fixed = FALSE

  withr::with_seed(1234L, {
    z1_samples <- mvtnorm::rmvnorm(5L, mean = mu1, diag(3))
    z2_samples <- mvtnorm::rmvnorm(5L, mean = mu2, diag(3))

    n_eff_samples <- -matrix(rowSums(z2_samples), 5, 3)
    n_rt_samples <- matrix(rowSums(z1_samples) / 2.0, 5, 3) + n_eff_samples

    z1tz1 <- numeric(5)
    for (i in 1L:5) {
      z1tz1[i] <- sum(z1_samples[i,]^2)
    }

    gamma_selected <- .chooseGamma(z1tz1 = z1tz1,
                                   n.rt = n_rt_samples[, 1L],
                                   n.eff = n_eff_samples[, 1L],
                                   n.cov = 3L)

    Icomb <- as.integer(gamma_selected$c_gamma  > Tstat1)
    Icomb_pval <- 1.0 - stats::pchisq(Tstat1, df = 3)

    ve_elas <- .veElastic(z1tz1 = z1tz1,
                          c.gamma = gamma_selected$c_gamma,
                          n.rt = n_rt_samples,
                          n.eff = n_eff_samples,
                          n.rwe = 100L)

    expected <- list("gamma" = gamma_selected$gamma,
                     "c.gamma" = gamma_selected$c_gamma,
                     "Icomb" = Icomb,
                     "Icomb.pval" = Icomb_pval,
                     "V.elastic" = ve_elas,
                     "settings" = list("n.gamma" = 5L,
                                       "fixed" = FALSE))
  })

  expect_equal(withr::with_seed(1234L,
                                .cGamma(mu1 = mu1, mu2 = mu2, n.gamma = 5,
                                        sqrt.V.rt_eff = sqrt.V.rt_eff,
                                        sqrt.V.eff = sqrt.V.eff,
                                        Tstat1 = Tstat1,
                                        n.rwe = 100L, fixed = FALSE)),
               expected)

  ## df = 1; fixed = TRUE

  mu1 <- withr::with_seed(1234L, runif(1))
  mu2 <- withr::with_seed(2345L, runif(1))
  sqrt.V.eff <- matrix(1, 1, 1)
  sqrt.V.rt_eff <- matrix(0.5, 1, 1)
  Tstat1 <- 0.4

  withr::with_seed(1234L, {
    z1_samples <- mvtnorm::rmvnorm(5L, mean = mu1, diag(1))
    z2_samples <- mvtnorm::rmvnorm(5L, mean = mu2, diag(1))

    n_eff_samples <- -matrix(rowSums(z2_samples), 5, 1, byrow = TRUE)
    n_rt_samples <- matrix(rowSums(z1_samples) / 2.0, 5, 1, byrow = TRUE) + n_eff_samples

    z1tz1 <- numeric(5)
    for (i in 1L:5) {
      z1tz1[i] <- sum(z1_samples[i,]^2)
    }

    gamma_selected <- list("gamma" = 0.05,
                           "c_gamma" = stats::qchisq(0.95, df = 1))

    Icomb <- as.integer(gamma_selected$c_gamma  > Tstat1)
    Icomb_pval <- 1.0 - stats::pchisq(Tstat1, df = 1)

    ve_elas <- .veElastic(z1tz1 = z1tz1,
                          c.gamma = gamma_selected$c_gamma,
                          n.rt = n_rt_samples,
                          n.eff = n_eff_samples,
                          n.rwe = 100L)

    expected <- list("gamma" = gamma_selected$gamma,
                     "c.gamma" = gamma_selected$c_gamma,
                     "Icomb" = Icomb,
                     "Icomb.pval" = Icomb_pval,
                     "V.elastic" = ve_elas,
                     "settings" = list("n.gamma" = 5L,
                                       "fixed" = TRUE))
  })

  expect_equal(withr::with_seed(1234L,
                                .cGamma(mu1 = mu1, mu2 = mu2, n.gamma = 5,
                                        sqrt.V.rt_eff = sqrt.V.rt_eff,
                                        sqrt.V.eff = sqrt.V.eff,
                                        Tstat1 = Tstat1,
                                        n.rwe = 100L, fixed = TRUE)),
               expected)

  ### df = 1, fixed = FALSE

  withr::with_seed(1234L, {
    z1_samples <- mvtnorm::rmvnorm(5L, mean = mu1, diag(1))
    z2_samples <- mvtnorm::rmvnorm(5L, mean = mu2, diag(1))

    n_eff_samples <- -matrix(rowSums(z2_samples), 5, 1)
    n_rt_samples <- matrix(rowSums(z1_samples) / 2.0, 5, 1) + n_eff_samples

    z1tz1 <- numeric(5)
    for (i in 1L:5) {
      z1tz1[i] <- sum(z1_samples[i,]^2)
    }

    gamma_selected <- .chooseGamma(z1tz1 = z1tz1,
                                   n.rt = n_rt_samples[, 1L],
                                   n.eff = n_eff_samples[, 1L],
                                   n.cov = 1L)

    Icomb <- as.integer(gamma_selected$c_gamma  > Tstat1)
    Icomb_pval <- 1.0 - stats::pchisq(Tstat1, df = 1)

    ve_elas <- .veElastic(z1tz1 = z1tz1,
                          c.gamma = gamma_selected$c_gamma,
                          n.rt = n_rt_samples,
                          n.eff = n_eff_samples,
                          n.rwe = 100L)

    expected <- list("gamma" = gamma_selected$gamma,
                     "c.gamma" = gamma_selected$c_gamma,
                     "Icomb" = Icomb,
                     "Icomb.pval" = Icomb_pval,
                     "V.elastic" = ve_elas,
                     "settings" = list("n.gamma" = 5L,
                                       "fixed" = FALSE))
  })

  expect_equal(withr::with_seed(1234L,
                                .cGamma(mu1 = mu1, mu2 = mu2, n.gamma = 5,
                                        sqrt.V.rt_eff = sqrt.V.rt_eff,
                                        sqrt.V.eff = sqrt.V.eff,
                                        Tstat1 = Tstat1,
                                        n.rwe = 100L, fixed = FALSE)),
               expected)

})
