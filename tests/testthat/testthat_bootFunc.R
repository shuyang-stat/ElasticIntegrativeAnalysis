test_that("`.generateElastic()` returns expected errors", {
  expect_error(.generateElastic(),
               "`mu1` must be a numeric vector")
  expect_error(.generateElastic(mu1 = "a"),
               "`mu1` must be a numeric vector")
  expect_error(.generateElastic(mu1 = matrix(1, 10, 1)),
               "`mu1` must be a numeric vector")
  expect_error(.generateElastic(mu1 = matrix(1, 1, 10)),
               "`mu1` must be a numeric vector")

  expect_error(.generateElastic(mu1 = 1:3),
               "`mu2` must be a numeric vector")
  expect_error(.generateElastic(mu1 = 1:3, mu2 = "a"),
               "`mu2` must be a numeric vector")
  expect_error(.generateElastic(mu1 = 1:3, mu2 = matrix(1, 3, 1)),
               "`mu2` must be a numeric vector")
  expect_error(.generateElastic(mu1 = 1:3, mu2 = matrix(1, 1, 3)),
               "`mu2` must be a numeric vector")
  expect_error(.generateElastic(mu1 = 1:3, mu2 = 1:4),
               "`mu2` must be a numeric vector")

  expect_error(.generateElastic(mu1 = 1:3, mu2 = 2:4),
               "`n.boot` must be an integer")
  expect_error(.generateElastic(mu1 = 1:3, mu2 = 2:4, n.boot = "a"),
               "`n.boot` must be an integer")
  expect_error(.generateElastic(mu1 = 1:3, mu2 = 2:4, n.boot = 1.5),
               "`n.boot` must be an integer")
  expect_error(.generateElastic(mu1 = 1:3, mu2 = 2:4, n.boot = 1L:2L),
               "`n.boot` must be an integer")
  expect_error(.generateElastic(mu1 = 1:3, mu2 = 2:4, n.boot = 0L),
               "`n.boot` must be an integer")

  expect_error(.generateElastic(mu1 = 1:3, mu2 = 2:4, n.boot = 10L),
               "`c.gamma` must be a scalar numeric")
  expect_error(.generateElastic(mu1 = 1:3, mu2 = 2:4, n.boot = 10L,
                                c.gamma = "a"),
               "`c.gamma` must be a scalar numeric")
  expect_error(.generateElastic(mu1 = 1:3, mu2 = 2:4, n.boot = 10L,
                                c.gamma = matrix(1, 3, 1)),
               "`c.gamma` must be a scalar numeric")
  expect_error(.generateElastic(mu1 = 1:3, mu2 = 2:4, n.boot = 10L,
                                c.gamma = matrix(1, 1, 3)),
               "`c.gamma` must be a scalar numeric")
  expect_error(.generateElastic(mu1 = 1:3, mu2 = 2:4, n.boot = 10L,
                                c.gamma = 1:4),
               "`c.gamma` must be a scalar numeric")

  expect_error(.generateElastic(mu1 = 1:3, mu2 = 2:4, n.boot = 10L,
                                c.gamma = 0.2),
               "`sqrt.V.rt_eff` must be a numeric matrix")
  expect_error(.generateElastic(mu1 = 1:3, mu2 = 2:4, n.boot = 10L,
                                c.gamma = 0.2,
                                sqrt.V.rt_eff = "a"),
               "`sqrt.V.rt_eff` must be a numeric matrix")
  expect_error(.generateElastic(mu1 = 1:3, mu2 = 2:4, n.boot = 10L,
                                c.gamma = 0.2,
                                sqrt.V.rt_eff = numeric(9)),
               "`sqrt.V.rt_eff` must be a numeric matrix")
  expect_error(.generateElastic(mu1 = 1:3, mu2 = 2:4, n.boot = 10L,
                                c.gamma = 0.2,
                                sqrt.V.rt_eff = matrix(1, 4, 3)),
               "`sqrt.V.rt_eff` must be a numeric matrix")
  expect_error(.generateElastic(mu1 = 1:3, mu2 = 2:4, n.boot = 10L,
                                c.gamma = 0.2,
                                sqrt.V.rt_eff = matrix(1, 3, 4)),
               "`sqrt.V.rt_eff` must be a numeric matrix")
  expect_error(.generateElastic(mu1 = 1:3, mu2 = 2:4, n.boot = 10L,
                                c.gamma = 0.2,
                                sqrt.V.rt_eff = matrix("a", 3, 3)),
               "`sqrt.V.rt_eff` must be a numeric matrix")

  expect_error(.generateElastic(mu1 = 1:3, mu2 = 2:4, n.boot = 10L,
                                c.gamma = 0.2,
                                sqrt.V.rt_eff = matrix(1, 3, 3)),
               "`sqrt.V.eff` must be a numeric matrix")
  expect_error(.generateElastic(mu1 = 1:3, mu2 = 2:4, n.boot = 10L,
                                c.gamma = 0.2,
                                sqrt.V.rt_eff = matrix(1, 3, 3),
                                sqrt.V.eff = "a"),
               "`sqrt.V.eff` must be a numeric matrix")
  expect_error(.generateElastic(mu1 = 1:3, mu2 = 2:4, n.boot = 10L,
                                c.gamma = 0.2,
                                sqrt.V.rt_eff = matrix(1, 3, 3),
                                sqrt.V.eff = numeric(9)),
               "`sqrt.V.eff` must be a numeric matrix")
  expect_error(.generateElastic(mu1 = 1:3, mu2 = 2:4, n.boot = 10L,
                                c.gamma = 0.2,
                                sqrt.V.rt_eff = matrix(1, 3, 3),
                                sqrt.V.eff = matrix(1, 4, 3)),
               "`sqrt.V.eff` must be a numeric matrix")
  expect_error(.generateElastic(mu1 = 1:3, mu2 = 2:4, n.boot = 10L,
                                c.gamma = 0.2,
                                sqrt.V.rt_eff = matrix(1, 3, 3),
                                sqrt.V.eff = matrix(1, 3, 4)),
               "`sqrt.V.eff` must be a numeric matrix")
  expect_error(.generateElastic(mu1 = 1:3, mu2 = 2:4, n.boot = 10L,
                                c.gamma = 0.2,
                                sqrt.V.rt_eff = matrix(1, 3, 3),
                                sqrt.V.eff = matrix("a", 3, 3)),
               "`sqrt.V.eff` must be a numeric matrix")

})

test_that("`.generateElastic()` returns expected results", {

  withr::with_seed(1234L, {
                     z0 <- mvtnorm::rmvnorm(10L, mean = c(0.0, 0.0, 0.0),
                                            diag(3L)) |> t()
                     zz0 <- mvtnorm::rmvnorm(10L, mean = c(0.0, 0.0, 0.0),
                                             diag(3L)) |> t()

                   })

  # p x n.boot
  z1 <- z0 + c(0.1, 0.2, 0.3)
  z2 <- zz0 + c(0.2, 0.3, 0.4)

  args <- list("n.boot" = 10L,
               "mu1" = c(0.1, 0.2, 0.3),
               "mu2" = c(0.2, 0.3, 0.4),
               "sqrt.V.rt_eff" = matrix(1, 3, 3),
               "sqrt.V.eff" = matrix(1, 3, 3),
               "c.gamma" = 2.0)

  z1tz1 <- apply(z1, 2, crossprod)

  mat_yes <- matrix(FALSE, 3L, 10L)
  for (i in 1L:length(z1tz1)) {
    for (j in 1L:3) {
      mat_yes[j, i] <- z1tz1[i] < 2
    }
  }

  z_eff <- matrix(-colSums(z2), 10, 3) |> t()
  z_rt <- {matrix(colSums(z1), 10, 3) |> t()} + z_eff

  expected <- matrix(0.0, 3L, 10L)
  expected[mat_yes] <- z_eff[mat_yes]
  expected[!mat_yes] <- z_rt[!mat_yes]
  expected <- t(expected)

  testthat::expect_equal(withr::with_seed(1234L,
                                          do.call(".generateElastic", args)),
                         expected)

})

test_that("`.boot_func_step()` returns expected results", {

  args <- list("n.boot" = 10L,
               "mu1" = c(0.1, 0.2, 0.3),
               "mu2" = c(0.2, 0.3, 0.4),
               "sqrt.V.rt_eff" = matrix(1, 3, 3),
               "sqrt.V.eff" = matrix(1, 3, 3),
               "c.gamma" = 2.0)

  genValue <- withr::with_seed(1234L,
                               do.call(".generateElastic", args))

  expected <- matrix(0, 2, 3, dimnames = list(c("1.25%", "98.75%"), NULL))
  for (i in 1L:3) {
    expected[1L, i] <- stats::quantile(genValue[, i], probs = c(0.0125), type = 5L)
    expected[2L, i] <- stats::quantile(genValue[, i], probs = c(0.9875), type = 5L)
  }

  expect_equal(withr::with_seed(1234L,
                                do.call(".boot_func_step", args)),
                         expected)



})

test_that("`.ciBelow()` returns expected errors", {
  expect_error(.ciBelow(),
               "`mu1` must be a numeric vector")
  expect_error(.ciBelow(mu1 = "a"),
               "`mu1` must be a numeric vector")
  expect_error(.ciBelow(mu1 = matrix(1, 10, 1)),
               "`mu1` must be a numeric vector")
  expect_error(.ciBelow(mu1 = matrix(1, 1, 10)),
               "`mu1` must be a numeric vector")

  expect_error(.ciBelow(mu1 = 1:3),
               "`mu2` must be a numeric vector")
  expect_error(.ciBelow(mu1 = 1:3, mu2 = "a"),
               "`mu2` must be a numeric vector")
  expect_error(.ciBelow(mu1 = 1:3, mu2 = matrix(1, 3, 1)),
               "`mu2` must be a numeric vector")
  expect_error(.ciBelow(mu1 = 1:3, mu2 = matrix(1, 1, 3)),
               "`mu2` must be a numeric vector")
  expect_error(.ciBelow(mu1 = 1:3, mu2 = 1:4),
               "`mu2` must be a numeric vector")

  expect_error(.ciBelow(mu1 = 1:3, mu2 = 2:4),
               "`c.gamma` must be a scalar numeric")
  expect_error(.ciBelow(mu1 = 1:3, mu2 = 2:4, c.gamma = "a"),
               "`c.gamma` must be a scalar numeric")
  expect_error(.ciBelow(mu1 = 1:3, mu2 = 2:4, c.gamma = matrix(1, 3, 1)),
               "`c.gamma` must be a scalar numeric")
  expect_error(.ciBelow(mu1 = 1:3, mu2 = 2:4, c.gamma = matrix(1, 1, 3)),
               "`c.gamma` must be a scalar numeric")
  expect_error(.ciBelow(mu1 = 1:3, mu2 = 2:4, c.gamma = 1:4),
               "`c.gamma` must be a scalar numeric")

  expect_error(.ciBelow(mu1 = 1:3, mu2 = 2:4, c.gamma = 0.2),
               "`sqrt.V.rt_eff` must be provided")
  expect_error(.ciBelow(mu1 = 1:3, mu2 = 2:4,
                        c.gamma = 0.2,
                        sqrt.V.rt_eff = matrix(1, 3, 3)),
               "`sqrt.V.eff` must be provided")

  expect_error(.ciBelow(mu1 = 1:3, mu2 = 2:4,
                        c.gamma = 0.2,
                        sqrt.V.rt_eff = matrix(1, 3, 3),
                        sqrt.V.eff = matrix(1, 3, 3)),
               "`n.rwe` must be an integer")
  expect_error(.ciBelow(mu1 = 1:3, mu2 = 2:4,
                        c.gamma = 0.2,
                        sqrt.V.rt_eff = matrix(1, 3, 3),
                        sqrt.V.eff = matrix(1, 3, 3), n.rwe = "a"),
               "`n.rwe` must be an integer")
  expect_error(.ciBelow(mu1 = 1:3, mu2 = 2:4,
                        c.gamma = 0.2,
                        sqrt.V.rt_eff = matrix(1, 3, 3),
                        sqrt.V.eff = matrix(1, 3, 3), n.rwe = 1.5),
               "`n.rwe` must be an integer")
  expect_error(.ciBelow(mu1 = 1:3, mu2 = 2:4,
                        c.gamma = 0.2,
                        sqrt.V.rt_eff = matrix(1, 3, 3),
                        sqrt.V.eff = matrix(1, 3, 3), n.rwe = 1L:2L),
               "`n.rwe` must be an integer")
  expect_error(.ciBelow(mu1 = 1:3, mu2 = 2:4,
                        c.gamma = 0.2,
                        sqrt.V.rt_eff = matrix(1, 3, 3),
                        sqrt.V.eff = matrix(1, 3, 3), n.rwe = 0L),
               "`n.rwe` must be an integer")

  expect_error(.ciBelow(mu1 = 1:3, mu2 = 2:4,
                        c.gamma = 0.2,
                        sqrt.V.rt_eff = matrix(1, 3, 3),
                        sqrt.V.eff = matrix(1, 3, 3), n.rwe = 50L),
               "`n.boot` must be an integer")
  expect_error(.ciBelow(mu1 = 1:3, mu2 = 2:4,
                        c.gamma = 0.2,
                        sqrt.V.rt_eff = matrix(1, 3, 3),
                        sqrt.V.eff = matrix(1, 3, 3), n.rwe = 50L, n.boot = "a"),
               "`n.boot` must be an integer")
  expect_error(.ciBelow(mu1 = 1:3, mu2 = 2:4,
                        c.gamma = 0.2,
                        sqrt.V.rt_eff = matrix(1, 3, 3),
                        sqrt.V.eff = matrix(1, 3, 3), n.rwe = 50L, n.boot = 1.5),
               "`n.boot` must be an integer")
  expect_error(.ciBelow(mu1 = 1:3, mu2 = 2:4,
                        c.gamma = 0.2,
                        sqrt.V.rt_eff = matrix(1, 3, 3),
                        sqrt.V.eff = matrix(1, 3, 3), n.rwe = 50L, n.boot = 1L:2L),
               "`n.boot` must be an integer")
  expect_error(.ciBelow(mu1 = 1:3, mu2 = 2:4,
                        c.gamma = 0.2,
                        sqrt.V.rt_eff = matrix(1, 3, 3),
                        sqrt.V.eff = matrix(1, 3, 3), n.rwe = 50L, n.boot = 0L),
               "`n.boot` must be an integer")
})

test_that("`ciBelow()` returns expected results", {

  args <- list("n.boot" = 10L,
               "n.rwe" = 100L,
               "mu1" = c(0.1, 0.2, 0.3),
               "mu2" = c(0.2, 0.3, 0.4),
               "sqrt.V.rt_eff" = matrix(1, 3, 3),
               "sqrt.V.eff" = matrix(1, 3, 3),
               "c.gamma" = 2.0)

  # simulation for elastic inference
  qq1 <- matrix(NA_real_, 10L, 3L)
  qq2 <- matrix(NA_real_, 10L, 3L)
  withr::with_seed(1234L, {
    UU <- mvtnorm::qmvnorm(0.975, mean = c(0.1, 0.2, 0.3), sigma = diag(3),
                           tail = "both.tails")$quantile
    UU <- c(UU, UU, UU)

    for (i in 1L:10L) {
      mu1_new <- mvtnorm::rmvnorm(1L, mean = c(0.1, 0.2, 0.3), sigma = diag(3L)) |> drop()

      while (any(mu1_new > UU | mu1_new < -UU)) {
        mu1_new <- mvtnorm::rmvnorm(1, mean = c(0.1, 0.2, 0.3), sigma = diag(3L)) |> drop()
      }

      res <- .boot_func_step(mu1 = mu1_new, mu2 = c(0.2, 0.3, 0.4), n.boot = 10L,
                             c.gamma = 2.0,
                             sqrt.V.rt_eff = matrix(1, 3, 3),
                             sqrt.V.eff = matrix(1, 3, 3))

      qq1[i, ] <- res[1L, ]
      qq2[i, ] <- res[2L, ]
    }})
  expected <- matrix(0, 2, 3)
  for (i in 1L:3L) {
    expected[1, i] <- min(qq1[, i]) / 10
    expected[2, i] <- max(qq2[, i]) / 10
  }

  expect_equal(withr::with_seed(1234L,
                                do.call(".ciBelow", args)),
               expected)

  args <- list("n.boot" = 10L,
               "n.rwe" = 100L,
               "mu1" = c(0.1),
               "mu2" = c(0.2),
               "sqrt.V.rt_eff" = matrix(1, 1, 1),
               "sqrt.V.eff" = matrix(1, 1, 1),
               "c.gamma" = 2.0)

  # simulation for elastic inference
  qq1 <- matrix(NA_real_, 10L, 1L)
  qq2 <- matrix(NA_real_, 10L, 1L)
  withr::with_seed(1234L, {
    UU <- mvtnorm::qmvnorm(0.975, mean = c(0.1), sigma = diag(1),
                           tail = "both.tails")$quantile
    UU <- c(UU, UU, UU)

    for (i in 1L:10L) {
      mu1_new <- mvtnorm::rmvnorm(1L, mean = c(0.1), sigma = diag(1L)) |> drop()

      while (any(mu1_new > UU | mu1_new < -UU)) {
        mu1_new <- mvtnorm::rmvnorm(1, mean = c(0.1), sigma = diag(1L)) |> drop()
      }

      res <- .boot_func_step(mu1 = mu1_new, mu2 = c(0.2), n.boot = 10L,
                             c.gamma = c(2),
                             sqrt.V.rt_eff = matrix(1, 1, 1),
                             sqrt.V.eff = matrix(1, 1, 1))

      qq1[i, ] <- res[1L, ]
      qq2[i, ] <- res[2L, ]
    }})
  expected <- matrix(0, 2, 1)
  for (i in 1L:1L) {
    expected[1, i] <- min(qq1[, i]) / 10
    expected[2, i] <- max(qq2[, i]) / 10
  }

  expect_equal(withr::with_seed(1234L,
                                do.call(".ciBelow", args)),
               expected)
})

test_that("`.ciAbove()` returns expected errors", {
  expect_error(.ciAbove(),
               "`V.rt` must be a numeric matrix")
  expect_error(.ciAbove(V.rt = "a"),
               "`V.rt` must be a numeric matrix")
  expect_error(.ciAbove(V.rt = numeric(9)),
               "`V.rt` must be a numeric matrix")
  expect_error(.ciAbove(V.rt = matrix(1, 4, 3)),
               "`V.rt` must be a numeric matrix")
  expect_error(.ciAbove(V.rt = matrix(1, 3, 4)),
               "`V.rt` must be a numeric matrix")
  expect_error(.ciAbove(V.rt = matrix("a", 3, 3)),
               "`V.rt` must be a numeric matrix")

  expect_error(.ciAbove(V.rt = matrix(1, 3, 3)),
               "`n.rwe` must be an integer")
  expect_error(.ciAbove(V.rt = matrix(1, 3, 3), n.rwe = "a"),
               "`n.rwe` must be an integer")
  expect_error(.ciAbove(V.rt = matrix(1, 3, 3), n.rwe = 1.5),
               "`n.rwe` must be an integer")
  expect_error(.ciAbove(V.rt = matrix(1, 3, 3), n.rwe = 1L:2L),
               "`n.rwe` must be an integer")
  expect_error(.ciAbove(V.rt = matrix(1, 3, 3), n.rwe = 0L),
               "`n.rwe` must be an integer")

  expect_error(.ciAbove(V.rt = matrix(1, 3, 3), n.rwe = 50L),
               "`n.boot` must be an integer")
  expect_error(.ciAbove(V.rt = matrix(1, 3, 3), n.rwe = 50L, n.boot = "a"),
               "`n.boot` must be an integer")
  expect_error(.ciAbove(V.rt = matrix(1, 3, 3), n.rwe = 50L, n.boot = 1.5),
               "`n.boot` must be an integer")
  expect_error(.ciAbove(V.rt = matrix(1, 3, 3), n.rwe = 50L, n.boot = 1L:2L),
               "`n.boot` must be an integer")
  expect_error(.ciAbove(V.rt = matrix(1, 3, 3), n.rwe = 50L, n.boot = 0L),
               "`n.boot` must be an integer")
})

test_that("`.ciAbove()` returns expected results", {

  V.rt <- diag(runif(3))

  args <- list("n.boot" = 10L,
               "n.rwe" = 100L,
               "V.rt" = V.rt)

  z_rt <- withr::with_seed(1234L,
                           mvtnorm::rmvnorm(10L, mean = rep(0.0, 3), sigma = V.rt))

  expected <- matrix(0.0, 2, 3, dimnames = list(c("2.5%", "97.5%"), NULL))
  for (i in 1L:3L) {
    expected[1, i] <- stats::quantile(z_rt[, i], probs = 0.025, type = 5L) / 10.0
    expected[2, i] <- stats::quantile(z_rt[, i], probs = 0.975, type = 5L) / 10.0
  }

  expect_equal(withr::with_seed(1234L,
                                do.call(".ciAbove", args)),
               expected)

  V.rt <- diag(runif(1), nrow = 1, ncol = 1)

  args <- list("n.boot" = 10L,
               "n.rwe" = 100L,
               "V.rt" = V.rt)

  z_rt <- withr::with_seed(1234L,
                           mvtnorm::rmvnorm(10L, mean = 0.0, sigma = V.rt))

  expected <- matrix(0.0, 2, 1, dimnames = list(c("2.5%", "97.5%"), NULL))
  for (i in 1L:1L) {
    expected[1, i] <- stats::quantile(z_rt[, i], probs = 0.025, type = 5L) / 10.0
    expected[2, i] <- stats::quantile(z_rt[, i], probs = 0.975, type = 5L) / 10.0
  }

  expect_equal(withr::with_seed(1234L,
                                do.call(".ciAbove", args)),
               expected)

})

test_that("`.bootFunc()` returns expected errors", {
  expect_error(.bootFunc(),
               "`mu1` must be a numeric vector")
  expect_error(.bootFunc(mu1 = "a"),
               "`mu1` must be a numeric vector")
  expect_error(.bootFunc(mu1 = matrix(1, 10, 1)),
               "`mu1` must be a numeric vector")
  expect_error(.bootFunc(mu1 = matrix(1, 1, 10)),
               "`mu1` must be a numeric vector")

  expect_error(.bootFunc(mu1 = 1:3),
               "`mu2` must be provided")
  expect_error(.bootFunc(mu1 = 1:3, mu2 = 2:4),
               "`c.gamma` must be provided")

  expect_error(.bootFunc(mu1 = 1:3, mu2 = 2:4, c.gamma = 0.2),
               "`V.rt` must be a numeric matrix")
  expect_error(.bootFunc(mu1 = 1:3, mu2 = 2:4, c.gamma = 0.2,
                         V.rt = "a"),
               "`V.rt` must be a numeric matrix")
  expect_error(.bootFunc(mu1 = 1:3, mu2 = 2:4, c.gamma = 0.2,
                         V.rt = numeric(9)),
               "`V.rt` must be a numeric matrix")
  expect_error(.bootFunc(mu1 = 1:3, mu2 = 2:4, c.gamma = 0.2,
                         V.rt = matrix(1, 4, 3)),
               "`V.rt` must be a numeric matrix")
  expect_error(.bootFunc(mu1 = 1:3, mu2 = 2:4, c.gamma = 0.2,
                         V.rt = matrix(1, 3, 4)),
               "`V.rt` must be a numeric matrix")
  expect_error(.bootFunc(mu1 = 1:3, mu2 = 2:4, c.gamma = 0.2,
                         V.rt = matrix("a", 3, 3)),
               "`V.rt` must be a numeric matrix")

  expect_error(.bootFunc(mu1 = 1:3, mu2 = 2:4, c.gamma = 0.2,
                         V.rt = matrix(1, 3, 3)),
               "`sqrt.V.rt_eff` must be provided")

  expect_error(.bootFunc(mu1 = 1:3, mu2 = 2:4, c.gamma = 0.2,
                         V.rt = matrix(1, 3, 3),
                         sqrt.V.rt_eff = matrix(1, 3, 3)),
               "`sqrt.V.eff` must be provided")

  expect_error(.bootFunc(mu1 = 1:3, mu2 = 2:4, c.gamma = 0.2,
                         V.rt = matrix(1, 3, 3),
                         sqrt.V.rt_eff = matrix(1, 3, 3),
                         sqrt.V.eff = matrix(1, 3, 3)),
               "`psi` must be a numeric matrix")
  expect_error(.bootFunc(mu1 = 1:3, mu2 = 2:4, c.gamma = 0.2,
                         V.rt = matrix(1, 3, 3),
                         sqrt.V.rt_eff = matrix(1, 3, 3),
                         sqrt.V.eff = matrix(1, 3, 3), psi= "a"),
               "`psi` must be a numeric matrix")
  expect_error(.bootFunc(mu1 = 1:3, mu2 = 2:4, c.gamma = 0.2,
                         V.rt = matrix(1, 3, 3),
                         sqrt.V.rt_eff = matrix(1, 3, 3),
                         sqrt.V.eff = matrix(1, 3, 3),
                         psi = numeric(9)),
               "`psi` must be a numeric matrix")
  expect_error(.bootFunc(mu1 = 1:3, mu2 = 2:4, c.gamma = 0.2,
                         V.rt = matrix(1, 3, 3),
                         sqrt.V.rt_eff = matrix(1, 3, 3),
                         sqrt.V.eff = matrix(1, 3, 3),
                         psi = matrix(1, 5, 2)),
               "`psi` must be a numeric matrix")

  expect_error(.bootFunc(mu1 = 1:3, mu2 = 2:4, c.gamma = 0.2,
                         V.rt = matrix(1, 3, 3),
                         sqrt.V.rt_eff = matrix(1, 3, 3),
                         sqrt.V.eff = matrix(1, 3, 3),
                         psi = matrix(1, 5, 3)),
               "`ve` must be a numeric matrix")
  expect_error(.bootFunc(mu1 = 1:3, mu2 = 2:4, c.gamma = 0.2,
                         V.rt = matrix(1, 3, 3),
                         sqrt.V.rt_eff = matrix(1, 3, 3),
                         sqrt.V.eff = matrix(1, 3, 3),
                         psi = matrix(1, 5, 3), ve= "a"),
               "`ve` must be a numeric matrix")
  expect_error(.bootFunc(mu1 = 1:3, mu2 = 2:4, c.gamma = 0.2,
                         V.rt = matrix(1, 3, 3),
                         sqrt.V.rt_eff = matrix(1, 3, 3),
                         sqrt.V.eff = matrix(1, 3, 3),
                         psi = matrix(1, 5, 3),
                         ve = numeric(9)),
               "`ve` must be a numeric matrix")
  expect_error(.bootFunc(mu1 = 1:3, mu2 = 2:4, c.gamma = 0.2,
                         V.rt = matrix(1, 3, 3),
                         sqrt.V.rt_eff = matrix(1, 3, 3),
                         sqrt.V.eff = matrix(1, 3, 3),
                         psi = matrix(1, 5, 3),
                         ve = matrix(1, 5, 2)),
               "`ve` must be a numeric matrix")
  expect_error(.bootFunc(mu1 = 1:3, mu2 = 2:4, c.gamma = 0.2,
                         V.rt = matrix(1, 3, 3),
                         sqrt.V.rt_eff = matrix(1, 3, 3),
                         sqrt.V.eff = matrix(1, 3, 3),
                         psi = matrix(1, 5, 3),
                         ve = matrix(1, 4, 3)),
               "`ve` must be a numeric matrix")

  expect_error(.bootFunc(mu1 = 1:3, mu2 = 2:4, c.gamma = 0.2,
                         V.rt = matrix(1, 3, 3),
                         sqrt.V.rt_eff = matrix(1, 3, 3),
                         sqrt.V.eff = matrix(1, 3, 3),
                         psi = matrix(1, 5, 3),
                         ve = matrix(1, 5, 3)),
               "`psi.elastic` must be a numeric vector")
  expect_error(.bootFunc(mu1 = 1:3, mu2 = 2:4, c.gamma = 0.2,
                         V.rt = matrix(1, 3, 3),
                         sqrt.V.rt_eff = matrix(1, 3, 3),
                         sqrt.V.eff = matrix(1, 3, 3),
                         psi = matrix(1, 5, 3),
                         ve = matrix(1, 5, 3), psi.elastic = "a"),
               "`psi.elastic` must be a numeric vector")
  expect_error(.bootFunc(mu1 = 1:3, mu2 = 2:4, c.gamma = 0.2,
                         V.rt = matrix(1, 3, 3),
                         sqrt.V.rt_eff = matrix(1, 3, 3),
                         sqrt.V.eff = matrix(1, 3, 3),
                         psi = matrix(1, 5, 3),
                         ve = matrix(1, 5, 3),
                         psi.elastic = numeric(4)),
               "`psi.elastic` must be a numeric vector")
  expect_error(.bootFunc(mu1 = 1:3, mu2 = 2:4, c.gamma = 0.2,
                         V.rt = matrix(1, 3, 3),
                         sqrt.V.rt_eff = matrix(1, 3, 3),
                         sqrt.V.eff = matrix(1, 3, 3),
                         psi = matrix(1, 5, 3),
                         ve = matrix(1, 5, 3),
                         psi.elastic = matrix(1, 1, 3)),
               "`psi.elastic` must be a numeric vector")
  expect_error(.bootFunc(mu1 = 1:3, mu2 = 2:4, c.gamma = 0.2,
                         V.rt = matrix(1, 3, 3),
                         sqrt.V.rt_eff = matrix(1, 3, 3),
                         sqrt.V.eff = matrix(1, 3, 3),
                         psi = matrix(1, 5, 3),
                         ve = matrix(1, 5, 3),
                         psi.elastic = matrix(1, 3, 1)),
               "`psi.elastic` must be a numeric vector")

  expect_error(.bootFunc(mu1 = 1:3, mu2 = 2:4, c.gamma = 0.2,
                         V.rt = matrix(1, 3, 3),
                         sqrt.V.rt_eff = matrix(1, 3, 3),
                         sqrt.V.eff = matrix(1, 3, 3),
                         psi = matrix(1, 5, 3),
                         ve = matrix(1, 5, 3),
                         psi.elastic = numeric(3)),
               "`n.rwe` must be provided")

  expect_error(.bootFunc(mu1 = 1:3, mu2 = 2:4, c.gamma = 0.2,
                         V.rt = matrix(1, 3, 3),
                         sqrt.V.rt_eff = matrix(1, 3, 3),
                         sqrt.V.eff = matrix(1, 3, 3),
                         psi = matrix(1, 5, 3),
                         ve = matrix(1, 5, 3),
                         psi.elastic = numeric(3), n.rwe = 10L),
               "`n.boot` must be an integer")
  expect_error(.bootFunc(mu1 = 1:3, mu2 = 2:4, c.gamma = 0.2,
                         V.rt = matrix(1, 3, 3),
                         sqrt.V.rt_eff = matrix(1, 3, 3),
                         sqrt.V.eff = matrix(1, 3, 3),
                         psi = matrix(1, 5, 3),
                         ve = matrix(1, 5, 3),
                         psi.elastic = numeric(3), n.rwe = 10L, n.boot = "a"),
               "`n.boot` must be an integer")
  expect_error(.bootFunc(mu1 = 1:3, mu2 = 2:4, c.gamma = 0.2,
                         V.rt = matrix(1, 3, 3),
                         sqrt.V.rt_eff = matrix(1, 3, 3),
                         sqrt.V.eff = matrix(1, 3, 3),
                         psi = matrix(1, 5, 3),
                         ve = matrix(1, 5, 3),
                         psi.elastic = numeric(3), n.rwe = 10L, n.boot = 1.5),
               "`n.boot` must be an integer")
  expect_error(.bootFunc(mu1 = 1:3, mu2 = 2:4, c.gamma = 0.2,
                         V.rt = matrix(1, 3, 3),
                         sqrt.V.rt_eff = matrix(1, 3, 3),
                         sqrt.V.eff = matrix(1, 3, 3),
                         psi = matrix(1, 5, 3),
                         ve = matrix(1, 5, 3),
                         psi.elastic = numeric(3), n.rwe = 10L, n.boot = 1L:2L),
               "`n.boot` must be an integer")
  expect_error(.bootFunc(mu1 = 1:3, mu2 = 2:4, c.gamma = 0.2,
                         V.rt = matrix(1, 3, 3),
                         sqrt.V.rt_eff = matrix(1, 3, 3),
                         sqrt.V.eff = matrix(1, 3, 3),
                         psi = matrix(1, 5, 3),
                         ve = matrix(1, 5, 3),
                         psi.elastic = numeric(3), n.rwe = 10L, n.boot = 0L),
               "`n.boot` must be an integer")

  expect_error(.bootFunc(mu1 = 1:3, mu2 = 2:4, c.gamma = 0.2,
                         V.rt = matrix(1, 3, 3),
                         sqrt.V.rt_eff = matrix(1, 3, 3),
                         sqrt.V.eff = matrix(1, 3, 3),
                         psi = matrix(1, 5, 3),
                         ve = matrix(1, 5, 3),
                         psi.elastic = numeric(3), n.rwe = 10L, n.boot = 10L),
               "`thres.psi` must be a numeric")
  expect_error(.bootFunc(mu1 = 1:3, mu2 = 2:4, c.gamma = 0.2,
                         V.rt = matrix(1, 3, 3),
                         sqrt.V.rt_eff = matrix(1, 3, 3),
                         sqrt.V.eff = matrix(1, 3, 3),
                         psi = matrix(1, 5, 3),
                         ve = matrix(1, 5, 3),
                         psi.elastic = numeric(3), n.rwe = 10L, n.boot = 10L,
                         thres.psi = "a"),
               "`thres.psi` must be a numeric")
  expect_error(.bootFunc(mu1 = 1:3, mu2 = 2:4, c.gamma = 0.2,
                         V.rt = matrix(1, 3, 3),
                         sqrt.V.rt_eff = matrix(1, 3, 3),
                         sqrt.V.eff = matrix(1, 3, 3),
                         psi = matrix(1, 5, 3),
                         ve = matrix(1, 5, 3),
                         psi.elastic = numeric(3), n.rwe = 10L, n.boot = 10L,
                         thres.psi = matrix(1, 1, 1)),
               "`thres.psi` must be a numeric")
  expect_error(.bootFunc(mu1 = 1:3, mu2 = 2:4, c.gamma = 0.2,
                         V.rt = matrix(1, 3, 3),
                         sqrt.V.rt_eff = matrix(1, 3, 3),
                         sqrt.V.eff = matrix(1, 3, 3),
                         psi = matrix(1, 5, 3),
                         ve = matrix(1, 5, 3),
                         psi.elastic = numeric(3), n.rwe = 10L, n.boot = 10L,
                         thres.psi = 1L:2L),
               "`thres.psi` must be a numeric")
  expect_error(.bootFunc(mu1 = 1:3, mu2 = 2:4, c.gamma = 0.2,
                         V.rt = matrix(1, 3, 3),
                         sqrt.V.rt_eff = matrix(1, 3, 3),
                         sqrt.V.eff = matrix(1, 3, 3),
                         psi = matrix(1, 5, 3),
                         ve = matrix(1, 5, 3),
                         psi.elastic = numeric(3), n.rwe = 10L, n.boot = 10L,
                         thres.psi = character(3)),
               "`thres.psi` must be a numeric")

  expect_error(.bootFunc(mu1 = 1:3, mu2 = 2:4, c.gamma = 0.2,
                         V.rt = matrix(1, 3, 3),
                         sqrt.V.rt_eff = matrix(1, 3, 3),
                         sqrt.V.eff = matrix(1, 3, 3),
                         psi = matrix(1, 5, 3),
                         ve = matrix(1, 5, 3),
                         psi.elastic = numeric(3), n.rwe = 10L, n.boot = 10L,
                         thres.psi = 0.5),
               "`Tstat` must be a numeric")
  expect_error(.bootFunc(mu1 = 1:3, mu2 = 2:4, c.gamma = 0.2,
                         V.rt = matrix(1, 3, 3),
                         sqrt.V.rt_eff = matrix(1, 3, 3),
                         sqrt.V.eff = matrix(1, 3, 3),
                         psi = matrix(1, 5, 3),
                         ve = matrix(1, 5, 3),
                         psi.elastic = numeric(3),
                         n.rwe = 10L, n.boot = 10L,
                         thres.psi = 0.5,
                         Tstat = "a"),
               "`Tstat` must be a numeric")
  expect_error(.bootFunc(mu1 = 1:3, mu2 = 2:4, c.gamma = 0.2,
                         V.rt = matrix(1, 3, 3),
                         sqrt.V.rt_eff = matrix(1, 3, 3),
                         sqrt.V.eff = matrix(1, 3, 3),
                         psi = matrix(1, 5, 3),
                         ve = matrix(1, 5, 3),
                         psi.elastic = numeric(3),
                         n.rwe = 10L, n.boot = 10L,
                         thres.psi = 0.5,
                         Tstat = matrix(1, 1, 1)),
               "`Tstat` must be a numeric")
  expect_error(.bootFunc(mu1 = 1:3, mu2 = 2:4, c.gamma = 0.2,
                         V.rt = matrix(1, 3, 3),
                         sqrt.V.rt_eff = matrix(1, 3, 3),
                         sqrt.V.eff = matrix(1, 3, 3),
                         psi = matrix(1, 5, 3),
                         ve = matrix(1, 5, 3),
                         psi.elastic = numeric(3),
                         n.rwe = 10L, n.boot = 10L,
                         thres.psi = 0.5,
                         Tstat = 1L:2L),
               "`Tstat` must be a numeric")
  expect_error(.bootFunc(mu1 = 1:3, mu2 = 2:4, c.gamma = 0.2,
                         V.rt = matrix(1, 3, 3),
                         sqrt.V.rt_eff = matrix(1, 3, 3),
                         sqrt.V.eff = matrix(1, 3, 3),
                         psi = matrix(1, 5, 3),
                         ve = matrix(1, 5, 3),
                         psi.elastic = numeric(3),
                         n.rwe = 10L, n.boot = 10L,
                         thres.psi = 0.5,
                         Tstat = character(3)),
               "`Tstat` must be a numeric")
})

test_that("`.bootFunc()` returns expected results", {

  args <- list("mu1" = c(0.1, 0.2, 0.3),
               "mu2" = c(0.2, 0.3, 0.4),
               "c.gamma" = 2.0,
               "V.rt" = matrix(0.1, 3, 3),
               "sqrt.V.rt_eff" = matrix(0.2, 3, 3),
               "sqrt.V.eff" = matrix(0.3, 3, 3,),
               "psi" = matrix(withr::with_seed(1234, runif(12)), 4, 3),
               "ve" = matrix(0.1, 4, 3),
               "psi.elastic" = c(0.2, 0.25, 0.3),
               "n.rwe" = 100L,
               "n.boot" = 10L,
               "thres.psi" = 0.5,
               "Tstat" = .1)

  expected <- withr::with_seed(2345L, {
    bootq1 <- args$psi - stats::qnorm(0.975) * sqrt(args$ve)
    bootq2 <- args$psi + stats::qnorm(0.975) * sqrt(args$ve)

    boot_elas <- .ciBelow(mu1 = args$mu1, mu2 = args$mu2,
                          c.gamma = args$c.gamma,
                          sqrt.V.rt_eff = args$sqrt.V.rt_eff,
                          sqrt.V.eff = args$sqrt.V.eff,
                          n.rwe = args$n.rwe, n.boot = args$n.boot)

    bootq1 <- rbind(bootq1, "elastic" = args$psi.elastic + boot_elas[1L, ])
    bootq2 <- rbind(bootq2, "elastic" = args$psi.elastic + boot_elas[2L, ])

    list("CIs.inf" = bootq1,
         "CIs.sup" = bootq2,
         "conservative" = args$Tstat < args$thres.psi,
         "CI.settings" = list("thres.psi" = args$thres.psi,
                              "n.boot" = args$n.boot))
  })

  expect_equal(withr::with_seed(2345L, do.call(".bootFunc", args)),
               expected)

  args <- list("mu1" = c(0.1, 0.2, 0.3),
               "mu2" = c(0.2, 0.3, 0.4),
               "c.gamma" = c(2, 2, 2),
               "V.rt" = matrix(0.1, 3, 3),
               "sqrt.V.rt_eff" = matrix(0.2, 3, 3),
               "sqrt.V.eff" = matrix(0.3, 3, 3,),
               "psi" = matrix(withr::with_seed(1234, runif(12)), 4, 3),
               "ve" = matrix(0.1, 4, 3),
               "psi.elastic" = c(0.2, 0.25, 0.3),
               "n.rwe" = 100L,
               "n.boot" = 10L,
               "thres.psi" = 0.5,
               "Tstat" = 1)

  expected <- withr::with_seed(2345L, {
    bootq1 <- args$psi - stats::qnorm(0.975) * sqrt(args$ve)
    bootq2 <- args$psi + stats::qnorm(0.975) * sqrt(args$ve)

    boot_elas <- .ciAbove(V.rt = args$V.rt, n.rwe = args$n.rwe, n.boot = args$n.boot)

    bootq1 <- rbind(bootq1, "elastic" = args$psi.elastic + boot_elas[1L, ])
    bootq2 <- rbind(bootq2, "elastic" = args$psi.elastic + boot_elas[2L, ])

    list("CIs.inf" = bootq1,
         "CIs.sup" = bootq2,
         "conservative" = args$Tstat < args$thres.psi,
         "CI.settings" = list("thres.psi" = args$thres.psi,
                              "n.boot" = args$n.boot))
  })

  expect_equal(withr::with_seed(2345L, do.call(".bootFunc", args)),
               expected)

})
