test_that("`.bias()` returns expected errors", {

    expect_error(.bias(),
                 "`Icomb` must be a numeric vector")
    expect_error(.bias(Icomb = "a"),
                 "`Icomb` must be a numeric vector")
    expect_error(.bias(Icomb = matrix(1, 10, 1)),
                 "`Icomb` must be a numeric vector")
    expect_error(.bias(Icomb = matrix(1, 1, 10)),
                 "`Icomb` must be a numeric vector")
    expect_error(.bias(Icomb = c(1.0, 2.0)),
                 "`Icomb` must be a numeric vector")

    expect_error(.bias(Icomb = 1),
                 "`psi.rt` must be a numeric vector")
    expect_error(.bias(Icomb = 1, psi.rt = "a"),
                 "`psi.rt` must be a numeric vector")
    expect_error(.bias(Icomb = 1, psi.rt = matrix(1, 3, 1)),
                 "`psi.rt` must be a numeric vector")
    expect_error(.bias(Icomb = 1, psi.rt = matrix(1, 1, 3)),
                 "`psi.rt` must be a numeric vector")

    expect_error(.bias(Icomb = 1, psi.rt = 1:3),
                 "`psi.eff` must be a numeric vector")
    expect_error(.bias(Icomb = 1, psi.rt = 1:3, psi.eff = "a"),
                 "`psi.eff` must be a numeric vector")
    expect_error(.bias(Icomb = 1, psi.rt = 1:3, psi.eff = matrix(1, 3, 1)),
                 "`psi.eff` must be a numeric vector")
    expect_error(.bias(Icomb = 1, psi.rt = 1:3, psi.eff = matrix(1, 1, 3)),
                 "`psi.eff` must be a numeric vector")
    expect_error(.bias(Icomb = 1, psi.rt = 1:3, psi.eff = 1:4),
                 "`psi.eff` must be a numeric vector")

    expect_error(.bias(Icomb = 1, psi.rt = 1:3, psi.eff = 2:4),
                 "`mu1` must be a numeric vector")
    expect_error(.bias(Icomb = 1, psi.rt = 1:3, psi.eff = 2:4, mu1 = "a"),
                 "`mu1` must be a numeric vector")
    expect_error(.bias(Icomb = 1, psi.rt = 1:3, psi.eff = 2:4, mu1 = matrix(1, 3, 1)),
                 "`mu1` must be a numeric vector")
    expect_error(.bias(Icomb = 1, psi.rt = 1:3, psi.eff = 2:4, mu1 = matrix(1, 1, 3)),
                 "`mu1` must be a numeric vector")
    expect_error(.bias(Icomb = 1, psi.rt = 1:3, psi.eff = 2:4, mu1 = 1:4),
                 "`mu1` must be a numeric vector")

    expect_error(.bias(Icomb = 1, psi.rt = 1:3, psi.eff = 2:4,
                       mu1 = c(0.1, 0.2, 0.3)),
                 "`gamma` must be a numeric vector")
    expect_error(.bias(Icomb = 1, psi.rt = 1:3, psi.eff = 2:4,
                       mu1 = c(0.1, 0.2, 0.3), gamma = "a"),
                 "`gamma` must be a numeric vector")
    expect_error(.bias(Icomb = 1, psi.rt = 1:3, psi.eff = 2:4,
                       mu1 = c(0.1, 0.2, 0.3), gamma = matrix(1, 3, 1)),
                 "`gamma` must be a numeric vector")
    expect_error(.bias(Icomb = 1, psi.rt = 1:3, psi.eff = 2:4,
                       mu1 = c(0.1, 0.2, 0.3), gamma = matrix(1, 1, 3)),
                 "`gamma` must be a numeric vector")
    expect_error(.bias(Icomb = 1, psi.rt = 1:3, psi.eff = 2:4,
                       mu1 = c(0.1, 0.2, 0.3), gamma = 1:4),
                 "`gamma` must be a numeric vector")

    expect_error(.bias(Icomb = 1, psi.rt = 1:3, psi.eff = 2:4,
                       mu1 = c(0.1, 0.2, 0.3), gamma = 0.2),
                 "`eta` must be a numeric vector")
    expect_error(.bias(Icomb = 1, psi.rt = 1:3, psi.eff = 2:4,
                       mu1 = c(0.1, 0.2, 0.3), gamma = 0.2,
                       eta = "a"),
                 "`eta` must be a numeric vector")
    expect_error(.bias(Icomb = 1, psi.rt = 1:3, psi.eff = 2:4,
                       mu1 = c(0.1, 0.2, 0.3), gamma = 0.2,
                       eta = matrix(1, 3, 1)),
                 "`eta` must be a numeric vector")
    expect_error(.bias(Icomb = 1, psi.rt = 1:3, psi.eff = 2:4,
                       mu1 = c(0.1, 0.2, 0.3), gamma = 0.2,
                       eta = matrix(1, 1, 3)),
                 "`eta` must be a numeric vector")
    expect_error(.bias(Icomb = 1, psi.rt = 1:3, psi.eff = 2:4,
                       mu1 = c(0.1, 0.2, 0.3), gamma = 0.2,
                       eta = 1:4),
                 "`eta` must be a numeric vector")

    expect_error(.bias(Icomb = 1, psi.rt = 1:3, psi.eff = 2:4,
                       mu1 = c(0.1, 0.2, 0.3), gamma = 0.2,
                       eta = c(0.5, 1.0, 1.5)),
                 "`V.eff` must be a numeric matrix")
    expect_error(.bias(Icomb = 1, psi.rt = 1:3, psi.eff = 2:4,
                       mu1 = c(0.1, 0.2, 0.3), gamma = 0.2,
                       eta = c(0.5, 1.0, 1.5), V.eff = "a"),
                 "`V.eff` must be a numeric matrix")
    expect_error(.bias(Icomb = 1, psi.rt = 1:3, psi.eff = 2:4,
                       mu1 = c(0.1, 0.2, 0.3), gamma = 0.2,
                       eta = c(0.5, 1.0, 1.5), V.eff = numeric(9)),
                 "`V.eff` must be a numeric matrix")
    expect_error(.bias(Icomb = 1, psi.rt = 1:3, psi.eff = 2:4,
                       mu1 = c(0.1, 0.2, 0.3), gamma = 0.2,
                       eta = c(0.5, 1.0, 1.5), V.eff = matrix(1, 4, 3)),
                 "`V.eff` must be a numeric matrix")
    expect_error(.bias(Icomb = 1, psi.rt = 1:3, psi.eff = 2:4,
                       mu1 = c(0.1, 0.2, 0.3), gamma = 0.2,
                       eta = c(0.5, 1.0, 1.5), V.eff = matrix(1, 3, 4)),
                 "`V.eff` must be a numeric matrix")
    expect_error(.bias(Icomb = 1, psi.rt = 1:3, psi.eff = 2:4,
                       mu1 = c(0.1, 0.2, 0.3), gamma = 0.2,
                       eta = c(0.5, 1.0, 1.5), V.eff = matrix("a", 3, 3)),
                 "`V.eff` must be a numeric matrix")

    expect_error(.bias(Icomb = 1, psi.rt = 1:3, psi.eff = 2:4,
                       mu1 = c(0.1, 0.2, 0.3), gamma = 0.2,
                       eta = c(0.5, 1.0, 1.5), V.eff = matrix(0.1, 3L, 3L)),
                 "`n.rwe` must be an integer")
    expect_error(.bias(Icomb = 1, psi.rt = 1:3, psi.eff = 2:4,
                       mu1 = c(0.1, 0.2, 0.3), gamma = 0.2,
                       eta = c(0.5, 1.0, 1.5), V.eff = matrix(0.1, 3L, 3L),
                       n.rwe = "a"),
                 "`n.rwe` must be an integer")
    expect_error(.bias(Icomb = 1, psi.rt = 1:3, psi.eff = 2:4,
                       mu1 = c(0.1, 0.2, 0.3), gamma = 0.2,
                       eta = c(0.5, 1.0, 1.5), V.eff = matrix(0.1, 3L, 3L),
                       n.rwe = 1.5),
                 "`n.rwe` must be an integer")
    expect_error(.bias(Icomb = 1, psi.rt = 1:3, psi.eff = 2:4,
                       mu1 = c(0.1, 0.2, 0.3), gamma = 0.2,
                       eta = c(0.5, 1.0, 1.5), V.eff = matrix(0.1, 3L, 3L),
                       n.rwe = 1L:2L),
                 "`n.rwe` must be an integer")
    expect_error(.bias(Icomb = 1, psi.rt = 1:3, psi.eff = 2:4,
                       mu1 = c(0.1, 0.2, 0.3), gamma = 0.2,
                       eta = c(0.5, 1.0, 1.5), V.eff = matrix(0.1, 3L, 3L),
                       n.rwe = 0L),
                 "`n.rwe` must be an integer")

})

test_that("`.bias()` return expected results", {

  args <- list(Icomb = c(0.15),
               psi.rt = 1:3,
               psi.eff = 2:4,
               mu1 = c(0.1, 0.2, 0.3),
               gamma = 0.2,
               eta = c(0.5, 1.0, 1.5),
               V.eff = matrix(0.1, 3L, 3L),
               n.rwe = 100L)

  est_elas <- c("elastic.1" = 1.15,
                "elastic.2" = 2.15,
                "elastic.3" = 3.15)

  expected <- est_elas

  expect_equal(do.call(".bias", args), expected)

})

test_that("`.bias()` return expected results", {

  args <- list(Icomb = c(0.15),
               psi.rt = 1,
               psi.eff = 2,
               mu1 = c(0.1),
               gamma = c(0.2),
               eta = c(0.5),
               V.eff = matrix(0.1, 1L, 1L),
               n.rwe = 100L)

  est_elas <- c("elastic.1" = 1.15)

  expected <- est_elas

  expect_equal(do.call(".bias", args), expected)

})
