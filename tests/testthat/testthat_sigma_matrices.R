test_that("`.calculateSigmaSSMatrices()` returns expected errors", {

  expect_error(.calculateSigmaSSMatrices(), "`V.rt` must be a numeric matrix")
  expect_error(.calculateSigmaSSMatrices(V.rt = "a"),
               "`V.rt` must be a numeric matrix")
  expect_error(.calculateSigmaSSMatrices(V.rt = matrix("a", 3, 3)),
               "`V.rt` must be a numeric matrix")
  expect_error(.calculateSigmaSSMatrices(V.rt = matrix(1, 2, 3)),
               "`V.rt` must be a numeric matrix")
  expect_error(.calculateSigmaSSMatrices(V.rt = matrix(1, 3, 2)),
               "`V.rt` must be a numeric matrix")
  expect_error(.calculateSigmaSSMatrices(V.rt = 1:3),
               "`V.rt` must be a numeric matrix")

  expect_error(.calculateSigmaSSMatrices(V.rt = matrix(1, 3, 3)),
               "`V.eff` must be a numeric matrix")
  expect_error(.calculateSigmaSSMatrices(V.rt = matrix(1, 3, 3), V.eff = "a"),
               "`V.eff` must be a numeric matrix")
  expect_error(.calculateSigmaSSMatrices(V.rt = matrix(1, 3, 3), V.eff = matrix("a", 3, 3)),
               "`V.eff` must be a numeric matrix")
  expect_error(.calculateSigmaSSMatrices(V.rt = matrix(1, 3, 3), V.eff = matrix(1, 2, 3)),
               "`V.eff` must be a numeric matrix")
  expect_error(.calculateSigmaSSMatrices(V.rt = matrix(1, 3, 3), V.eff = matrix(1, 3, 2)),
               "`V.eff` must be a numeric matrix")
  expect_error(.calculateSigmaSSMatrices(V.rt = matrix(1, 3, 3), V.eff = 1:3),
               "`V.eff` must be a numeric matrix")

  expect_error(.calculateSigmaSSMatrices(V.rt = matrix(1, 3, 3), V.eff = matrix(1, 3, 3)),
               "`rho` must be a scalar numeric in [0, 1]", fixed = TRUE)
  expect_error(.calculateSigmaSSMatrices(V.rt = matrix(1, 3, 3),
                                         V.eff = matrix(1, 3, 3), rho = "a"),
               "`rho` must be a scalar numeric in [0, 1]", fixed = TRUE)
  expect_error(.calculateSigmaSSMatrices(V.rt = matrix(1, 3, 3),
                                         V.eff = matrix(1, 3, 3), rho = 1.000001),
               "`rho` must be a scalar numeric in [0, 1]", fixed = TRUE)
  expect_error(.calculateSigmaSSMatrices(V.rt = matrix(1, 3, 3),
                                         V.eff = matrix(1, 3, 3), rho = -0.000001),
               "`rho` must be a scalar numeric in [0, 1]", fixed = TRUE)

})

test_that("`.calculateSigmaSSMatrices()` returns expected results", {
  V.rt <- matrix(1.0, 3, 3)
  V.eff <- matrix(0.5, 3, 3)
  rho <- 0.5

  I_rt <- matrix(0.1111111111111, 3, 3) / 0.5
  I_rw <- matrix(0.1111111111111, 3, 3)
  inv_I_rt <- matrix(0.5, 3, 3)
  # {p x p}
  Gamma <-  matrix(0.166666666666667, 3, 3) / sqrt(rho)
  Sigma_SS <- matrix(0.2222222222222, 3, 3)
  sqrt_Sigma_SS <- matrix(0.27216555, 3, 3)
  inv_Sigma_SS <- matrix(0.5, 3, 3)
  sqrt_inv_Sigma_SS <- matrix(0.4082483, 3, 3)

  expected <- list("sqrt.Sigma.SS" = sqrt_Sigma_SS,
                   "inv.Sigma.SS" = inv_Sigma_SS,
                   "sqrt.inv.Sigma.SS" = sqrt_inv_Sigma_SS)

  expect_equal(.calculateSigmaSSMatrices(V.rt, V.eff, rho), expected, tolerance = 1e-6)

  expect_warning(.calculateSigmaSSMatrices(matrix(-1, 3, 3), matrix(0.5, 3, 3), 0.4),
                 "sqrtm returned complex matrix; only real component used")

  expect_error(.calculateSigmaSSMatrices(matrix(1, 3, 3), matrix(1.0, 3, 3), 0.4),
               "unable to obtain square root of Sigma_SS\n\t")

  expect_error(.calculateSigmaSSMatrices(matrix(NA_real_, 3, 3), matrix(1.0, 3, 3), 0.4),
               "unable to invert variance matrix of efficient.RCT\n\t")

})
