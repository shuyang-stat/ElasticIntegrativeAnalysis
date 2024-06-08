#' Generate Elastic
#'
#' @noRd
#' @param mu1 A numeric vector. Length p. The mean of Z1.
#' @param mu2 A numeric vector. Length p. The mean of Z2.
#' @param n.boot An integer. The number of bootstrap samples to generate.
#' @param c.gamma A scalar numeric. The threshold corresponding to the selected
#'   gamma.
#' @param sqrt.V.rt_eff A numeric matrix. Dim p x p. Square root of the
#'   difference between the variance of psi.rt and the variance of psi.eff
#' @param sqrt.V.eff A numeric matrix. Dim p x p. Square root of the
#'   variance of psi.eff estimated using perturbation-based resampling.
#'
#' @returns An n.boot x p matrix.
#'
#' @importFrom mvtnorm rmvnorm
#' @keywords internal
.generateElastic <- function(mu1, mu2, n.boot, c.gamma,
                             sqrt.V.rt_eff, sqrt.V.eff) {
  stopifnot(
    "`mu1` must be a numeric vector" = !missing(mu1) &&
      .isNumericVector(mu1),
    "`mu2` must be a numeric vector" = !missing(mu2) &&
      .isNumericVector(mu2, length(mu1)),
    "`n.boot` must be an integer" = !missing(n.boot) && .isNumericVector(n.boot, 1L) &&
      isTRUE(all.equal(n.boot, round(n.boot, 0))) && n.boot > 0,
    "`c.gamma` must be a scalar numeric" = !missing(c.gamma) &&
      .isNumericVector(c.gamma, 1L),
    "`sqrt.V.rt_eff` must be a numeric matrix" = !missing(sqrt.V.rt_eff) &&
      is.matrix(sqrt.V.rt_eff) &&
      is.numeric(sqrt.V.rt_eff) && nrow(sqrt.V.rt_eff) == ncol(sqrt.V.rt_eff) &&
      nrow(sqrt.V.rt_eff) == length(mu1),
    "`sqrt.V.eff` must be a numeric matrix" = !missing(sqrt.V.eff) &&
      is.matrix(sqrt.V.eff) &&
      is.numeric(sqrt.V.eff) && nrow(sqrt.V.eff) == ncol(sqrt.V.eff) &&
      nrow(sqrt.V.eff) == length(mu1)
  )

  n_cov <- length(mu1)

  # p x n.boot
  z0 <- mvtnorm::rmvnorm(n.boot, mean = rep(0.0, n_cov), diag(n_cov)) |> t()
  zz0 <- mvtnorm::rmvnorm(n.boot, mean = rep(0.0, n_cov), diag(n_cov)) |> t()

  # p x n.boot
  z1 <- z0 + mu1
  z2 <- zz0 + mu2

  # z1^T z1 for each n.boot
  z1tz1 <- colSums(z1 * z1)

  # p x n.boot
  mat_yes <- outer(z1tz1, rep(c.gamma, n_cov), FUN = "<") |> t()

  # p x n.boot
  z_eff <- -sqrt.V.eff %*% z2 # accept
  z_rt <- sqrt.V.rt_eff %*% z1 + z_eff  #rej

  {z_eff * mat_yes  + z_rt * (1.0 - mat_yes)} |> t()
}

#' Bootstrap Confidence Intervals T-statistic Less Than Threshold
#'
#' @noRd
#' @param mu1 A numeric vector. Length p. The mean of Z1.
#' @param mu2 A numeric vector. Length p. The mean of Z1.
#' @param n.boot An integer. The number of samples to generate.
#' @param c.gamma A scalar numeric. The threshold corresponding to the selected
#'   gamma.
#' @param sqrt.V.rt_eff A numeric matrix. Dim p x p. Square root of the
#'   difference between the variance of psi.rt and the variance of psi.eff.
#' @param sqrt.V.eff A numeric matrix. Dim p x p. Square root of the variance
#'   of psi.eff.
#'
#' @return A matrix of CI
#'
#' @keywords internal
.boot_func_step <- function(mu1, mu2, n.boot, c.gamma,
                            sqrt.V.rt_eff, sqrt.V.eff) {
  stopifnot(
    "`mu1` must be provided" = !missing(mu1),
    "`mu2` must be provided" = !missing(mu2),
    "`n.boot` must be provided" = !missing(n.boot),
    "`c.gamma` must be provided" = !missing(c.gamma),
    "`sqrt.V.rt_eff` must be provided" = !missing(sqrt.V.rt_eff),
    "`sqrt.V.eff` must be provided" = !missing(sqrt.V.eff)
  )

  # n.boot x p
  gen_value <- .generateElastic(mu1 = mu1, mu2 = mu2, n.boot = n.boot,
                                c.gamma = c.gamma,
                                sqrt.V.rt_eff = sqrt.V.rt_eff,
                                sqrt.V.eff = sqrt.V.eff)
  # 2 x n_cov matrix
  apply(gen_value, MARGIN = 2L, FUN = stats::quantile,
        probs = c(0.0125, 0.9875), type = 5L, na.rm = TRUE)
}

#' Bootstrap Confidence Intervals T-statistic Less Than Threshold
#'
#' @noRd
#' @param mu1 A numeric vector. Length p. The mean of Z1.
#' @param mu2 A numeric vector. Length p. The mean of Z2.
#' @param c.gamma A scalar numeric. The threshold corresponding to the selected
#'   gamma.
#' @param sqrt.V.rt_eff A numeric matrix. Dim p x p. Square root of the
#'   difference between the variance of psi.rt and the variance of psi.eff.
#' @param sqrt.V.eff A numeric matrix. Dim p x p. Square root of the
#'   variance of psi.eff.
#' @param n.rwe An integer. The number of participants in the RWE dataset.
#' @param n.boot An integer. The number of bootstrap samples to generate
#'
#' @return A 2 x p matrix.
#'
#' @keywords internal
.ciBelow <- function(mu1, mu2,
                     c.gamma,
                     sqrt.V.rt_eff,
                     sqrt.V.eff,
                     n.rwe, n.boot) {

  stopifnot(
    "`mu1` must be a numeric vector" = !missing(mu1) &&
      .isNumericVector(mu1),
    "`mu2` must be a numeric vector" = !missing(mu2) &&
      .isNumericVector(mu2, length(mu1)),
    "`c.gamma` must be a scalar numeric" = !missing(c.gamma) &&
      .isNumericVector(c.gamma, 1L),
    "`sqrt.V.rt_eff` must be provided" = !missing(sqrt.V.rt_eff),
    "`sqrt.V.eff` must be provided" = !missing(sqrt.V.eff),
    "`n.rwe` must be an integer" = !missing(n.rwe) && .isNumericVector(n.rwe, 1L) &&
      isTRUE(all.equal(n.rwe, round(n.rwe, 0))) && n.rwe > 0,
    "`n.boot` must be an integer" = !missing(n.boot) && .isNumericVector(n.boot, 1L) &&
      isTRUE(all.equal(n.boot, round(n.boot, 0))) && n.boot > 0
  )

  n_cov <- length(mu1)
  # numeric vector length p
  UU_mu1 <- rep(mvtnorm::qmvnorm(0.975, mean = mu1, sigma = diag(n_cov),
                                 tail = "both.tails")$quantile,
                n_cov)
  LL_mu1 <- -UU_mu1

  # simulation for elastic inference
  qq1 <- matrix(NA_real_, n.boot, n_cov)
  qq2 <- matrix(NA_real_, n.boot, n_cov)

  for (i in seq_len(n.boot)) {

    # p
    mu1_new <- mvtnorm::rmvnorm(1L, mean = mu1, sigma = diag(n_cov)) |> drop()
    # if sample is not in the truncated normal, resample
    while (any(mu1_new > UU_mu1 | mu1_new < LL_mu1)) {
      mu1_new <- mvtnorm::rmvnorm(1, mean = mu1, sigma = diag(n_cov)) |> drop()
    }

    res <- .boot_func_step(mu1 = mu1_new, mu2 = mu2, n.boot = n.boot,
                           c.gamma = c.gamma,
                           sqrt.V.rt_eff = sqrt.V.rt_eff,
                           sqrt.V.eff = sqrt.V.eff)

    qq1[i, ] <- res[1L, ]
    qq2[i, ] <- res[2L, ]
  }

  rbind(apply(qq1, 2L, min) / sqrt(n.rwe),
        apply(qq2, 2L, max) / sqrt(n.rwe))

}

#' Bootstrap Confidence Intervals T-statistic Greater Than Threshold
#'
#' @noRd
#' @param V.rt A matrix. The estimates variance of psi.rt.
#' @param n.rwe An integer. The number of participants in the RWE dataset.
#' @param n.boot An integer. The number of bootstrap samples to generate
#'
#' @return A list containing elas1 and elas2
#'
#' @importFrom mvtnorm rmvnorm
#' @keywords internal
.ciAbove <- function(V.rt, n.rwe, n.boot) {

  stopifnot(
    "`V.rt` must be a numeric matrix" = !missing(V.rt) &&
      is.matrix(V.rt) && is.numeric(V.rt) && nrow(V.rt) == ncol(V.rt),
    "`n.rwe` must be an integer" = !missing(n.rwe) && .isNumericVector(n.rwe, 1L) &&
      isTRUE(all.equal(n.rwe, round(n.rwe, 0))) && n.rwe > 0,
    "`n.boot` must be an integer" = !missing(n.boot) && .isNumericVector(n.boot, 1L) &&
      isTRUE(all.equal(n.boot, round(n.boot, 0))) && n.boot > 0
  )

  n_cov <- ncol(V.rt)

  # 1000 x n_cov
  z_rt <- mvtnorm::rmvnorm(n.boot, mean = rep(0.0, n_cov), sigma = V.rt)

  apply(z_rt, 2L,
        FUN = stats::quantile,
        probs = c(0.025, 0.975),
        type = 5L, na.rm = TRUE) / sqrt(n.rwe)
}

#' Bootstrap Confidence Intervals
#'
#' @noRd
#' @param mu1 A numeric vector. Length p. The mean of Z_1.
#' @param mu2 A numeric vector. Length p. The mean of Z_2.
#' @param c.gamma A scalar numeric. The threshold corresponding to the selected
#'   gamma.
#' @param V.rt A numeric matrix. Dim p x p. Variance of psi.rt currently
#'   estimated using perturbation-based resampling.
#' @param sqrt.V.rt_eff A numeric matrix. Dim p x p. Square of the difference
#'   between the variance of psi.rt and the variance of psi.eff
#' @param sqrt.V.eff A numeric matrix. Dim p x p. Square-root of the variance
#'   of psi.eff estimated using perturbation-based resampling.
#' @param psi A numeric matrix. Dim 5 x p. The various estimates of psi.
#' @param ve A numeric matrix. Dim 5 x p. The variance of each estimated psi.
#' @param psi.elastic A numeric vector. Length p. The estimated elastic psi.
#' @param n.rwe An integer. The number of participants in the RWE dataset.
#' @param n.boot An integer. The number of bootstrap samples to generate.
#' @param thres.psi A scalar numeric. The testing threshold for accept/reject.
#' @param Tstat A scalar numeric. The test statistics.
#'
#' @return A list containing CIs.inf, the standard and infimum CI;
#'   CIs.sup, the standard and supremum CI; setting indicating the basic
#'   settings provided for the estimator; and conservative, a logical --
#'   if TRUE, CI were estimated using the local alternative strategy; otherwise
#'   CI were estimated using the fixed alternative;
#'
#' @importFrom stats qnorm
#' @keywords internal
.bootFunc <- function(mu1, mu2,
                      c.gamma,
                      V.rt,
                      sqrt.V.rt_eff, sqrt.V.eff,
                      psi, ve,
                      psi.elastic,
                      n.rwe, n.boot,
                      thres.psi, Tstat) {

  stopifnot(
    "`mu1` must be a numeric vector" = !missing(mu1) &&
      .isNumericVector(mu1),
    "`mu2` must be provided" = !missing(mu2),
    "`c.gamma` must be provided" = !missing(c.gamma),
    "`V.rt` must be a numeric matrix" = !missing(V.rt) &&
      is.matrix(V.rt) && is.numeric(V.rt) && nrow(V.rt) == ncol(V.rt) &&
      nrow(V.rt) == length(mu1),
    "`sqrt.V.rt_eff` must be provided" = !missing(sqrt.V.rt_eff),
    "`sqrt.V.eff` must be provided" = !missing(sqrt.V.eff),
    "`psi` must be a numeric matrix" = !missing(psi) && is.matrix(psi) &&
      is.numeric(psi) && ncol(psi) == length(mu1),
    "`ve` must be a numeric matrix" = !missing(ve) &&
      is.matrix(ve) && is.numeric(ve) && all(dim(ve) == dim(psi)),
    "`psi.elastic` must be a numeric vector" = !missing(psi.elastic) &&
      .isNumericVector(psi.elastic, length(mu1)),
    "`n.rwe` must be provided" = !missing(n.rwe),
    "`n.boot` must be an integer" = !missing(n.boot) && .isNumericVector(n.boot, 1L) &&
      isTRUE(all.equal(n.boot, round(n.boot, 0))) && n.boot > 0,
    "`thres.psi` must be a numeric" = !missing(thres.psi) &&
      .isNumericVector(thres.psi, 1L),
    "`Tstat` must be a numeric" = !missing(Tstat) && .isNumericVector(Tstat, 1L)
  )

  n_cov <- length(mu1)

  # standard confidence intervals
  bootq1 <- psi - stats::qnorm(0.975) * sqrt(ve)
  bootq2 <- psi + stats::qnorm(0.975) * sqrt(ve)

  boot_elas <- if (Tstat < thres.psi) {
                 .ciBelow(mu1 = mu1, mu2 = mu2,
                          c.gamma = c.gamma,
                          sqrt.V.rt_eff = sqrt.V.rt_eff,
                          sqrt.V.eff = sqrt.V.eff,
                          n.rwe = n.rwe, n.boot = n.boot)
               } else {
                 .ciAbove(V.rt = V.rt, n.rwe = n.rwe, n.boot = n.boot)
               }

  bootq1 <- rbind(bootq1, "elastic" = psi.elastic + boot_elas[1L, ])
  bootq2 <- rbind(bootq2, "elastic" = psi.elastic + boot_elas[2L, ])

  list("CIs.inf" = bootq1,
       "CIs.sup" = bootq2,
       "conservative" = Tstat < thres.psi,
       "CI.settings" = list("thres.psi" = thres.psi,
                            "n.boot" = n.boot))

}
