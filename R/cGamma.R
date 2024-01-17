#' @noRd
#' @param z1tz1 A numeric vector. The crossproduct of each generated z1. (ngen)
#' @param n.rt A numeric vector. (ngen)
#' @param n.eff A numeric vector. (ngen)
#' @param n.cov An integer. The number of covariates in the model.
#'
#' @returns A list containing
#'   "gamma", a scalar numeric - the gamma that minimizes the MSE of the intercept,
#'   "c_gamma", a scalar numeric - the threshold corresponding the gamma
#' @keywords internal
.chooseGamma <- function(z1tz1, n.rt, n.eff, n.cov) {

  stopifnot(
    "`z1tz1` must be a numeric vector" = !missing(z1tz1) && .isNumericVector(z1tz1),
    "`n.rt` must be a numeric vector" = !missing(n.rt) &&
      .isNumericVector(n.rt, length(z1tz1)),
    "`n.eff` must be a numeric vector" = !missing(n.eff) &&
      .isNumericVector(n.eff, length(z1tz1)),
    "`n.cov` must be a positive integer" = !missing(n.cov) &&
      .isIntegerVector(n.cov, 1L) && n.cov > 0L
  )

  n_gen <- length(n.rt)

  # setup the gamma candidates
  allgamma <- seq(1.0 - 1.0e-10 , 1.0e-10, length.out = 50L)

  Lgamma <- length(allgamma)

  critmse <- rep(NA_real_, Lgamma)

  # Lgamma
  qchi <- stats::qchisq(1.0 - allgamma, df = n.cov)

  for (i in seq_len(Lgamma)) {
    mat_yes <- as.numeric(z1tz1 < qchi[i])
    gen_value <- {n.rt * (1.0 - mat_yes) + n.eff * mat_yes}
    critmse[i] <- mean((gen_value)^2, na.rm = TRUE)
  }

  # select the gamma with the lowest MSE
  chosen <- which.min(critmse)

  list("gamma" = allgamma[chosen],
       "c_gamma" = qchi[chosen])

}

#' @noRd
#' @param z1tz1 A numeric vector. The crossproduct of each generated z1. (ngen)
#' @param c.gamma A scalar numeric. The selected threshold.
#' @param n.rt A numeric vector. (ngen)
#' @param n.eff A numeric vector. (ngen)
#'
#' @returns A numeric vector. The variance of psi.elastic
#'
#' @importFrom stats var
#' @keywords internal
.veElastic <- function(z1tz1, c.gamma, n.rt, n.eff, n.rwe) {

  stopifnot(
    "`z1tz1` must be a numeric vector" = !missing(z1tz1) && .isNumericVector(z1tz1),
    "`c.gamma` must be a scalar numeric" = !missing(c.gamma) &&
      .isNumericVector(c.gamma, 1L),
    "`n.rt` must be a numeric matrix" = !missing(n.rt) &&
      .isNumericMatrix(n.rt) && nrow(n.rt) == length(z1tz1),
    "`n.eff` must be a numeric matrix" = !missing(n.eff) &&
      .isNumericMatrix(n.eff) && nrow(n.eff) == length(z1tz1) &&
      ncol(n.eff) == ncol(n.rt),
    "`n.rwe` must be a positive integer" = !missing(n.rwe) &&
      .isIntegerVector(n.rwe, 1L) && n.rwe > 0L
  )

  vec_yes <- as.numeric(z1tz1 < c.gamma)

  gen_value <- {n.eff * vec_yes  + n.rt * (1.0 - vec_yes)}

  apply(gen_value, 2L, stats::var, na.rm = TRUE) / n.rwe
}

#' @noRd
#' @param mu1 A numeric vector. Length p. The mean of Z_1.
#' @param mu2 A numeric vector. Length p. The mean of Z_2.
#' @param n.gamma An integer. The number of samples to generate.
#' @param sqrt.V.rt_eff A numeric matrix. Dim p x p. Square root of the
#'   difference between the variance of psi.rt and the variance of psi.eff.
#' @param sqrt.Veff A numeric matrix. Dim p x p. Square root of the variance
#'   of psi.eff.
#' @param Tstat1 A scalar numeric. The test statistic.
#' @param n.rwe An integer. The number of participants in the RWE dataset.
#' @param fixed A logical.
#'
#' @returns A list object containing:
#' \begin{itemize}
#'   \item{gamma }{A numeric vector. The selected gamma for each covariate.}
#'   \item{c.gamma }{A numeric vector. The threshold corresponding to the selected gamma.}
#'   \item{Icomb }{A numeric vector. I(c.gamma > T)}
#'   \item{Icomb.pval }{A numeric. The smoothed p-value.}
#'   \item{V.elastic }{A numeric matrix. The variance of psi.elastic}
#'
#' @importFrom mvtnorm rmvnorm
#' @importFrom stats qchisq pchisq
#' @keywords internal
.cGamma <- function(mu1, mu2, n.gamma,
                    sqrt.V.rt_eff,
                    sqrt.V.eff,
                    Tstat1,
                    n.rwe, fixed) {

  stopifnot(
    "`mu1` must be a numeric vector" = !missing(mu1) && .isNumericVector(mu1),
    "`mu2` must be a numeric vector" = !missing(mu2) && .isNumericVector(mu2),
    "`mu1` and `mu2` must be of equivalent length" = length(mu1) == length(mu2),
    "`n.gamma` must be a positive integer" = !missing(n.gamma) &&
      .isNumericVector(n.gamma, 1L) && isTRUE(all.equal(n.gamma, round(n.gamma, 0))) &&
      n.gamma > 0,
    "`sqrt.V.rt_eff` must be a numeric matrix" = !missing(sqrt.V.rt_eff) &&
      is.matrix(sqrt.V.rt_eff) && is.numeric(sqrt.V.rt_eff) &&
      all(dim(sqrt.V.rt_eff) == c(length(mu1), length(mu1))),
    "`sqrt.V.eff` must be a numeric matrix" = !missing(sqrt.V.eff) &&
      is.matrix(sqrt.V.eff) && is.numeric(sqrt.V.eff) &&
      all(dim(sqrt.V.eff) == c(length(mu1), length(mu1))),
    "`Tstat1` must be a scalar numeric" = !missing(Tstat1) &&
      .isNumericVector(Tstat1, 1L),
    "`n.rwe` must be a positive scalar" = !missing(n.rwe) &&
      .isNumericVector(n.rwe, 1L) && isTRUE(all.equal(n.rwe, round(n.rwe, 0))) &&
      n.rwe > 0,
    "`fixed` must be a logical" = !missing(fixed) && is.logical(fixed) &&
      is.vector(fixed) && length(fixed) == 1L
  )

  n_cov <- length(mu1)

  # ngen x p
  z1_samples <- mvtnorm::rmvnorm(n.gamma, mean = mu1, diag(n_cov)) # z1
  z2_samples <- mvtnorm::rmvnorm(n.gamma, mean = mu2, diag(n_cov)) # z2

  # ngen x p
  n_eff_samples <- - z2_samples %*% t(sqrt.V.eff) # accept
  n_rt_samples <- z1_samples %*% t(sqrt.V.rt_eff) + n_eff_samples  #rej

  # ngen Z1^T Z1 for each ngen
  z1tz1 <- apply(z1_samples, MARGIN = 1L, FUN = crossprod)

  # select the optimal gamma values (by simulation or analytic form)
  if (fixed) {
    gamma_selected <- list("gamma" = 0.05,
                           "c_gamma" = stats::qchisq(0.95, df = n_cov))
  } else {
    gamma_selected <- .chooseGamma(z1tz1 = z1tz1,
                                   n.rt = n_rt_samples[, 1L],
                                   n.eff = n_eff_samples[, 1L],
                                   n.cov = n_cov)
  }

  # indicator of c_gamma > Tstat1 (take psi.eff)
  Icomb <- as.integer(gamma_selected$c_gamma  > Tstat1)

  # pvalue smoothed
  Icomb_pval <- 1.0 - stats::pchisq(Tstat1, df = n_cov)

  ve_elas <- .veElastic(z1tz1 = z1tz1,
                        c.gamma = gamma_selected$c_gamma,
                        n.rt = n_rt_samples,
                        n.eff = n_eff_samples,
                        n.rwe = n.rwe)

  list("gamma" = gamma_selected$gamma,
       "c.gamma" = gamma_selected$c_gamma,
       "Icomb" = Icomb,
       "Icomb.pval" = Icomb_pval,
       "V.elastic" = ve_elas,
       "settings" = list("n.gamma" = n.gamma,
                         "fixed" = fixed))
}
