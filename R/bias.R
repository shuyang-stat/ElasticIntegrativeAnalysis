#' Elastic Parameter Estimates
#'
#' Elastic parameter estimates and the bias adjusted estimates
#'
#' @noRd
#' @param Icomb A numeric vector. Indicator I(c_gamma > T)
#' @param psi.rt A numeric vector. The parameters estimated using rct data.
#' @param psi.eff A numeric vector. The efficient parameters estimated using
#'   combined data.
#' @param mu1 A numeric vector. Length p. The mean of Z_1.
#' @param gamma A numeric vector. The selected gamma.
#' @param eta A numeric vector. The weighted score.
#' @param V.eff A numeric matrix. The variance of psi.eff
#' @param n.rwe An integer. The number of participants in RWE
#'
#' @returns A named numeric vector - the estimated elastic parameters
#'
#' @importFrom stats pchisq qchisq
#' @keywords internal
.bias <- function(Icomb, psi.rt, psi.eff,
                  mu1, gamma, eta,
                  V.eff, n.rwe) {

  stopifnot(
    "`Icomb` must be a numeric vector" = !missing(Icomb) &&
      .isNumericVector(Icomb, 1L),
    "`psi.rt` must be a numeric vector" = !missing(psi.rt) &&
      .isNumericVector(psi.rt),
    "`psi.eff` must be a numeric vector" = !missing(psi.eff) &&
      .isNumericVector(psi.eff, length(psi.rt)),
    "`mu1` must be a numeric vector" = !missing(mu1) &&
      .isNumericVector(mu1, length(psi.rt)),
    "`gamma` must be a numeric vector" = !missing(gamma) &&
      .isNumericVector(gamma, 1L),
    "`eta` must be a numeric vector" = !missing(eta) &&
      .isNumericVector(eta, length(psi.rt)),
    "`V.eff` must be a numeric matrix" = !missing(V.eff) && is.matrix(V.eff) &&
      is.numeric(V.eff) && nrow(V.eff) == ncol(V.eff) && nrow(V.eff) == length(psi.rt),
    "`n.rwe` must be an integer" = !missing(n.rwe) && .isNumericVector(n.rwe, 1L) &&
      isTRUE(all.equal(n.rwe, round(n.rwe, 0))) && n.rwe > 0
  )

  n_cov <- length(mu1)

  est_elas <- (1.0 - Icomb) * psi.rt + Icomb * psi.eff
  names(est_elas) <- paste0("elastic.", seq_len(n_cov))

  # expected bias for the elastic estimator
  biastemp <- -stats::pchisq(stats::qchisq(1.0 - gamma, df = n_cov),
                            df = n_cov + 2L, ncp = crossprod(mu1)) * V.eff %*% eta

  est_elas
}
