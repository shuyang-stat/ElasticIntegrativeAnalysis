#' Calculate Sigma Matrix
#'
#' @noRd
#' @param V.rt A numeric matrix. The variance of psi_rt.
#' @param V.eff A numeric matrix. The variance of the psi_eff.
#' @param rho A numeric object. The ratio of the number of participants
#'   n.rct / n.rwe.
#'
#' @returns A list object containing p x p matrices inv.Sigma.SS,
#'   sqrt.inv.Sigma.SS, and sqrt.Sigma.SS
#'
#' @importFrom MASS ginv
#' @importFrom expm sqrtm
#' @keywords internal
.calculateSigmaSSMatrices <- function(V.rt, V.eff, rho) {

  stopifnot(
    "`V.rt` must be a numeric matrix" = !missing(V.rt) &&
      .isNumericMatrix(V.rt) && nrow(V.rt) == ncol(V.rt),
    "`V.eff` must be a numeric matrix" = !missing(V.eff) &&
      .isNumericMatrix(V.eff) && nrow(V.eff) == ncol(V.eff),
    "`V.rt` and `V.eff` must be of the same dimensions" = all(dim(V.rt) == dim(V.eff)),
    "`rho` must be a scalar numeric in [0, 1]" = !missing(rho) &&
      .isNumericVector(rho, 1L) && rho > 0.0 && rho <= 1.0
  )

  # Fisher information matrix for S_rt
  # {p x p}
  I_rt <- tryCatch(MASS::ginv(V.rt) / rho,
                   error = function(e) {
                     stop("unable to invert variance matrix of efficient.RCT\n\t",
                          e$message, call. = FALSE)
                   })

  # Fisher information matrix for S_rw
  # {p x p}
  I_rw <- tryCatch(MASS::ginv(V.eff) - I_rt * rho,
                   error = function(e) {
                     stop("unable to invert variance matrix of efficient.integ\n\t",
                          e$message, call. = FALSE)
                   })

  # Inverse of Fisher information matrix for S_rt
  # {p x p}
  inv_I_rt <- tryCatch(MASS::ginv(I_rt),
                       error = function(e) {
                         stop("unable to invert Fisher information matrix for S_rt\n\t",
                              e$message, call. = FALSE)
                       })

  # {p x p}
  Gamma <-  inv_I_rt %*% I_rw / sqrt(rho)

  # {p x p}
  Sigma_SS <- crossprod(Gamma, I_rt %*% Gamma) + I_rw

  sqrt_Sigma_SS <- tryCatch(expm::sqrtm(Sigma_SS),
                            error = function(e) {
                              stop("unable to obtain square root of Sigma_SS\n\t",
                                   e$message, call. = FALSE)
                              })

  if (any(abs(Im(sqrt_Sigma_SS)) > 1e-6)) {
    warning("sqrtm returned complex matrix; only real component used", call. = FALSE)
  }
  sqrt_Sigma_SS <- Re(sqrt_Sigma_SS)

  inv_Sigma_SS <- tryCatch(MASS::ginv(Sigma_SS),
                           error = function(e) {
                             stop("unable to invert Sigma_SS matrix\n\t",
                                  e$message, call. = FALSE)
                           })

  sqrt_inv_Sigma_SS <- tryCatch(expm::sqrtm(inv_Sigma_SS),
                                error = function(e) {
                                  stop("unable to obtain square root of Sigma_SS inverse\n\t",
                                       e$message, call. = FALSE)
                                })

  if (any(abs(Im(sqrt_inv_Sigma_SS)) > 1e-6)) {
    warning("sqrtm returned complex matrix; only real component used", call. = FALSE)
  }
  sqrt_inv_Sigma_SS <- Re(sqrt_inv_Sigma_SS)

  list("sqrt.Sigma.SS" = sqrt_Sigma_SS,
       "inv.Sigma.SS" = inv_Sigma_SS,
       "sqrt.inv.Sigma.SS" = sqrt_inv_Sigma_SS)
}
