#' Heterogeneous Treatment Effect
#'
#' @noRd
#' @param psi A numeric vector. The estimated coefficients.
#' @param X A named numeric matrix. The covariates.
#' @param outcome.type A character. The type of outcome ('cont' or 'bin')
#'
#' @returns A numeric vector.
#'
#' @include stopTests.R
#' @keywords internal
.HTE <- function(psi, X, outcome.type) {

  stopifnot(
    "`psi` must be a named numeric vector" = !missing(psi) &&
      .isNumericVector(psi) && !is.null(names(psi)) &&
      all(nchar(names(psi)) > 0L),
    "`X` must be a named numeric matrix" = !missing(X) &&
      {.isNamedNumericMatrix(X) || ncol(X) == 0L},
    "`outcome.type` must be one of 'cont' or 'bin'" = !missing(outcome.type) &&
      .isCharacterVector(outcome.type, 1L) && outcome.type %in% c("cont", "bin")
  )

  # map psi to correct column of X
  idx <- match(names(psi), colnames(X))[-1L]
  if (any(is.na(idx))) stop("names of psi do not match provided X", call. = FALSE)

  # note that there will always be an intercept, so this is safe
  HTE <- drop({psi[1L] + X[, idx, drop = FALSE] %*% psi[-1L]})

  if (outcome.type == "bin") {
    eXB <- pmin(exp(HTE) |> drop(), 1e8)
    HTE <- {eXB - 1.0} / {eXB + 1.0}
  }

  HTE
}

#' Derivative of Heterogeneous Treatment Effect
#'
#' @noRd
#' @param psi A numeric vector. The estimated coefficients.
#' @param X A named numeric matrix. The covariates.
#' @param outcome.type A character. The type of outcome ('cont' or 'bin')
#'
#' @returns A numeric matrix in order of psi.
#'
#' @include stopTests.R
#' @keywords internal
.dHTE <- function(psi, X, outcome.type) {

  stopifnot(
    "`psi` must be a named numeric vector" = !missing(psi) &&
      .isNumericVector(psi) && !is.null(names(psi)) &&
      all(nchar(names(psi)) > 0L),
    "`X` must be a named numeric matrix" = !missing(X) &&
      {.isNamedNumericMatrix(X) || ncol(X) == 0L},
    "`outcome.type` must be one of 'cont' or 'bin'" = !missing(outcome.type) &&
      .isCharacterVector(outcome.type, 1L) && outcome.type %in% c("cont", "bin")
  )

  # map psi to correct column of X
  idx <- match(names(psi), colnames(X))[-1L]
  if (any(is.na(idx))) stop("names of psi do not match provided X", call. = FALSE)

  if (outcome.type == "cont") {
    dHTE <- cbind(1.0, X[, idx, drop = FALSE])
  } else {
    # note that there will always be an intercept, so this is safe
    Xbeta <- drop({psi[1L] + X[, idx, drop = FALSE] %*% psi[-1L]})
    eXB <- pmin(exp(Xbeta) |> drop(), 1e8)

    dHTE <- cbind(1.0, X[, idx, drop = FALSE]) * {2.0 * eXB} / {eXB + 1.0}^2
  }

  dHTE
}
