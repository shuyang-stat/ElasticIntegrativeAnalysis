#' Score function Continuous Outcome
#'
#' @noRd
#' @param psi A numeric vector object. The estimated parameters. Assuming
#'   an intercept is present in the model.
#' @param X A data.frame or matrix object. The covariates. Columns must be named.
#' @param Y A numeric vector object. The outcome of interest.
#' @param A An integer vector object. The observed treatment.
#' @param wgt A numeric vector object. The case weights.
#' @param ps A numeric vector object. The estimated propensity score.
#' @param mu0 A numeric vector object. The estimated outcome of the A = 0 model.
#'
#' @returns A numeric vector. The score function in the order of input `psi`.
#' @keywords internal
.score.cont <- function(psi, X, Y, A, wgt, ps, mu0) {

  stopifnot(
    "`psi` must be a numeric vector" = !missing(psi) && .isNumericVector(psi),
    "`X` must be a named numeric matrix" = !missing(X) &&
      {.isNamedNumericMatrix(X)  || ncol(X) == 0L} &&
      ncol(X) == {length(psi) - 1L} && all(colnames(X) %in% names(psi)),
    "`Y` must be a numeric vector" = !missing(Y) && .isNumericVector(Y, nrow(X)),
    "`A` must be a binary vector" = !missing(A) && .isIntegerVector(A, nrow(X)) &&
      length(unique(A)) <= 2L,
    "`wgt` must be a numeric vector" = !missing(wgt) && .isNumericVector(wgt, nrow(X)),
    "`ps` must be a numeric vector" = !missing(ps) && .isNumericVector(ps, nrow(X)),
    "`mu0` must be a numeric vector" = !missing(mu0) && .isNumericVector(mu0, nrow(X))
  )

  # match covariate columns to psi names
  # the first element will always be the intercept term and is unmatched
  idx <- match(names(psi), colnames(X))[-1L]

  H <- {Y - mu0 - A * drop({psi[1L] + X[, idx, drop = FALSE] %*% psi[-1L]})} *
    {A - ps} * wgt

  c(mean(H), colMeans(X[, idx, drop = FALSE] * H)) |> unname()
}

#' Score function Binary Outcome
#'
#' @noRd
#' @param psi A numeric vector object. The estimated parameters. Assuming
#'   an intercept is present in the model.
#' @param X A data.frame or matrix object. The covariates. Columns must be named.
#' @param Y A numeric vector object. The outcome of interest.
#' @param A An integer vector object. The observed treatment.
#' @param wgt A numeric vector object. The case weights.
#' @param ps A numeric vector object. The estimated propensity score.
#' @param mu0 A numeric vector object. The estimated outcome of the A = 0 model.
#'
#' @returns A numeric vector. The score function in the order of input `psi`.
#' @keywords internal
.score.binary <- function(psi, X, Y, A, wgt, ps, mu0) {

  stopifnot(
    "`psi` must be a numeric vector" = !missing(psi) && .isNumericVector(psi),
    "`X` must be a named numeric matrix" = !missing(X) &&
      {.isNamedNumericMatrix(X) || ncol(X) == 0L} &&
      ncol(X) == {length(psi) - 1L} && all(colnames(X) %in% names(psi)),
    "`Y` must be a binary vector" = !missing(Y) && .isNumericVector(Y, nrow(X)) &&
      length(unique(Y)) <= 2L,
    "`A` must be a binary vector" = !missing(A) && .isIntegerVector(A, nrow(X)) &&
      length(unique(A)) <= 2L,
    "`wgt` must be a numeric vector" = !missing(wgt) && .isNumericVector(wgt, nrow(X)),
    "`ps` must be a numeric vector" = !missing(ps) && .isNumericVector(ps, nrow(X)),
    "`mu0` must be a numeric vector" = !missing(mu0) && .isNumericVector(mu0, nrow(X))
  )

  # match covariate columns to psi names
  # the first element will always be the intercept term and is unmatched
  idx <- match(names(psi), colnames(X))[-1L]
  # pmin() avoids issues with infinities when par estimates are off the rails
  eXpsi <- pmin(drop({psi[1L] + X[, idx, drop = FALSE] %*% psi[-1L]}) |> exp(), 1e8)

  wgt <- wgt * 2.0 * eXpsi / {eXpsi + 1.0}^2 / {mu0 * (1.0 - mu0)}

  H <- {Y - mu0 - A * {eXpsi - 1.0} / {eXpsi + 1.0}} * {A - ps} * wgt

  c(mean(H), colMeans(X[, idx, drop = FALSE] * H)) |> unname()
}

#' The Evaluated Score Function
#'
#' @noRd
#' @param data A list object. Must contain elements
#'   {Y, X, A, q, mu0, ps, ml.mu1, ml.mu0, ml.ps}
#' @param psi A numeric vector object The estimated parameters.
#' @param outcome.type A character object. The type of outcome. Must be one of
#'   'cont' or 'bin'.
#'
#' @returns The Score evaluated at psi.
#'
#' @keywords internal
.evaluatedScore <- function(data, psi, outcome.type, contName) {

  stopifnot(
    "`data` must be a list containing {X, Y, A, q, ml.mu0, ml.ps, ml.sigma0}" =
      !missing(data) && is.list(data) &&
      all(c("X", "Y", "A", "q", "ml.mu0", "ml.ps", "ml.sigma0") %in% names(data)),
    "`data$ml.sigma0` must be a scalar numeric" = .isNumericVector(data$ml.sigma0, 1L),
    "`data$X must be a matrix" = is.matrix(data$X),
    "`psi` must be a named numeric vector of length ncol(X) + 1" =
      !missing(psi) && .isNumericVector(psi) && !is.null(names(psi)),
    "`outcome.type` must be a character" = !missing(outcome.type) &&
      .isCharacterVector(outcome.type, 1L) && all(outcome.type %in% c("cont", "bin")),
    "`contName` must be a character vector" = !missing(contName) &&
      {is.null(contName) || .isCharacterVector(contName)} &&
      all(contName %in% colnames(data$X)),
    "`psi` must be of length(contName) + 1" = length(psi) == {length(contName) + 1L}
  )

  if (outcome.type == "cont") {
    .score.cont(psi = psi,
                X = data$X[, contName, drop = FALSE],
                Y = data$Y,
                A = data$A,
                wgt = data$q,
                ps = data$ml.ps,
                mu0 = data$ml.mu0) / data$ml.sigma0
  } else if (outcome.type == "bin") {
    .score.binary(psi = psi,
                  X = data$X[, contName, drop = FALSE],
                  Y = data$Y,
                  A = data$A,
                  wgt = data$q,
                  ps = data$ml.ps,
                  mu0 = data$ml.mu0)
  } else {
    stop("unrecognized outcome type", call. = FALSE)
  }
}


