#' Score Function Without Confounding Function
#'
#' @noRd
#' @param psi A numeric vector object. The estimated parameters. Assuming
#'   an intercept is present in the model.
#' @param X A data.frame or matrix object. The covariates. Columns must be named.
#' @param Y A numeric vector object. The outcome of interest.
#' @param A An integer vector object. The observed treatment.
#' @param outcome.type A character object. The type of outcome. Must be one of
#'  "cont" or "bin".
#' @param mu A numeric vector object. The estimated main effects.
#' @param ps A numeric vector object. The estimated propensity score.
#' @param inv.sig2 A numeric vector object. The inverse of the conditional variance.
#' @param wgt A numeric vector object. An optional weight.
#'
#' @returns A numeric vector. The score function in the order of input `psi`.
#'
#' @keywords internal
.score.no.confounding <- function(psi, X, Y, A, outcome.type, mu, ps, inv.sig2, wgt) {

  stopifnot(
    "`psi` must be a named numeric vector" = !missing(psi) &&
      .isNamedNumericVector(psi),
    "`X` must be a named numeric matrix" = !missing(X) &&
      {ncol(X) == 0L || .isNamedNumericMatrix(X, nms = names(psi)[-1L])} &&
      ncol(X) == {length(psi) - 1L},
    "`Y` must be a numeric vector" = !missing(Y) && .isNumericVector(Y, nrow(X)),
    "`A` must be a binary vector" = !missing(A) && .isIntegerVector(A, nrow(X)) &&
      length(unique(A)) <= 2L,
    "`outcome.type` must be provided" = !missing(outcome.type),
    "`mu` must be a numeric vector" = !missing(mu) && .isNumericVector(mu, nrow(X)),
    "`ps` must be a numeric vector" = !missing(ps) &&
      {.isNumericVector(ps, nrow(X)) | .isNumericVector(ps, 1L)},
    "`inv.sig2` must be a numeric vector" = !missing(inv.sig2) &&
      .isNumericVector(inv.sig2, nrow(X)),
    "`wgt` must be a numeric vector" = !missing(wgt) &&
      .isNumericVector(wgt, nrow(X))
  )

  hte <- .HTE(psi = psi, X = X, outcome.type = outcome.type)
  dhte <- .dHTE(psi = psi, X = X, outcome.type = outcome.type)

  H <- {Y - mu - A * hte} * {A - ps} * inv.sig2 * wgt

  colMeans(dhte * H) |> unname()
}

#' Roots of the Score Function
#'
#' @noRd
#' @param X A matrix object. The covariates. Columns must be named.
#' @param initial.guess A numeric vector object. The starting parameter values.
#' @param fit.name A character object. Used for printing error messages. Should
#'   uniquely describe the call.
#' @param score.func A character. The type of outcome. Must be "basic".
#'   this input is not used here, but included to match that of future pkg
#' @param ... Additional inputs required for score function.
#'
#' @returns A numeric vector of the location of the root.
#'
#' @importFrom rootSolve multiroot
#' @include scores.R stopTests.R
#' @keywords internal
.rootsOfScore <- function(X, initial.guess, fit.name, score.func, ...) {

  dots <- list(...)

  stopifnot(
    "`X` must be a named numeric matrix" = !missing(X) &&
      {.isNamedNumericMatrix(X) || ncol(X) == 0L},
    "`initial.guess` must be a named numeric vector" = !missing(initial.guess) &&
      .isNamedNumericVector(initial.guess),
    "`fit.name` must be a character object" = !missing(fit.name) &&
      .isCharacterVector(fit.name, 1L),
    "`score.func` must be one of 'basic' 'integ'" = !missing(score.func) &&
      .isCharacterVector(score.func, 1L) && score.func %in% c("basic", "integ"),
    "`...` must contain additional inputs" = length(dots) != 0L
  )

  func <- .score.no.confounding

  tryCatch(rootSolve::multiroot(f = func, start = initial.guess, X = X, ...)$root,
           warning = function(w) {
             message(w$message, " for ", fit.name, "\n\t",
                     "parameters set to 0.0")
             rep(0.0, ncol(X) + 1L)
           },
           error = function(e) {
             stop("unable to obtain root of Score for ", fit.name, "\n\t",
                  e$message, call. = FALSE)
           })
}
