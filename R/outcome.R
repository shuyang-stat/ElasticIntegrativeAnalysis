.conditionalVar <- function(y, yhat, outcome.type) {
  switch(outcome.type,
         "cont" = stats::var(y - yhat),
         "bin" = mean(yhat) * {1.0 - mean(yhat)})
}

#' Estimated Outcome Mean Models and Variances
#'
#' @noRd
#' @param X A numeric matrix object. The model design matrix. Must have named
#'   columns.
#' @param Y A numeric vector object. The outcome of interest.
#' @param A An integer vector object. The binary treatment.
#' @param wgt A numeric vector object.
#' @param sieve.degree A scalar numeric object. The degree of the polynomial
#'   used to define the sieve model.
#' @param method A character indicating the regression method to be used.
#'   Must be one of {"glm", "SL"}.
#' @param method.controls A list object. User specified inputs to
#'   regression method.
#' @param outcome.type A character object. One of {"cont", "bin"}.
#'
#' @returns A list with elements mu0, ml.mu0, ml.mu1, and ml.mu0.conditional.var,
#'   which are the predicted outcome for all participants using
#'   the fitted A = 0 model, the predicted outcome for all participants using the
#'   fitted A = 0 sieve model, the predicted outcome for all participants using
#'   the fitted A = 1 sieve model, and the conditional variance of the fitted
#'   A = 0 sieve model
#'
#' @keywords internal
.outcome <- function(X, Y, A, wgt,
                     sieve.degree, method, method.controls,
                     outcome.type) {

  stopifnot(
    "`X` must be a named numeric matrix" = !missing(X) &&
      {.isNamedNumericMatrix(X) || ncol(X) == 0L},
    "`Y` must be a numeric vector" = !missing(Y) && .isNumericVector(Y, nrow(X)),
    "`A` must be a binary vector" = !missing(A) && .isIntegerVector(A, nrow(X)) &&
      length(unique(A)) <= 2L,
    "`outcome.type` must be one of 'cont' or 'bin'" = !missing(outcome.type) &&
      .isCharacterVector(outcome.type, 1L) && outcome.type %in% c("cont", "bin"),
    "`wgt` must be a numeric vector" = !missing(wgt) && .isNumericVector(wgt, nrow(X)),
    "`sieve.degree` must be a scalar numeric" = !missing(sieve.degree) &&
      .isNumericVector(sieve.degree, 1L),
    "`method` must be provided" = !missing(method),
    "`method.controls` must be a list" = !missing(method.controls) &&
      is.list(method.controls)
  )

  res <- list()

  subset <- A == 0L

  if (any(subset)) {

    res$me0 <- tryCatch(.sieveEstimator(X = X, Y = Y, wgt = wgt,
                                        sieve.degree = 1L,
                                        subset = subset,
                                        method = method,
                                        method.controls = method.controls),
                        error = function(e) {
                          stop("unable to fit outcome model\n\t",
                               e$message, call. = FALSE)
                        })

    res$me0.conditional.var <- .conditionalVar(y = Y[subset],
                                               yhat = res$me0[subset],
                                               outcome.type = outcome.type)

    res$ml.me0 <- tryCatch(.sieveEstimator(X = X, Y = Y, wgt = wgt,
                                           sieve.degree = sieve.degree,
                                           subset = subset,
                                           method = method,
                                           method.controls = method.controls),
                           error = function(e) {
                             stop("unable to fit outcome sieve model for subset A = 0\n\t",
                                  e$message, call. = FALSE)
                           })

    res$ml.me0.conditional.var <- .conditionalVar(y = Y[subset],
                                                  yhat = res$ml.me0[subset],
                                                  outcome.type = outcome.type)

  } else {
    # set variances to 1 here to avoid dividing by 0
    message("no A = 0 cases; fitted outcomes set to 0.0")
    res$me0 <- numeric(nrow(X))
    res$me0.conditional.var <- 1.0
    res$ml.me0 <- numeric(nrow(X))
    res$ml.me0.conditional.var <- 1.0
  }

  if (any(!subset)) {
    res$ml.me1 <- tryCatch(.sieveEstimator(X = X, Y = Y, wgt = wgt,
                                           sieve.degree = sieve.degree,
                                           subset = !subset,
                                           method = method,
                                           method.controls = method.controls),
                           error = function(e) {
                             stop("unable to fit outcome sieve model for subset A = 1\n\t",
                                  e$message, call. = FALSE)
                           })

  } else {
    message("no A = 1 cases; fitted outcome set to 0.0")
    res$ml.me1 <- numeric(nrow(X))
  }

  # need to store the conditional variance as a vector
  res$inv.sig2 <- rep(1.0 / res$me0.conditional.var, nrow(X))
  res$ml.inv.sig2 <- rep(1.0 / res$ml.me0.conditional.var, nrow(X))

  res
}
