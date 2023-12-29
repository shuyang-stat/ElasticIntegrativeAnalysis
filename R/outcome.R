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
#' @param method A character object. The regression method. Must be one of
#'   'glm' or 'sl.
#' @param method.controls A list object. User specified inputs to the
#'   regression method. Element names must match formal arguments of the
#'   regression method.
#'
#' @returns A list with elements mu0, ml.mu0, ml.mu1, and ml.sigma0,
#'   which are the predicted outcome for all participants using
#'   the fitted A = 0 model, the predicted outcome for all participants using the
#'   fitted A = 0 sieve model, the predicted outcome for all participants using
#'   the fitted A = 1 sieve model, and the dispersion of the fitted A = 0
#'   sieve model
#'
#' @importFrom stats glm predict.glm
#' @keywords internal
.outcome <- function(X, Y, A, wgt, sieve.degree, method, method.controls) {
  stopifnot(
    "`X` must be a named numeric matrix" = !missing(X) &&
      {.isNamedNumericMatrix(X) || ncol(X) == 0L},
    "`Y` must be a numeric vector" = !missing(Y) && .isNumericVector(Y, nrow(X)),
    "`A` must be a binary vector" = !missing(A) && .isIntegerVector(A, nrow(X)) &&
      length(unique(A)) <= 2L,
    "`wgt` must be a numeric vector" = !missing(wgt) && .isNumericVector(wgt, nrow(X)),
    "`sieve.degree` must be a scalar numeric" = !missing(sieve.degree) &&
      .isNumericVector(sieve.degree, 1L),
    "`method` must be one of {'glm', 'sl'}" = !missing(method) &&
      .isCharacterVector(method, 1L) && method %in% c("glm", "sl"),
    "`method.controls` must be a list" = !missing(method.controls) &&
      is.list(method.controls)
  )

  res <- list()

  subset <- A == 0L

  if (any(subset)) {

    res$mu0 <- tryCatch(.sieveEstimator(X = X, Y = Y, wgt = wgt,
                                        sieve.degree = 1L,
                                        subset = subset,
                                        method = method,
                                        method.controls = method.controls),
                        error = function(e) {
                          stop("unable to fit outcome model\n\t",
                               e$message, call. = FALSE)
                        })

    res$ml.mu0 <- tryCatch(.sieveEstimator(X = X, Y = Y, wgt = wgt,
                                           sieve.degree = sieve.degree,
                                           subset = subset,
                                           method = method,
                                           method.controls = method.controls),
                           error = function(e) {
                             stop("unable to fit outcome sieve model for subset A = 0\n\t",
                                  e$message, call. = FALSE)
                           })
    res$ml.sigma0 <- {{Y[subset] - res$ml.mu0[subset]}^2} |> mean() |> sqrt()

  } else {
    message("no A = 0 cases; fitted outcomes and sigma set to 0.0")
    res$mu0 <- numeric(nrow(X))
    res$ml.mu0 <- numeric(nrow(X))
    res$ml.sigma0 <- 0.0
  }

  if (any(!subset)) {
    res$ml.mu1 <- tryCatch(.sieveEstimator(X = X, Y = Y, wgt = wgt,
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
    res$ml.mu1 <- numeric(nrow(X))
  }

  res
}
