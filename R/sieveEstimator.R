#' Sieve Estimator
#'
#' @noRd
#' @param X A numeric matrix object. The covariates. Columns must be named.
#' @param Y A numeric vector object. The outcome of interest.
#' @param wgt A numeric vector object. The case weights.
#' @param subset A logical vector object. The participants to include in the
#'   regression.
#' @param sieve.degree A scalar numeric object. The degree of the polynomial
#'   used to define the sieve model.
#' @param method.controls A list object. User specified inputs to the
#'   SuperLearner::SuperLearner(). Element names must match formal arguments of the
#'   SuperLearner::SuperLearner().
#'
#' @returns The predicted outcome for all participants in `X`.
#'
#' @importFrom stats as.formula glm glm.control poly
#' @importFrom nnls nnls
#' @import SuperLearner
#' @include stopTests.R
.sieveEstimator <- function(X, Y, wgt, subset,
                            sieve.degree, method.controls) {

  stopifnot(
    "`X` must be a named matrix" = !missing(X) &&
      {.isNamedNumericMatrix(X) || ncol(X) == 0L},
    "`Y` must be a numeric vector" = !missing(Y) && .isNumericVector(Y, nrow(X)),
    "`wgt` must be a numeric vector" = !missing(wgt) &&
      .isNumericVector(wgt, nrow(X)),
    "`subset` must be a logical vector" = !missing(subset) &&
      .isLogicalVector(subset, nrow(X)),
    "`sieve.degree` must be a scalar numeric" = !missing(sieve.degree) &&
      .isNumericVector(sieve.degree, 1L),
    "`method.controls` must be a list" = !missing(method.controls) &&
      is.list(method.controls)
  )

  if (ncol(X) > 0L) {
    # expand design matrix
    X <- stats::poly(X, degree = sieve.degree, raw = TRUE)
    colnames(X) <- paste0("x", seq_len(ncol(X)))


    # there may be duplicated columns
    X <- X[, !duplicated(t(X)), drop = FALSE]
  }

  sl.args <- names(formals(SuperLearner::SuperLearner))
  if (!all(names(method.controls) %in% sl.args)) {
    stop("`method.controls` is not properly defined", call. = FALSE)
  }

  cannot_provide <- c("Y", "X", "newX", "obsWeights")
  if (any(cannot_provide %in% names(method.controls))) {
    warning("Element(s) ",
            paste(intersect(cannot_provide, names(method.controls)), collapse = ", "),
            " cannot be provided as input; input overwritten.")
  }

  method.controls$Y <- Y[subset]
  method.controls$X <- as.data.frame(cbind(1.0, X[subset, , drop = FALSE]))
  method.controls$newX <- as.data.frame(cbind(1.0, X))
  method.controls$obsWeights <- wgt[subset]

  fit <- tryCatch(do.call(SuperLearner::SuperLearner, method.controls),
                  error = function(e) {
                    stop("SuperLearner encountered errors\n\t",
                         e$message, call. = FALSE)
                  })
  # STH CONSIDER VERIFYING NOT ALL ZERO
  fit$SL.predict |> drop()
}
