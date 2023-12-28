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
#' @param method A character object. The regression method. Must be one of
#'   'glm' or 'sl.
#' @param method.controls A list object. User specified inputs to the
#'   regression method. Element names must match formal arguments of the
#'   regression method.
#'
#' @returns The predicted outcome for all participants in `X`.
#'
#' @importFrom stats as.formula glm glm.control poly
#' @importFrom nnls nnls
#' @import SuperLearner
#' @include stopTests.R
.sieveEstimator <- function(X, Y, wgt, subset,
                            sieve.degree, method, method.controls) {

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
    "`method` must be one of {'glm', 'sl'}" = !missing(method) &&
      .isCharacterVector(method, 1L) && method %in% c("glm", "sl"),
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

  if (method == "glm") {
    # ensure that method.controls are properly specified
    # glm allows for controls to be passed either through input 'control'
    # or through ellipsis
    glm.args <- c(names(formals(stats::glm)), names(formals(stats::glm.control)))
    if (!all(names(method.controls) %in% glm.args)) {
      stop("`method.controls` is not properly defined", call. = FALSE)
    }

    cannot_provide <- c("formula", "data", "weights", "subset")
    if (any(cannot_provide %in% names(method.controls))) {
      warning("Element(s) ",
              paste(intersect(cannot_provide, names(method.controls)), collapse = ", "),
              " cannot be provided as input; input overwritten", call. = FALSE)
    }

    response_name <- "Y"
    while (response_name %in% colnames(X)) {
      response_name <- sample(LETTERS, 26, TRUE) |> paste(collapse = "")
    }

    if (ncol(X) > 0L) {
      method.controls$formula <- paste(response_name, "~",
                                       paste(colnames(X), collapse = " + ")) |>
        stats::as.formula()
    } else {
      method.controls$formula <- Y ~ 1
    }

    df <- cbind(X, Y) |> as.data.frame()
    colnames(df) <- c(colnames(X), response_name)
    method.controls$data <- df

    method.controls$weights <- wgt
    method.controls$subset <- subset

    fit <- tryCatch(do.call(stats::glm, method.controls),
                    error = function(e) {
                      stop("glm() encountered errors\n\t",
                           e$message, call. = FALSE)
                    })

    stats::predict.glm(fit, newdata = as.data.frame(X), type = "response")

  } else {
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
}
