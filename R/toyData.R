#' Generate continuous data for example
#'
#' Code used to create package datasets. Provided for the convenience of future
#'   developer, not intended for use by users. Default settings are those
#'   used to generate the data provided with the package. The code is not
#'   robustly tested.
#'
#' @noRd
#' @param seed n ninteger. The random seed for data generation
#' @param n.rct An integer. The size of the clinical trial dataset.
#' @param n.rwe An integer. The size of the real-world evidence dataset.
#'
#' @returns A list containing trivial datasets provided with the package
#'
#' @importFrom mvtnorm rmvnorm
#' @importFrom stats rbinom rnorm
#' @keywords internal
.generateToyContData <- function(seed = 1234L, n.rct = 100L, n.rwe = 500L) {

  set.seed(seed)

  Y_cont_rct <- stats::rnorm(n.rct, 0.0, 1.0)
  Y_cont_rwe <- stats::rnorm(n.rwe, 0.0, 1.0)

  X_rct <- mvtnorm::rmvnorm(n.rct, c(0,0.5), matrix(c(1.0, 0.2, 0.2, 1.0), 2L, 2L))
  colnames(X_rct) <- c("X1", "X2")
  X_rwe <- mvtnorm::rmvnorm(n.rwe, c(0,0.5), matrix(c(1.0, 0.2, 0.2, 1.0), 2L, 2L))
  colnames(X_rwe) <- c("X1", "X2")

  A_rct <- stats::rbinom(n.rct, 1L, 0.3)
  A_rwe <- stats::rbinom(n.rwe, 1L, 0.35)

  list("RCT" = list("Y" = Y_cont_rct, "X" = X_rct, "A" = A_rct),
       "RWE" = list("Y" = Y_cont_rwe, "X" = X_rwe, "A" = A_rwe))
}

#' Toy Continuous Outcome Dataset
#'
#' These datasets are provided only to facilitate examples. They are not based
#'   on or representative of any real-world applications.
#'
#' @name elasticToy.cont
#' @rdname elasticToy.cont
#' @aliases elasticToy.cont.rct elasticToy.cont.rwe
#'
#' @usage data("elasticToy.cont")
#'
#' @format elasticToy.cont provides two datasets. The elasticToy.cont.rct
#'   100 participant records; elasticToy.cont.rwe 500 participant records. Each
#'   data.frame provides the following:
#' \itemize{
#'   \item Y: A continuous outcome.
#'   \item X1: A continuous covariate.
#'   \item X2: A continuous covariate.
#'   \item A: A binary treatment variable
#' }
#'
#' @keywords datasets
NULL


#' Generate continuous data for example
#'
#' Code used to create package datasets. Provided for the convenience of future
#'   developer, not intended for use by users. Default settings are those
#'   used to generate the data provided with the package. The code is not
#'   robustly tested.
#'
#' @noRd
#' @param seed n ninteger. The random seed for data generation
#' @param n.rct An integer. The size of the clinical trial dataset.
#' @param n.rwe An integer. The size of the real-world evidence dataset.
#'
#' @returns A list containing trivial datasets provided with the package
#'
#' @importFrom mvtnorm rmvnorm
#' @importFrom stats rbinom rnorm
#' @keywords internal
.generateToyBinData <- function(seed = 2345L, n.rct = 100L, n.rwe = 500L) {

  set.seed(seed)

  Y_bin_rct <- stats::rbinom(n.rct, 1, 0.6)
  Y_bin_rwe <- stats::rbinom(n.rwe, 1, 0.55)

  X_rct <- mvtnorm::rmvnorm(n.rct, c(0, 0.5), matrix(c(1.0, 0.2, 0.2, 1.0), 2L, 2L))
  colnames(X_rct) <- c("X1", "X2")
  X_rwe <- mvtnorm::rmvnorm(n.rwe, c(0, 0.5), matrix(c(1.0, 0.3, 0.3, 1.0), 2L, 2L))
  colnames(X_rwe) <- c("X1", "X2")

  A_rct <- stats::rbinom(n.rct, 1L, 0.3)
  A_rwe <- stats::rbinom(n.rwe, 1L, 0.35)

  list("RCT" = data.frame(X_rct, "Y" = Y_bin_rct, "A" = A_rct),
       "RWE" = data.frame(X_rwe, "Y" = Y_bin_rwe, "A" = A_rwe))
}

#' Toy Binary Outcome Dataset
#'
#' These datasets are provided only to facilitate examples. They are not based
#'   on or representative of any real-world applications.
#'
#' @name elasticToy.bin
#' @rdname elasticToy.bin
#' @aliases elasticToy.bin.rct elasticToy.bin.rwe
#'
#' @usage data("elasticToy.bin")
#'
#' @format elasticToy.bin provides two datasets. The elasticToy.bin.rct
#'   100 participant records; elasticToy.bin.rwe 500 participant records. Each
#'   data.frame provides the following:
#' \itemize{
#'   \item Y: A binary outcome.
#'   \item X1: A continuous covariate.
#'   \item X2: A continuous covariate.
#'   \item A: A binary treatment variable
#' }
#'
#' @keywords datasets
NULL
