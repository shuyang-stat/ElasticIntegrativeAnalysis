#' Sinlge Step of the Perturbation-based Variance Estimation
#'
#' Perturbation-based resampling Iteration
#'
#' @noRd
#' @param data.rct A named list containing element "X" for an RCT.
#'   Note that this function calls .psiEst(), which has additional requirements
#'   for the elements that must be in `data.rct`
#' @param data.rwe A named list containing element "X"for an RWE.
#'   Note that this function calls .psiEst(), which has additional requirements
#'   for the elements that must be in `data.rwe`
#' @param outcome.type A character. The type of outcome. Must be one of
#'   \{"cont", "bin"\} indicating a continuous or binary outcome, respectively.
#' @param models A list. Must contain elements 'RCT', a list containing the
#'   main effects model (ME) and propensity score model (PS); 'RWE', a list
#'   containing the main effects model (ME) and propensity score model (PS);
#'   'contName' the variables of the treatment effect model; 'sieve.degree',
#'   the degree of the Sieve estimator; 'outcome', a list containing the
#'   method (method) and regression control arguments (controls) for the
#'   outcome regression; and 'ps', a list containing the
#'   method (method) and regression control arguments (controls) for the
#'   propensity score regression
#'
#' @returns A list object containing $psi is a 4 * {p+1} vector containing the
#'   preliminary, efficient integrative and RCT estimators. Element
#'   $weighted.score returns Shat_{RWE, psi_rt}.
#'
#' @importFrom stats rexp
#' @include psiEst.R
#' @keywords internal
.perturbationEstIteration <- function(data.rct, data.rwe, outcome.type, models) {

  stopifnot(
    "`data.rct` must be a list containing X" = !missing(data.rct) &&
      is.list(data.rct) && "X" %in% names(data.rct),
    "`data.rwe` must be a list containing X" = !missing(data.rwe) &&
      is.list(data.rwe) && "X" %in% names(data.rwe),
    "`data.rct$X` must be a named numeric matrix" = .isNamedNumericMatrix(data.rct$X) ||
      ncol(data.rct$X) == 0L,
    "`data.rwe$X` must be a named numeric matrix" = .isNamedNumericMatrix(data.rwe$X) ||
      ncol(data.rwe$X) == 0L,
    "`outcome.type` must be provided" = !missing(outcome.type),
    "`models` must be provided" = !missing(models)
  )

  data.rct$q <- stats::rexp(nrow(data.rct$X))
  data.rwe$q <- stats::rexp(nrow(data.rwe$X))

  psi_list_p <- .psiEst(data.rct = data.rct, data.rwe = data.rwe,
                        outcome.type = outcome.type, models = models)

  # convert matrix of parameter estimates to a vector with names
  # set as the combination approx_method.covariate_name
  nms <- c(t(outer(rownames(psi_list_p$psi), colnames(psi_list_p$psi),
                   FUN = paste, sep = ".")))
  psi_list_p$psi <- c(t(psi_list_p$psi))
  names(psi_list_p$psi) <- nms

  psi_list_p
}

#' Perturbation-based Variance Estimation
#'
#' Perturbation-based resampling
#'
#' @noRd
#' @param data.rct A named list containing element "X" for an RCT.
#'   Note that this function calls .psiEst(), which has additional requirements
#'   for the elements that must be in `data.rct`
#' @param data.rwe A named list containing element "X"for an RWE.
#'   Note that this function calls .psiEst(), which has additional requirements
#'   for the elements that must be in `data.rwe`
#' @param n.pert An integer. The number of perturbations.
#' @param outcome.type A character. The type of outcome. Must be one of
#'   \{"cont", "bin"\} indicating a continuous or binary outcome, respectively.
#' @param models A list. Must contain elements 'RCT', a list containing the
#'   main effects model (ME) and propensity score model (PS); 'RWE', a list
#'   containing the main effects model (ME) and propensity score model (PS);
#'   'contName' the variables of the treatment effect model; 'sieve.degree',
#'   the degree of the Sieve estimator; 'outcome', a list containing the
#'   method (method) and regression control arguments (controls) for the
#'   outcome regression; and 'ps', a list containing the
#'   method (method) and regression control arguments (controls) for the
#'   propensity score regression
#'
#' @returns A list object containing `ptb` a matrix of the estimated parameters
#'   for each perturbation and `Shat.rw.psihat.rt` a matrix of the estimated
#'   weighted efficient RCT score on RWE for each perturbation.
#'
#' @importFrom stats rexp
#' @include psiEst.R
#' @keywords internal
.perturbationEst <- function(data.rwe, data.rct, n.pert = 100L,
                             outcome.type, models) {

  stopifnot(
    "`data.rct` must be provided" = !missing(data.rct),
    "`data.rwe` must be provided" = !missing(data.rwe),
    "`n.pert` must be a positive integer" = !missing(n.pert) &&
      .isNumericVector(n.pert, 1L) &&
      isTRUE(all.equal(n.pert, as.integer(n.pert))) && n.pert > 0,
    "`outcome.type` must be provided" = !missing(outcome.type),
    "`models` must be provided" = !missing(models)
  )

  res <- list()
  for (i in seq_len(n.pert)) {
    res[[i]] <- .perturbationEstIteration(data.rct = data.rct,
                                          data.rwe = data.rwe,
                                          outcome.type = outcome.type,
                                          models = models)

  }

  res <- do.call(rbind, res)

  list("ptb" = do.call(rbind, res[, "psi"]),
       "Shat.rw.psihat.rt" = do.call(rbind, res[, "weighted.score"]))
}

#' Calculated V_eff and V_rt
#'
#' @noRd
#' @param ptb A numeric matrix. The estimated psi for all perturbations
#' @param n.rwe A scalar. The number of participants in the RWE dataset.
#'
#' @returns FALSE if estimates are not acceptable; A list containing the
#'   estimated variances if acceptable. Acceptable means all positive
#'   eigenvalues for V_rt - V_eff
#'
#' @importFrom stats var
#' @importFrom utils tail
#' @keywords internal
.computeV <- function(ptb, n.rwe) {

  # compute Vrt and Veff

  # identify the columns of ptb for the efficient estimator
  eff_columns <- grepl("eff.", colnames(ptb))
  # extract only those columns
  ptb_eff <- ptb[, eff_columns, drop = FALSE]

  # drop the names to only the covariate names
  colnames(ptb_eff) <- strsplit(colnames(ptb_eff), ".", fixed = TRUE) |>
    lapply(utils::tail, n = -1L) |> lapply(paste, collapse = ".")
  V_eff <- {stats::var(ptb_eff, na.rm = TRUE) * n.rwe} |> matrix(ncol = sum(eff_columns))

  # identify the columns of ptb for the RCT estimator
  rt_columns <- grepl("rt.", colnames(ptb))
  # extract only those columns
  ptb_rt <- ptb[, rt_columns, drop = FALSE]
  # rename to the covariate onnly names
  colnames(ptb_rt) <- names(ptb_eff)
  V_rt <- {stats::var(ptb_rt, na.rm = TRUE) * n.rwe} |> matrix(ncol = sum(rt_columns))

  V_rt_eff <- V_rt - V_eff

  evs <- tryCatch(eigen(V_rt_eff, only.values = TRUE),
                  error = function(e) {
                    stop("unable to complete eigenvalue decomposition\n\t",
                         e$message, call. = FALSE)
                  })

  if (all(evs$values > 0.0)) {
    list("V.eff" = V_eff, "V.rt" = V_rt)
  } else {
    FALSE
  }

}

#' Perturbation-based Variance Estimation Procedure
#'
#' Perturbation-based resampling to estimate variance.
#'
#' @noRd
#' @param data.rct A named list containing element "X" for an RCT.
#'   Note that this function calls .perturbationEst(), which has additional
#'   requirements for the elements that must be in `data.rct`
#' @param data.rwe A named list containing element ""X" for an RWE.
#'   Note that this function calls .perturbationEst(), which has additional
#'   requirements for the elements that must be in `data.rwe`
#' @param n.pert An integer. The number of perturbations.
#' @param outcome.type A character. The type of outcome. Must be one of
#'   \{"cont", "bin"\} indicating a continuous or binary outcome, respectively.
#' @param models A list. Must contain elements 'RCT', a list containing the
#'   main effects model (ME) and propensity score model (PS); 'RWE', a list
#'   containing the main effects model (ME) and propensity score model (PS);
#'   'contName' the variables of the treatment effect model; 'sieve.degree',
#'   the degree of the Sieve estimator; 'outcome', a list containing the
#'   method (method) and regression control arguments (controls) for the
#'   outcome regression; and 'ps', a list containing the
#'   method (method) and regression control arguments (controls) for the
#'   propensity score regression
#'
#' @returns A list object containing
#'   \begin{itemize}
#'     \item{V.est }{Numeric vector. The variance across perturbations for all
#'                   parameter estimates.}
#'     \item{V.rt }{Numeric matrix. The variance for psi.rt.}
#'     \item{V.eff }{Numeric matrix. The variance for psi.eff.}
#'     \item{sqrt.V.eff }{Numeric matrix. The sqrt of V.eff.}
#'     \item{Shat.rw.psihat.rt.means }{Numeric vector. The weighted score.}
#'   \end{itemize}
#'
#' @importFrom stats rexp
#' @importFrom expm sqrtm
#' @include psiEst.R
#' @keywords internal
.perturbationProcedure <- function(data.rct, data.rwe, n.pert = 100L,
                                   outcome.type, models) {

  stopifnot(
    "`data.rct` must be a list containing X" = !missing(data.rct) &&
      is.list(data.rct) && "X" %in% names(data.rct),
    "`data.rwe` must be a list containing X" = !missing(data.rwe) &&
      is.list(data.rwe) && "X" %in% names(data.rwe),
    "`data.rct$X` must be a named numeric matrix" = .isNamedNumericMatrix(data.rct$X) ||
      ncol(data.rct$X) == 0L,
    "`data.rwe$X` must be a named numeric matrix" = .isNamedNumericMatrix(data.rwe$X) ||
      ncol(data.rwe$X) == 0L,
    "`n.pert` must be provided" = !missing(n.pert),
    "`outcome.type` must be provided" = !missing(outcome.type),
    "`models` must be a list with elements 'RWE', 'RCT', 'outcome', 'ps', 'sieve.degree', and 'contName'" =
      !missing(models) && is.vector(models, mode = "list") &&
      all(c("RWE", "RCT", "outcome", "ps", "sieve.degree", "contName") %in% names(models))
  )

  # +1 for intercept
  n_cov <- length(models$contName) + 1L
  n_rwe <- nrow(data.rwe$X)

  ntime <- 0L
  while (TRUE) {
    ntime <- ntime + 1L

    # begin perturbation estimation

    # a list object is returned. Element ptb contains an n.pert x 4p matrix;
    # element ptb.S.rwe1 contains an n.pert x p matrix
    pert_est <- .perturbationEst(data.rwe = data.rwe,
                                 data.rct = data.rct,
                                 n.pert = n.pert,
                                 outcome.type = outcome.type,
                                 models = models)

    Vee <- .computeV(ptb = pert_est$ptb, n.rwe = n_rwe)

    if (is.list(Vee)) break

    if (ntime > 50L) {
      warning("perturbation procedure did not converge", call. = FALSE)
      break
    }
  }

  sqrt_V_eff <- tryCatch(expm::sqrtm(Vee$V.eff),
                         error = function(e) {
                           stop("unable to obtain square root of V_eff\n\t",
                                e$message, call. = FALSE)
                           })
  if (any(abs(Im(sqrt_V_eff)) > 1e-6)) {
    warning("sqrtm returned complex matrix; only real component used", call. = FALSE)
  }

  # estimate the variance of each estimated covariate for all estimator
  # convert vector result to an n_estimator x n_cov matrix
  ve <- apply(pert_est$ptb, 2L, stats::var) |> matrix(nrow = n_cov) |> t()

  # set column names as the covariate names
  colnames(ve) <- strsplit(colnames(pert_est$ptb), ".", fixed = TRUE) |>
    lapply("[", 2L) |> unlist() |> unique()
  # set row names as the estimator names
  rownames(ve) <- strsplit(colnames(pert_est$ptb), ".", fixed = TRUE) |>
    lapply("[", 1L) |> unlist() |> unique()

  # eta will be standardized later
  eta <- colMeans(pert_est$Shat.rw.psihat.rt) * sqrt(n_rwe)
  names(eta) <- colnames(ve)

  list("V.est" = ve,
       "eta" = eta,
       "V.rt" = Vee$V.rt,
       "V.eff" = Vee$V.eff,
       "sqrt.V.eff" = sqrt_V_eff)
}
