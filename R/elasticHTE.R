#' Elastic Integrative Analysis for Heterogeneous Treatment Effect
#'
#'@description
#' `elasticHTE()` is a test-based dynamic borrowing framework combining
#'   a randomized clinical trial (RCT) and a real-world evidence (RWE) study,
#'   in which a preliminary test statistic is used to gauge the comparability
#'   and reliability of the RWE and to decide whether or not to use the RWE in
#'   an integrative analysis. The parameter of interest is \eqn{\psi}{`psi`},
#'   which quantifies how the treatment effect varies over the treatment
#'   modifiers.
#'
#' @param data.rct A named list containing elements "X", "Y", and "A" for an RCT.
#'   Element X is a numeric covariate matrix without an intercept.
#'   Element Y is a binary or continuous response vector.
#'   Element A is a binary treatment vector.
#'   List can also contain element "ps", a vector of known propensity scores P(A=1).
#' @param data.rwe A named list containing elements "X", "Y", and "A" for an RWE
#'   study.
#'   Element X is a numeric covariate matrix without an intercept.
#'   Element Y is a binary or continuous response vector.
#'   Element A is a binary treatment vector.
#' @param ... Ignored. Included to require named inputs.
#' @param mainName.rct NULL, character vector, or an integer. The covariates of the
#'   main effects component of the outcome model for the RCT data. If NULL, all covariates in
#'   \code{data.rct$X} specify the model; if a character vector, the column
#'   headers of \code{data.rct$X} to include in the model. Note that an
#'   intercept is always included in the model; though it is not recommended,
#'   an intercept only main effects model can be specified as \code{mainName.rct = 1}.
#' @param mainName.rwe NULL, character vector, or an integer. The covariates of the
#'   main effects component of the outcome model for the RWE data. If NULL, all covariates in
#'   \code{data.rwe$X} specify the model; if a character vector, the column
#'   headers of \code{data.rwe$X} to include in the model. Note that an
#'   intercept is always included in the model; though it is not recommended,
#'   an intercept only main effects model can be specified as \code{mainName.rwe = 1}.
#' @param contName NULL, character vector, or an integer. The covariates of the
#'   contrasts component of the outcome model. If NULL, all covariates in
#'   \code{data.rct$X}  specify the model; if a character vector, the column
#'   headers of \code{data.rct$X}  to include in the model. Note these are the
#'   covariates that interact with the treatment variable (~ A:contName); an
#'   intercept is always included, such that ~A is the minimal contrasts model;
#'   though it is not recommended, an intercept only model can be specified as
#'   \code{contName = 1}.
#' @param psName.rct NULL, character vector, or an integer. The covariates of the
#'   the propensity score model for the RCT data. If NULL, all covariates in \code{data.rct$X}
#'   specify the model; if a character vector, the column headers of
#'   \code{data.rct$X} to include in the model. Note that an intercept is
#'   always included in the model; an intercept only model can be specified as
#'   \code{psName.rct = 1}.
#' @param psName.rwe NULL, character vector, or an integer. The covariates of the
#'   the propensity score model for the RWE data. If NULL, all covariates in \code{data.rwe$X}
#'   specify the model; if a character vector, the column headers of
#'   \code{data.rwe$X} to include in the model. Note that an intercept is
#'   always included in the model; an intercept only model can be specified as
#'   \code{psName.rwe = 1}.
#' @param thres.psi NULL or a scalar numeric. The threshold for constructing
#'   adaptive confidence interval. STH QUESTION: Is there an allowed range we
#'   should mention?
#' @param sieve.degree A positive integer > 1. The order of the polynomial
#'   defining the sieve model. For example, `sieve.degree = 2` yields outcome and
#'   propensity models that include all covariates, the squared covariates, and
#'   all pair-wise interactions.
#' @param outcome.type A character. The type of outcome. Must be one of
#'   \{"cont", "bin"\} indicating a continuous or binary outcome, respectively.
#' @param outcome.method A character. The regression method for outcomes.
#'   Must be one of \{'glm', 'SL'\}
#' @param outcome.controls A named list. Additional inputs provided to
#'   \code{stats::glm()} or
#'   \code{SuperLearner::SuperLearner()} for the outcome regression analyses.
#'   Element names must match the formal arguments of \code{stats::glm()} or
#'   \code{SuperLearner::SuperLearner()} and should include, at a minimum,
#'   elements "family" (specifying the error distribution). Please see documentation
#'   of \code{stats::glm()} or\code{SuperLearner::SuperLearner()} for additional
#'   input options. Note that for \code{SuperLearner::SuperLearner()},
#'   formal arguments \code{Y}, \code{X}, \code{newX}, and  \code{obsWeight} are
#'   set internally and should not be provided here. Similarly, for \code{stats::glm()}
#'   input \code{formula}, \code{data}, and \code{weights} cannot be set through input.
#' @param ps.method A character. The regression method for propensity score
#'   analysis. Must be one of \{'glm', 'SL'\}
#' @param ps.controls A named list. Additional inputs provided to
#'   \code{stats::glm()} or
#'   \code{SuperLearner::SuperLearner()} for the outcome regression analyses.
#'   Element names must match the formal arguments of \code{stats::glm()} or
#'   \code{SuperLearner::SuperLearner()} and should include, at a minimum,
#'   elements "family" (specifying the error distribution). Please see documentation
#'   of \code{stats::glm()} or\code{SuperLearner::SuperLearner()} for additional
#'   input options. Note that for \code{SuperLearner::SuperLearner()},
#'   formal arguments \code{Y}, \code{X}, \code{newX}, and  \code{obsWeight} are
#'   set internally and should not be provided here. Similarly, for \code{stats::glm()}
#'   input \code{formula}, \code{data}, and \code{weights} cannot be set through input.
#' @param fixed A logical. How to select the tuning parameter
#'   \eqn{c_{\gamma}}{c_gamma}. FALSE, the default, selects an adaptive
#'   selection strategy; TRUE selects a fixed threshold strategy.
#'   The default fixed threshold is \code{stats::qchisq(0.95, df = p)},
#'   in which p is the dimension of the contrasts model.
#' @param n.pert An integer. The number of perturbations to generate when
#'   estimating the variance.
#' @param n.boot An integer. The number of bootstrap samples to generate
#'   when estimating the confidence intervals.
#' @param n.gamma An integer. The number of samples to generate to estimate
#'   \eqn{c_{\gamma}}{c_gamma}, the threshold.
#' @returns A list with components:
#'   \item{psi }{A matrix containing the estimated \eqn{\psi}{`psi`} associated
#'     with the treatment modifiers under various models
#'     (\eqn{\psi_p}{`psi_p`}, \eqn{\psi_{eff}}{`psi_eff`}, \eqn{\psi_{rt}}{`psi_rt`},
#'     \eqn{\psi_{elastic}}{`psi_elastic`},
#'     \eqn{\psi_{elastic.debiased}}{`psi_elastic.debiased`}).}
#'   \item{ve }{A matrix containing the estimated standard error for
#'     \eqn{\psi}{`psi`}.}
#'   \item{CIs.inf, CIs.sup }{A matrix containing the estimated confidence
#'     intervals for \eqn{\psi}{`psi`}.}
#'   \item{conservative }{A logical \eqn{I(Tstat < thres.psi)}{I(Tstat < thres.psi)} }
#'   \item{CI.settings }{A list of the settings used in the confidence interval
#'     procedure.}
#'   \item{nuispar }{A list providing the selected \eqn{\gamma}{gamma} and
#'     its corresponding threshold value \eqn{c_{\gamma}}{c.gamma};
#'     indicator \eqn{I(c_{\gamma}  > Tstat)}{I(c.gamma > Tstat)} and its
#'     p-value as well as a list of the settings used in the
#'     selection procedure.}
#'
#' @examples
#' # load provided illustrative toy dataset with continuous outcome
#' data("elasticToy.cont")
#'
#' # conduct the elastic integrative analysis with defaults
#' \dontrun{
#' result.cont <- elasticHTE(data.rct = elasticToy.cont.rct,
#'                           data.rwe = elasticToy.cont.rwe)
#' }
#' # load provided illustrative toy dataset with binary outcome
#' data("elasticToy.bin")
#'
#' # conduct the elastic integrative analysis with defaults
#' \dontrun{
#' result.bin <- elasticHTE(data.rct = elasticToy.bin.rct,
#'                          data.rwe = elasticToy.bin.rwe,
#'                          outcome.type = "bin")
#' }
#'
#' @importFrom stats complete.cases
#' @include bias.R bootFunc.R cGamma.R perturbationEst.R psiEst.R
#' @include sigma_matrices.R stopTests.R utils.R
#' @export
elasticHTE <- function(data.rct,
                       data.rwe,
                       ...,
                       mainName.rct = NULL,
                       mainName.rwe = mainName.rct,
                       contName = NULL,
                       psName.rct = NULL,
                       psName.rwe = psName.rct,
                       thres.psi = NULL,
                       sieve.degree = 2L,
                       outcome.type = c("cont", "bin"),
                       outcome.method = c("glm", "SL"),
                       outcome.controls = list("family" = "gaussian"),
                       ps.method = c("glm", "SL"),
                       ps.controls = list("family" = "quasibinomial"),
                       fixed = FALSE,
                       n.pert = 100L,
                       n.boot = 100L,
                       n.gamma = 1000L) {

  outcome.type <- match.arg(outcome.type)
  outcome.method <- match.arg(outcome.method)
  ps.method <- match.arg(ps.method)
  mainName.rwe <- eval(mainName.rwe)
  psName.rwe <- eval(psName.rwe)

  stopifnot(
    "`data.rct` must be a named list containing elements 'X', 'Y', and 'A'" =
      !missing(data.rct) &&
      is.list(data.rct) && all(c("X", "Y", "A") %in% names(data.rct)),
    "`data.rwe` must be a named list containing elements 'X', 'Y', and 'A'" =
      !missing(data.rwe) &&
      is.list(data.rwe) && all(c("X", "Y", "A") %in% names(data.rwe)),
    "`data.rwe$X` and `data.rct$X` must be matrices with column names" =
      {.isNamedNumericMatrix(data.rwe$X) || ncol(data.rwe$X) == 0L} &&
      {.isNamedNumericMatrix(data.rct$X) || ncol(data.rwe$X) == 0L},
    "`mainName.rct` must be a character vector of X column headers" = is.null(mainName.rct) ||
      {.isNumericVector(mainName.rct, 1L) && isTRUE(all.equal(mainName.rct, 1))} ||
      {.isCharacterVector(mainName.rct) && all(mainName.rct %in% colnames(data.rct$X))},
    "`mainName.rwe` must be a character vector of X column headers" = is.null(mainName.rwe) ||
      {.isNumericVector(mainName.rwe, 1L) && isTRUE(all.equal(mainName.rwe, 1))} ||
      {.isCharacterVector(mainName.rwe) && all(mainName.rwe %in% colnames(data.rwe$X))},
    "`contName` must be a character vector of X column headers" = is.null(contName) ||
      {.isNumericVector(contName, 1L) && isTRUE(all.equal(contName, 1))} ||
      {.isCharacterVector(contName) && all(contName %in% colnames(data.rct$X)) &&
          all(contName %in% colnames(data.rwe$X))},
    "`psName.rct` must be a character vector of X column headers" = is.null(psName.rct) ||
      {.isNumericVector(psName.rct, 1L) && isTRUE(all.equal(psName.rct, 1))} ||
      {.isCharacterVector(psName.rct) && all(psName.rct %in% colnames(data.rct$X))},
    "`psName.rwe` must be a character vector of X column headers" = is.null(psName.rwe) ||
      {.isNumericVector(psName.rwe, 1L) && isTRUE(all.equal(psName.rwe, 1))} ||
      {.isCharacterVector(psName.rwe) && all(psName.rwe %in% colnames(data.rwe$X))},
    "`thres.psi` must be a positive scalar" = is.null(thres.psi) ||
      {.isNumericVector(thres.psi, 1L) && thres.psi > 0.0},
    "`sieve.degree` must be a positive integer" = .isNumericVector(sieve.degree, 1L) &&
      isTRUE(all.equal(sieve.degree, round(sieve.degree, 0L))) && sieve.degree > 1,
    "`outcome.type` must be one of {'cont', 'bin'}" = .isCharacterVector(outcome.type, 1L) &&
      outcome.type %in% c("cont", "bin"),
    # outcome.method is being tested by match.arg()
    "`outcome.controls` must be a named list" = is.list(outcome.controls) &&
      {{length(outcome.controls) > 0L && !is.null(names(outcome.controls)) &&
          !any(nchar(names(outcome.controls)) == 0L)} ||
       {length(outcome.controls) == 0L}},
    # ps.method is being tested by match.arg()
    "`ps.controls` must be a named list" = is.list(ps.controls) &&
      {{length(ps.controls) > 0L && !is.null(names(ps.controls)) &&
          !any(nchar(names(ps.controls)) == 0L)} ||
      {length(ps.controls) == 0L}},
    "`fixed` must be a logical" = .isLogicalVector(fixed, 1L),
    "`n.pert` must be a positive integer" = .isNumericVector(n.pert, 1L) &&
      isTRUE(all.equal(n.pert, round(n.pert, 0L))) && n.pert > 0,
    "`n.boot` must be a positive integer" = .isNumericVector(n.boot, 1L) &&
      isTRUE(all.equal(n.boot, round(n.boot, 0L))) && n.boot > 0,
    "`n.gamma` must be a positive integer" = .isNumericVector(n.gamma, 1L) &&
      isTRUE(all.equal(n.gamma, round(n.gamma, 0L))) && n.gamma > 0
  )

  if (is.null(outcome.controls$family)) {
    outcome.controls$family <- switch(outcome.type,
                                      "cont" = "gaussian",
                                      "bin" = "quasibinomial")
  }

  if (is.null(ps.controls$family)) ps.controls$family <- "quasibinomial"

  # NULL input means "all covariates in X"; integer input means "intercept only"
  mainName.rct <- .adjustModelCoding(mainName.rct, colnames(data.rct$X))
  mainName.rwe <- .adjustModelCoding(mainName.rwe, colnames(data.rwe$X))
  contName <- .adjustModelCoding(contName, colnames(data.rct$X))
  psName.rct <- .adjustModelCoding(psName.rct, colnames(data.rct$X))
  psName.rwe <- .adjustModelCoding(psName.rwe, colnames(data.rwe$X))

  # reduce dataset down to only those covariates used in models
  if (is.null(mainName.rct) && is.null(contName) && is.null(psName.rct)) {
    data.rct$X <- matrix(NA, nrow(data.rct$X), 0L)
  } else if (!is.null(mainName.rct) || !is.null(contName) || !is.null(psName.rct)) {
    all_cov <- unique(c(mainName.rct, contName, psName.rct))

    if (!all(all_cov %in% colnames(data.rct$X))) {
      stop("not all model covariates are found in provided data\n\t",
           "model covariate: ", paste(all_cov, collapse = ", "), "\n\t",
           "data.rct: ", paste(colnames(data.rct$X), collapse = ", "), "\n\t", call. = FALSE)
    }

    data.rct$X <- data.rct$X[, all_cov]

    # spaces in covariate names might cause issues later
    colnames(data.rct$X) <- .fixNames(colnames(data.rct$X))
    mainName.rct <- .fixNames(mainName.rct)
    contName <- .fixNames(contName)
    psName.rct <- .fixNames(psName.rct)

    # if this introduces duplicate column headers, ask user to adjust column
    # headers themselves
    if (length(unique(colnames(data.rct$X))) != ncol(data.rct$X)) {
      stop("duplicate column headers found in X, ",
           "possibly due to required removal of spaces ",
           "please eliminate spaces from column header names in `data.rct$X` ",
           call. = FALSE)
    }
  }

  if (is.null(mainName.rwe) && is.null(contName) && is.null(psName.rwe)) {
    data.rwe$X <- matrix(NA, nrow(data.rwe$X), 0L)
  } else if (!is.null(mainName.rwe) || !is.null(contName) || !is.null(psName.rwe)) {
    all_cov <- unique(c(mainName.rwe, contName, psName.rwe))

    if (!all(all_cov %in% colnames(data.rwe$X))) {
      stop("not all model covariates are found in provided data\n\t",
           "model covariate: ", paste(all_cov, collapse = ", "), "\n\t",
           "data.rwe: ", paste(colnames(data.rwe$X), collapse = ", "), "\n\t", call. = FALSE)
    }

    data.rwe$X <- data.rwe$X[, all_cov]

    # spaces in covariate names might cause issues later
    colnames(data.rwe$X) <- .fixNames(colnames(data.rwe$X))
    mainName.rwe <- .fixNames(mainName.rwe)
    contName <- .fixNames(contName)
    psName.rwe <- .fixNames(psName.rwe)

    # if this introduces duplicate column headers, ask user to adjust column
    # headers themselves
    if (length(unique(colnames(data.rwe$X))) != ncol(data.rwe$X)) {
      stop("duplicate column headers found in X, ",
           "possibly due to required removal of spaces ",
           "please eliminate spaces from column header names in `data.rwe$X` ",
           call. = FALSE)
    }
  }

  # we do not allow for missing values
  if (any(!stats::complete.cases(data.rct$X, data.rct$Y, data.rct$A, data.rct$ps)) ||
      any(!stats::complete.cases(data.rwe$X, data.rwe$Y, data.rwe$A))) {
    stop("elements of `data.rct` and `data.rwe` cannot contain missing values",
         call. = FALSE)
  }

  # if user provided ps for RWE, warn and remove
  if (!is.null(data.rwe$ps)) {
    warning("`ps` cannot be provided in `data.rwe`; input ignored",
            call. = FALSE)
    data.rwe$ps <- NULL
  }

  # keep only the information we need
  data.rct <- data.rct[names(data.rct) %in% c("X", "Y", "A", "ps")]
  data.rwe <- data.rwe[names(data.rwe) %in% c("X", "Y", "A")]

  # flag indicates if user provided ps for RCT
  # TRUE indicates that these propensity scores must be estimated
  data.rct$est.ps <- is.null(data.rct$ps)

  # element q is required in the perturbation resampling procedure. it is
  # not used in the initial estimates of psi. default them to 1 here
  # to avoid having to worry about it in the psi estimation procedure
  data.rct$q <- rep(1.0, nrow(data.rct$X))
  data.rwe$q <- rep(1.0, nrow(data.rwe$X))

  # number of participants in RCT
  n_rct <- length(data.rct$Y)

  # number of participants in RWE
  n_rwe <- length(data.rwe$Y)

  if (n_rct >= n_rwe) {
    warning("methods developed under the assumption that n >> m; ",
            "requested analysis has m/n = ", format(n_rct / n_rwe, digits = 2),
            call. = FALSE)
  }

  # ensure that treatments are binary integer 0/1
  if (is.factor(data.rct$A) && is.factor(data.rwe$A)) {
    if (all(levels(data.rct$A) != levels(data.rwe$A))) {
      stop("levels of data.rct$A and data.rwe$A do not match", call. = FALSE)
    }
  } else if (is.factor(data.rct$A)) {
    orig_levels <- levels(data.rct$A)
    if (!all(data.rwe$A %in% orig_levels)) {
      stop("treatment sets of data.rct$A and data.rwe$A do not match", call. = FALSE)
    }
  } else if (is.factor(data.rwe$A)) {
    orig_levels <- levels(data.rwe$A)
    if (!all(data.rct$A %in% orig_levels)) {
      stop("treatment sets of data.rct$A and data.rwe$A do not match", call. = FALSE)
    }
  } else {
    orig_levels <- c(unique(data.rct$A), unique(data.rwe$A)) |> unique() |> sort()
  }

  # ensure the treatment in RCT and RWE are the same set
  if (length(orig_levels) != 2L) {
    stop("more than 2 treatments found in data.rct$A and data.rwe$A", call. = FALSE)
  }

  data.rct$A <- .fixA(data.rct$A, "data.rct$A", orig.levels = orig_levels)
  data.rwe$A <- .fixA(data.rwe$A, "data.rwe$A", orig.levels = orig_levels)

  # Ensure that outcome type does not blatantly conflict with provided data
  # NA values shouldn't be an issue as there is a previous stopping condition
  # for that
  data.rct$Y <- .fixY(data.rct$Y, outcome.type, "data.rct$Y")
  data.rwe$Y <- .fixY(data.rwe$Y, outcome.type, "data.rwe$Y")

  # if not provided set default threshold value
  if (is.null(thres.psi)) thres.psi <- n_rwe |> log() |> sqrt()

  models <- list("RWE" = list("ME" = mainName.rwe,
                              "PS" = psName.rwe),
                 "RCT" = list("ME" = mainName.rct,
                              "PS" = psName.rct),
                 "contName" = contName,
                 "outcome" = list("method" = outcome.method,
                                  "controls" = outcome.controls),
                 "ps" = list("method" = ps.method,
                             "controls" = ps.controls),
                 "sieve.degree" = sieve.degree)

  # a list object is returned with elements `psi` and `weighted.score`
  psi_list <- .psiEst(data.rwe = data.rwe,
                      data.rct = data.rct,
                      outcome.type = outcome.type,
                      models = models)

  ### Variance estimation using resampling

  # a list object is returned with element V.est (vector length 4 * p);
  # eta (vector length p); V.rt (matrix p x p);
  # V.eff (matrix p x p); and sqrt.V.eff (matrix p x p)
  perm_result <- .perturbationProcedure(data.rwe = data.rwe,
                                        data.rct = data.rct,
                                        n.pert = n.pert,
                                        outcome.type = outcome.type,
                                        models = models)

  # a list object is returned with elements inv.Sigma.SS, sqrt.inv.Sigma.SS,
  # and sqrt.Sigma.SS each a p x p matrix
  Sigma_SS_matrices <- .calculateSigmaSSMatrices(V.rt = perm_result$V.rt,
                                                 V.eff = perm_result$V.eff,
                                                 rho = n_rct / n_rwe)

  sqrt_V_rt_eff <- perm_result$V.eff %*% Sigma_SS_matrices$sqrt.Sigma.SS

  # test statistic
  Tstat <- .calculateTstat(eta = perm_result$eta,
                           inv.Sigma.SS = Sigma_SS_matrices$inv.Sigma.SS)

  mu1 <- {Sigma_SS_matrices$sqrt.inv.Sigma.SS %*% perm_result$eta} |> drop()
  mu2 <- {perm_result$sqrt.V.eff %*% perm_result$eta} |> drop()

  # a list object is returned with elements gamma, c.gamma, Icomb, Icomb,pval,
  # and V.elastic
  nuispar <- .cGamma(mu1, mu2,
                     n.gamma = n.gamma,
                     sqrt.V.rt_eff = sqrt_V_rt_eff,
                     sqrt.V.eff = perm_result$sqrt.V.eff,
                     Tstat1 = Tstat,
                     n.rwe = n_rwe,
                     fixed = fixed)
  nuispar$eta <- perm_result$eta

  # a list object is returned with elements elastic and elastic.debiased
  est_bias <- .bias(Icomb = nuispar$Icomb,
                    psi.rt = psi_list$psi["rt", ],
                    psi.eff = psi_list$psi["eff", ],
                    mu1 = mu1,
                    gamma = nuispar$gamma,
                    eta = nuispar$eta,
                    V.eff = perm_result$V.eff,
                    n.rwe = n_rwe)

  # returns a list containing CI.inf, CI.sup, and CI.settings
  cis <- .bootFunc(mu1 = mu1,
                   mu2 = mu2,
                   c.gamma = nuispar$c.gamma,
                   V.rt = perm_result$V.rt,
                   sqrt.V.rt_eff = sqrt_V_rt_eff,
                   sqrt.V.eff = perm_result$sqrt.V.eff,
                   psi = psi_list$psi, ve = perm_result$V.est,
                   psi.elastic = est_bias$elastic,
                   n.rwe = n_rwe, n.boot = n.boot,
                   thres.psi = thres.psi,
                   Tstat = Tstat)

  # getting things ready to be returned
  psi <- rbind(psi_list$psi,
               "elastic" = est_bias$elastic,
               "elastic.debiased" = est_bias$elastic.debiased)
  ve <- rbind(perm_result$V.est,
              "elastic" = nuispar$V.elastic,
              "elastic.debiased" = nuispar$V.elastic)
  nuispar$V.elastic <- NULL

  cis$CIs.inf <- rbind(cis$CIs.inf,
                       "elastic.debiased" = cis$CIs.inf["elastic", ])
  cis$CIs.sup <- rbind(cis$CIs.sup,
                       "elastic.debiased" = cis$CIs.sup["elastic", ])

  obj <- c(list("call" = match.call(),
                "psi" = psi, "ve" = ve,
                "nuispar" = nuispar, "Tstat" = Tstat), cis)
  class(obj) <- c("elasticHTE", class(obj))
  obj
}
