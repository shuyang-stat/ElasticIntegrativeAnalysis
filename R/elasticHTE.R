#' Elastic Integrative Analysis for Heterogeneous Treatment Effect
#'
#'@description
#' A test-based dynamic borrowing framework combining
#'   a randomized clinical trial (RCT) and a real-world evidence (RWE) study,
#'   in which a preliminary test statistic is used to gauge the comparability
#'   and reliability of the RWE and to decide whether or not to use the RWE in
#'   an integrative analysis. The parameter of interest is \eqn{\psi}{`psi`},
#'   which quantifies how the treatment effect varies over the treatment
#'   modifiers.
#'
#'@details
#' Inputs \code{data.rct} and \code{data.rwe} are most easily specified using
#'   the provided convenience function \link{dataInput}(). However, this is
#'   not required. See \link{dataInput}() for details of the returned
#'   object.
#'
#'@note
#'   When specifying \code{outcome.controls} and \code{ps.controls}, some
#'   input arguments cannot be accessed. Specifically, formal arguments
#'   \code{Y}, \code{X}, \code{newX}, and  \code{obsWeight} of
#'   \code{SuperLearner::SuperLearner()} and \code{formula}, \code{data}, and
#'   \code{weights} of \code{stats::glm()} cannot be set through these inputs.
#'
#' @param data.rct The value object returned by \code{dataInput()} for the
#'   data from a randomized clinical trial (RCT). See \link{dataInput} for
#'   further details.
#' @param data.rwe The value object returned by \code{dataInput()} for the
#'   data from a real-world evidence (RWE) study. See \link{dataInput} for
#'   further details. Note that the treatment effect model must be identical
#'   to that of \code{data.rct}.
#' @param ... Ignored. Included to require named inputs.
#' @param ps.rct NULL or a numeric vector. Optional input providing a vector of
#'   known propensity scores P(A=1) for the RCT dataset. If not provided,
#'   it will be estimated using the model defined in \code{data.rct$psName}.
#' @param thres.psi NULL or a scalar numeric. The threshold for constructing
#'   adaptive confidence intervals. If NULL, a default value of
#'   \eqn{\sqrt{\log(n)}}{sqrt(log(n))}, where n is the number of participants
#'   in \code{data.rwe}, is used.
#' @param sieve.degree A positive integer > 1. The order of the polynomial
#'   defining the sieve model. For example, `sieve.degree = 2` yields outcome and
#'   propensity models that include all covariates, the squared covariates, and
#'   all pair-wise interactions.
#' @param outcome.type A character. The type of outcome. Must be one of
#'   \{"cont", "bin"\} indicating a continuous or binary outcome, respectively.
#' @param outcome.method A character. The regression method for outcomes.
#'   Must be one of \{"glm", "SL"\}. The outcome is modeled without adjustment,
#'   so method and family must be appropriate for the outcome.type.
#' @param outcome.controls A named list. Additional inputs provided to
#'   \code{stats::glm()} or
#'   \code{SuperLearner::SuperLearner()} for the outcome regression analyses.
#'   Element names must match the formal arguments of \code{stats::glm()} or
#'   \code{SuperLearner::SuperLearner()} and should include, at a minimum,
#'   element "family". Please see ?stats::glm or ?SuperLearner::SuperLearner
#'   for additional input options.
#' @param ps.method A character. The regression method for propensity score
#'   analysis. Must be one of \{"glm", "SL"\}.
#' @param ps.controls A named list. Additional inputs provided to
#'   \code{stats::glm()} or
#'   \code{SuperLearner::SuperLearner()} for the propensity score regression analyses.
#'   Element names must match the formal arguments of \code{stats::glm()} or
#'   \code{SuperLearner::SuperLearner()} and should include, at a minimum,
#'   element "family". Please see ?stats::glm or ?SuperLearner::SuperLearner
#'   for additional input options.
#' @param fixed A logical. How to select the tuning parameter
#'   \eqn{c_{\gamma}}{c_gamma}. FALSE, the default, selects an adaptive
#'   selection strategy; TRUE selects a fixed threshold strategy.
#'   The default fixed threshold is \code{stats::qchisq(0.95, df = p)},
#'   in which p is the dimension of the treatment effect model.
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
#'     and \eqn{\psi_{elastic}}{`psi_elastic`}).}
#'   \item{ve }{A matrix containing the estimated standard error for
#'     \eqn{\psi}{`psi`}.}
#'   \item{CIs.inf, CIs.sup }{A matrix containing the estimated confidence
#'     intervals for \eqn{\psi}{`psi`}.}
#'   \item{CI.settings }{A list of the settings used in the confidence interval
#'     procedure.}
#'   \item{Tstat }{The estimated test statistic.}
#'   \item{conservative }{A logical \eqn{I(Tstat < thres.psi)}{I(Tstat < thres.psi)} }
#'   \item{nuispar }{A list providing the selected \eqn{\gamma}{gamma} and
#'     its corresponding threshold value \eqn{c_{\gamma}}{c.gamma};
#'     indicator \eqn{I(c_{\gamma}  > Tstat)}{I(c.gamma > Tstat)} and its
#'     p-value; eta, where \eqn{Tstat = \eta^T \eta}{Tstat = eta^T eta};
#'     and a list of the settings used in the selection procedure.}
#'
#' @examples
#' # Note that n.gamma and n.pert are smaller than recommended to accommodate
#' # fast examples.
#' #
#' # load provided illustrative toy dataset with continuous outcome
#' data("elasticToy.cont")
#'
#' # conduct the elastic integrative analysis with defaults
#' result.cont <- elasticHTE(data.rct = dataInput(elasticToy.cont.rct,
#'                                                outcome.model = Y ~ (X1+X2)*A,
#'                                                ps.model = A ~ X1 + X2),
#'                           data.rwe = dataInput(elasticToy.cont.rwe,
#'                                                outcome.model = Y ~ (X1+X2)*A,
#'                                                ps.model = A ~ X1 + X2),
#'                           n.boot = 0L, n.gamma = 10L, n.pert = 10L)
#'
#' # load provided illustrative toy dataset with binary outcome
#' data("elasticToy.bin")
#'
#' # conduct the elastic integrative analysis with defaults
#' result.bin <- elasticHTE(data.rct = dataInput(elasticToy.bin.rct,
#'                                               outcome.model = Y ~ (X1+X2)*A,
#'                                               ps.model = A ~ X1 + X2),
#'                          data.rwe = dataInput(elasticToy.bin.rwe,
#'                                               outcome.model = Y ~ (X1+X2)*A,
#'                                               ps.model = A ~ X1 + X2),
#'                          outcome.type = "bin",
#'                          n.boot = 0L, n.gamma = 10L, n.pert = 10L)
#'
#' @importFrom stats complete.cases
#' @include bias.R bootFunc.R cGamma.R dataInput.R perturbationEst.R psiEst.R
#' @include sigma_matrices.R stopTests.R utils.R
#' @export
elasticHTE <- function(data.rct,
                       data.rwe,
                       ...,
                       outcome.type = c("cont", "bin"),
                       ps.rct = NULL,
                       sieve.degree = 2L,
                       outcome.method = c("glm", "SL"),
                       outcome.controls = list("family" = "gaussian"),
                       ps.method = c("glm", "SL"),
                       ps.controls = list("family" = "quasibinomial"),
                       n.pert = 100L,
                       fixed = FALSE,
                       n.gamma = 1000L,
                       n.boot = 100L,
                       thres.psi = NULL) {

  outcome.type <- match.arg(outcome.type)
  outcome.method <- match.arg(outcome.method)
  ps.method <- match.arg(ps.method)

  stopifnot(
    "`data.rct` must be provided" = !missing(data.rct),
    "`data.rwe` must be provided" = !missing(data.rwe)
  )

  # ensure that provided data match expected structure
  .isDI(data.rct, "data.rct")
  .isDI(data.rwe, "data.rwe")

  stopifnot(
    "`data.rct$contName` must match `data.rwe$contName`" =
      isTRUE(all.equal(data.rct$contName, data.rwe$contName)),
    "`ps.rct` must be NULL or a numeric vector of length = nrow(data.rct$X)" =
      is.null(ps.rct) || .isNumericVector(ps.rct, nrow(data.rct$X)),
    "`thres.psi` must be a positive scalar" = is.null(thres.psi) ||
      {.isNumericVector(thres.psi, 1L) && thres.psi > 0.0},
    "`sieve.degree` must be a positive integer > 1" = .isNumericVector(sieve.degree, 1L) &&
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
      isTRUE(all.equal(n.boot, round(n.boot, 0L))) && n.boot >= 0,
    "`n.gamma` must be a positive integer" = .isNumericVector(n.gamma, 1L) &&
      isTRUE(all.equal(n.gamma, round(n.gamma, 0L))) && n.gamma > 0
  )

  # set default family values based on outcome type if not provided
  if (is.null(outcome.controls$family)) {
    outcome.controls$family <- switch(outcome.type,
                                      "cont" = "gaussian",
                                      "bin" = "quasibinomial")
  }

  # set default family value for propensity model if not provided
  if (is.null(ps.controls$family)) ps.controls$family <- "quasibinomial"

  # NULL input means "all covariates in X"; integer input means "intercept only"
  mainName.rct <- .adjustModelCoding(data.rct$mainName, colnames(data.rct$X))
  mainName.rwe <- .adjustModelCoding(data.rwe$mainName, colnames(data.rwe$X))
  contName <- .adjustModelCoding(data.rct$contName, colnames(data.rct$X))
  psName.rct <- .adjustModelCoding(data.rct$psName, colnames(data.rct$X))
  psName.rwe <- .adjustModelCoding(data.rwe$psName, colnames(data.rwe$X))

  # remove these from the data.objects to conform to original
  # expectations
  data.rct[c("mainName", "contName", "psName")] <- NULL
  data.rwe[c("mainName", "contName", "psName")] <- NULL

  # reduce dataset down to only those covariates used in models
  if (is.null(mainName.rct) && is.null(contName) && is.null(psName.rct)) {
    # if all are null, intercept only models are requested
    data.rct$X <- matrix(NA, nrow(data.rct$X), 0L)
  } else {
    # only need one copy of the covariates
    all_cov <- unique(c(mainName.rct, contName, psName.rct))

    if (!all(all_cov %in% colnames(data.rct$X))) {
      stop("not all model covariates are found in provided data\n\t",
           "model covariates: ", paste(all_cov, collapse = ", "), "\n\t",
           "data.rct: ", paste(colnames(data.rct$X), collapse = ", "), "\n\t", call. = FALSE)
    }

    data.rct$X <- data.rct$X[, all_cov, drop = FALSE]

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
    # if all are null, intercept only models are requested
    data.rwe$X <- matrix(NA, nrow(data.rwe$X), 0L)
  } else {
    # only need one copy of the covariates
    all_cov <- unique(c(mainName.rwe, contName, psName.rwe))

    if (!all(all_cov %in% colnames(data.rwe$X))) {
      stop("not all model covariates are found in provided data\n\t",
           "model covariate: ", paste(all_cov, collapse = ", "), "\n\t",
           "data.rwe: ", paste(colnames(data.rwe$X), collapse = ", "), "\n\t",
           call. = FALSE)
    }

    data.rwe$X <- data.rwe$X[, all_cov, drop = FALSE]

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

  data.rct$ps <- ps.rct

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

  est_bias <- .bias(Icomb = nuispar$Icomb,
                    psi.rt = psi_list$psi["rt", ],
                    psi.eff = psi_list$psi["eff", ],
                    mu1 = mu1,
                    gamma = nuispar$gamma,
                    eta = perm_result$eta,
                    V.eff = perm_result$V.eff,
                    n.rwe = n_rwe)

  if (n.boot > 0L) {
    # returns a list containing CI.inf, CI.sup, and CI.settings
    cis <- .bootFunc(mu1 = mu1,
                     mu2 = mu2,
                     c.gamma = nuispar$c.gamma,
                     V.rt = perm_result$V.rt,
                     sqrt.V.rt_eff = sqrt_V_rt_eff,
                     sqrt.V.eff = perm_result$sqrt.V.eff,
                     psi = psi_list$psi, ve = perm_result$V.est,
                     psi.elastic = est_bias,
                     n.rwe = n_rwe, n.boot = n.boot,
                     thres.psi = thres.psi,
                     Tstat = Tstat)
  } else {
    cis <- list("CIs.inf" = NA_real_, "CIs.sup" = NA_real_)
  }

  # getting things ready to be returned
  psi <- rbind(psi_list$psi,
               "elastic" = est_bias)
  ve <- rbind(perm_result$V.est,
              "elastic" = nuispar$V.elastic)
  nuispar$V.elastic <- NULL
  nuispar$eta <- {perm_result$eta %*% Sigma_SS_matrices$sqrt.inv.Sigma.SS} |> drop()

  obj <- c(list("call" = match.call(),
                "psi" = psi, "ve" = ve,
                "nuispar" = nuispar, "Tstat" = Tstat), cis)
  class(obj) <- c("elasticHTE", class(obj))
  obj
}
