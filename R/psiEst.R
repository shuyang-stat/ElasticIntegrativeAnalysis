#' Roots of the Score Function
#'
#' @noRd
#' @param X A data.frame or matrix object. The covariates. Columns must be named.
#' @param Y A numeric vector object. The outcome of interest.
#' @param A An integer vector object. The observed treatment.
#' @param wgt A numeric vector object. The case weights.
#' @param mu0 A numeric vector object. The estimated outcome of the A = 0 model.
#' @param ps A numeric vector object. The estimated psName score.
#' @param initial.guess A numeric vector object. The starting parameter values.
#' @param fit.name A character object. Used for printing error messages. Should
#'   uniquely describe the call.
#' @param outcome.type A character. The type of outcome. Must be one of
#'   \{"cont", "bin"\} indicating a continuous or binary outcome, respectively.
#'
#' @returns A numeric vector of the location of the root.
#'
#' @importFrom rootSolve multiroot
#' @include scores.R
#' @keywords internal
.rootsOfScore <- function(X, Y, A, wgt, mu0, ps, initial.guess, fit.name,
                          outcome.type) {

  stopifnot(
    "`X` must be a named matrix" = !missing(X) &&
      {.isNamedNumericMatrix(X) || ncol(X) == 0L},
    "`Y` must be provided" = !missing(Y),
    "`A` must be provided" = !missing(A),
    "`wgt` must be provided" = !missing(wgt),
    "`mu0` must be provided" = !missing(mu0),
    "`ps` must be provided" = !missing(ps),
    "`initial.guess` must be a named numeric vector" = !missing(initial.guess) &&
      .isNumericVector(initial.guess, ncol(X) + 1L) &&
      !is.null(names(initial.guess)),
    "`fit.name` must be a character object" = !missing(fit.name) &&
      .isCharacterVector(fit.name, 1L),
    "`outcome.type` must be one of 'cont' or 'bin'" = !missing(outcome.type) &&
      .isCharacterVector(outcome.type, 1L) && outcome.type %in% c("bin", "cont")
  )

  func <- switch(outcome.type,
                 "cont" = .score.cont,
                 "bin" = .score.binary,
                 stop("unrecognized outcome type"))

  tryCatch(rootSolve::multiroot(f = func,
                                start = initial.guess,
                                X = X,
                                Y = Y,
                                A = A,
                                wgt = wgt,
                                ps = ps,
                                mu0 = mu0)$root,
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

#' Data prep for psi Estimators
#'
#' @noRd
#' @param data A named list object. Must contain {X, Y, A, q, est.ps}
#' @param sieve.degree A scalar numeric object. The degree of the polynomial
#'   used to define the sieve model.
#' @param outcome.type A character. The type of outcome. Must be one of
#'   \{"cont", "bin"\} indicating a continuous or binary outcome, respectively.
#' @param mainName NULL or character vector. The covariates of the
#'   main effects component of the outcome model. If NULL,
#'   the main effects model is an intercept only model.
#' @param outcome.controls A list object. Any user specified inputs to
#'   SuperLearner::SuperLearner().
#' @param psName NULL or character vector. The covariates of the propensity
#'   score. If NULL, the propensity score model is an intercept only model.
#' @param ps.controls A list object. Any user specified inputs to
#'   SuperLearner::SuperLearner()
#' @param fit.name A character. Used to make error messages more informative.
#'
#' @returns A list. Elements $ps/$ml.ps contain the estimated psName scores
#'   and $mu0, $ml.mu0, $ml.sigma0, and $ml.mu1 the estimated outcomes
#'
#' @include outcome.R propensityScore.R
#' @keywords internal
.psiEstDataPrep <- function(data, sieve.degree,
                            mainName,
                            outcome.method,
                            outcome.controls,
                            psName,
                            ps.method,
                            ps.controls, fit.name) {

  stopifnot(
    "`data` must be a named list containing elements 'X', 'Y', 'A', 'q', and 'est.ps'" =
      !missing(data) && is.list(data) &&
      all(c("X", "Y", "A", "q", "est.ps") %in% names(data)),
    "`sieve.degree` must be provided" = !missing(sieve.degree),
    "`mainName` must be NULL or a character vector" = !missing(mainName) &&
      {is.null(mainName) ||
          {.isCharacterVector(mainName) && all(mainName %in% colnames(data$X))}},
    "`outcome.method` must be provided" = !missing(outcome.method),
    "`outcome.controls` must be provided" = !missing(outcome.controls),
    "`psName` must be NULL or a character vector" = !missing(psName) &&
      {is.null(psName) ||
          {.isCharacterVector(psName) && all(psName %in% colnames(data$X))}},
    "`ps.method` must be provided" = !missing(ps.method),
    "`ps.controls` must be provided" = !missing(ps.controls),
    "`fit.name` must be a character object" = !missing(fit.name) &&
      .isCharacterVector(fit.name, 1L)
  )

  # estimate propensity score
  # function returns a list containing elements $ps and $ml.ps
  if (data$est.ps) {
    if (length(unique(data$A)) > 1L) {
      ps <- tryCatch(.propensityScore(X = data$X[, psName, drop = FALSE],
                                      A = data$A,
                                      wgt = data$q,
                                      sieve.degree = sieve.degree,
                                      method = ps.method,
                                      method.controls = ps.controls,
                                      models = c("ps", "ml.ps")),
                     error = function(e) {
                       stop("unable to estimate parameters for ", fit.name, "\n\t",
                            e$message, call. = FALSE)
                     })
      data[names(ps)] <- ps
    } else {
      message("All treatments are the same, propensity set as 1")
      data$ps <- rep(1.0, nrow(data$X))
      data$ml.ps <- rep(1.0, nrow(data$X))
    }
  } else {
    if (is.null(data$ps)) {
      stop("ps must be provided for ", fit.name, call. = FALSE)
    }
    data$ml.ps <- data$ps
  }

  # need to remove this for later mapply -- names of the data.x lists
  # must match
  data$est.ps <- NULL

  # outcome fitted values and dispersion
  # function returns a list containing elements $mu0, $ml.mu0, $ml.sigma0, and
  # $ml.mu1

  outcome <- tryCatch(.outcome(X = data$X[, mainName, drop = FALSE],
                               Y = data$Y,
                               A = data$A, wgt = data$q,
                               sieve.degree = sieve.degree,
                               method = outcome.method,
                               method.controls = outcome.controls),
                      error = function(e) {
                        stop("unable to estimate parameters for ", fit.name, "\n\t",
                             e$message, call. = FALSE)
                        })
  data[names(outcome)] <- outcome

  data
}





#' psi Estimators
#'
#' @noRd
#' @param data.rct A named list object. Must contain {X, Y, A, q, est.ps}. Data
#'   for the RCT.
#' @param data.rwe A named list object. Must contain {X, Y, A, q}. Data
#'   for the RWE.
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
#' @returns A list. Element $psi is a 4 x {p+1} matrix containing the
#'   preliminary, efficient integrative and RCT estimators. Element
#'   $weighted.score returns Shat_{RWE, psi_rt}.
#'
#' @include outcome.R propensityScore.R
#' @keywords internal
.psiEst <- function(data.rct, data.rwe, outcome.type, models) {

  stopifnot(
    "`data.rct` must be a named list containing elements 'X', 'Y', 'A', 'q', and 'est.ps'" =
      !missing(data.rct) && is.list(data.rct) &&
      all(c("X", "Y", "A", "q", "est.ps") %in% names(data.rct)),
    "`data.rwe` must be a named list containing elements 'X', 'Y', 'A', and 'q'" =
      !missing(data.rwe) && is.list(data.rwe) &&
      all(c("X", "Y", "A", "q") %in% names(data.rwe)),
    "`outcome.type` must be one of 'cont' or 'bin'" = !missing(outcome.type) &&
      .isCharacterVector(outcome.type, 1L) && outcome.type %in% c("bin", "cont"),
    "`models` must be a list with elements 'RWE', 'RCT', 'outcome', 'ps', 'sieve.degree', and 'contName'" =
      !missing(models) && is.vector(models, mode = "list") &&
      all(c("RWE", "RCT", "outcome", "ps", "sieve.degree", "contName") %in% names(models)),
    "elements 'RWE' and 'RCT' of 'models' must be lists with elements 'ME' and 'PS'" =
      is.vector(models$RWE, mode = "list") && is.vector(models$RCT, mode = "list") &&
      all(c("ME", "PS") %in% names(models$RWE)) && all(c("ME", "PS") %in% names(models$RCT)),
    "elements 'outcome' and 'ps' of 'models' must be lists with elements 'method' and 'controls'" =
      is.vector(models$outcome, mode = "list") && is.vector(models$ps, mode = "list") &&
      all(c("method", "controls") %in% names(models$outcome)) &&
      all(c("method", "controls") %in% names(models$ps))
  )

  data.rwe$est.ps <- TRUE

  data.rwe <- .psiEstDataPrep(data = data.rwe,
                              sieve.degree = models$sieve.degree,
                              mainName = models$RWE$ME,
                              outcome.method = models$outcome$method,
                              outcome.controls = models$outcome$controls,
                              psName = models$RWE$PS,
                              ps.method = models$ps$method,
                              ps.controls = models$ps$controls,
                              fit.name = "RWE")

  data.rct <- .psiEstDataPrep(data = data.rct,
                              sieve.degree = models$sieve.degree,
                              mainName = models$RCT$ME,
                              outcome.method = models$outcome$method,
                              outcome.controls = models$outcome$controls,
                              psName = models$RCT$PS,
                              ps.method = models$ps$method,
                              ps.controls = models$ps$controls,
                              fit.name = "RCT")

  # ensure that the two lists are in the same order and combine
  data.rwe <- data.rwe[names(data.rct)]
  data.integ <- mapply(data.rct, data.rwe,
                       FUN = function(x, y) {
                         if (is.matrix(x)) {
                           rbind(x, y)
                         } else {
                           c(x, y)
                         }
                       })

  psi <- matrix(NA, nrow = 3, ncol = length(models$contName) + 1L,
                dimnames = c(list(c("p", "eff", "rt"),
                                  c("(Intercept)", models$contName))))

  par <- numeric(length(models$contName) + 1L)
  names(par) <- c("(Intercept)", models$contName)

  psi["p", ] <- .rootsOfScore(X = data.integ$X[, models$contName, drop = FALSE],
                              Y = data.integ$Y,
                              A = data.integ$A,
                              wgt = data.integ$q,
                              mu0 = data.integ$mu0,
                              ps = data.integ$ps,
                              initial.guess = par,
                              fit.name = "Preliminary Estimator psi_p",
                              outcome.type = outcome.type)
  par[] <- psi["p", ]

  psi["eff", ] <- .rootsOfScore(X = data.integ$X[, models$contName, drop = FALSE],
                                Y = data.integ$Y,
                                A = data.integ$A,
                                wgt = data.integ$q,
                                mu0 = data.integ$ml.mu0,
                                ps = data.integ$ml.ps,
                                initial.guess = par,
                                fit.name = "Efficient Integrative Estimator psi_eff",
                                outcome.type = outcome.type)

  psi["rt", ] <- .rootsOfScore(X = data.rct$X[, models$contName, drop = FALSE],
                               Y = data.rct$Y,
                               A = data.rct$A,
                               wgt = data.rct$q,
                               mu0 = data.rct$ml.mu0,
                               ps = data.rct$ml.ps,
                               initial.guess = par,
                               fit.name = "Initial Estimator psi_rt",
                               outcome.type = outcome.type)

  par[] <- psi["rt", ]
  weighted_score <- .evaluatedScore(data = data.rwe, psi = par,
                                    outcome.type = outcome.type,
                                    contName = models$contName)

  list("psi" = psi, "weighted.score" = weighted_score)
}
