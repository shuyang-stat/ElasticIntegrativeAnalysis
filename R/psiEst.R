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
#' @param mainName A character vector. The covariates of the main effects
#'   component of the outcome model.
#' @param contName A character vector. The covariates of the contrasts
#'   component of the outcome model.
#' @param outcome.method A character object. Must be one of 'glm' or 'sl'
#'   indicating if stats::glm() or SuperLearner::SuperLearner() should be
#'   used to obtain outcome model parameter estimates.
#' @param outcome.controls A list object. Any user specified inputs to
#'   `outcome.method`
#' @param ps.method A character object. Must be one of 'glm' or 'sl'
#'   indicating if stats::glm() or SuperLearner::SuperLearner() should be
#'   used to obtain psName score parameter estimates.
#' @param ps.controls A list object. Any user specified inputs to
#'   `ps.method`
#' @param fit.name A character. Used to make error messages more informative.
#'
#' @returns A list. Elements $ps/$ml.ps contain the estimated psName scores
#'   and $mu0, $ml.mu0, $ml.sigma0, and $ml.mu1 the estimated outcomes
#'
#' @include outcome.R propensityScore.R
#' @keywords internal
.psiEstDataPrep <- function(data, sieve.degree,
                            mainName, contName,
                            outcome.method, outcome.controls,
                            psName,
                            ps.method, ps.controls, fit.name) {

  stopifnot(
    "`data` must be a named list containing elements 'X', 'Y', 'A', 'q', and 'est.ps'" =
      !missing(data) && is.list(data) &&
      all(c("X", "Y", "A", "q", "est.ps") %in% names(data)),
    "`sieve.degree` must be provided" = !missing(sieve.degree),
    "`mainName` must be NULL or a character vector" = !missing(mainName) &&
      {is.null(mainName) ||
          {.isCharacterVector(mainName) && all(mainName %in% colnames(data$X))}},
    "`contName` must be NULL or a character vector" = !missing(contName) &&
      {is.null(contName) ||
          {.isCharacterVector(contName) && all(contName %in% colnames(data$X))}},
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
#' @param sieve.degree A scalar numeric object. The degree of the polynomial
#'   used to define the sieve model.
#' @param outcome.type A character. The type of outcome. Must be one of
#'   \{"cont", "bin"\} indicating a continuous or binary outcome, respectively.
#' @param mainName NULL, character vector or an integer. The covariates of the
#'   main effects component of the outcome model. If NULL, all covariates in
#'   `data.rct$X` are included; if a character vector, the column headers of
#'   `data.rct$X` to include in the model. Note that an intercept is always
#'   added; though it is not recommended, an intercept only model can be
#'   specified as `mainName = 1`.
#' @param contName NULL, character vector or an integer. The covariates of the
#'   contrasts component of the outcome model. If NULL, all covariates in
#'   `data.rct$X` are included; if a character vector, the column headers of
#'   `data.rct$X` to include in the model. Note these are the covariates that
#'   interact with the treatment variable (~ A:contName); an intercept is always
#'   included, such that ~A is the minimal model; though it is not recommended,
#'   an intercept only model can be specified as `contName = 1`.
#' @param outcome.method A character object. Must be one of 'glm' or 'sl'
#'   indicating if stats::glm() or SuperLearner::SuperLearner() should be
#'   used to obtain outcome model parameter estimates.
#' @param outcome.controls A list object. Any user specified inputs to
#'   `outcome.method`
#' @param ps.method A character object. Must be one of 'glm' or 'sl'
#'   indicating if stats::glm() or SuperLearner::SuperLearner() should be
#'   used to obtain psName score parameter estimates.
#' @param ps.controls A list object. Any user specified inputs to
#'   `ps.method`
#'
#' @returns A list. Element $psi is a 4 x {p+1} matrix containing the
#'   preliminary, efficient integrative and RCT estimators. Element
#'   $weighted.score returns Shat_{RWE, psi_rt}.
#'
#' @include outcome.R propensityScore.R
#' @keywords internal
.psiEst <- function(data.rct, data.rwe, sieve.degree,
                    outcome.type,
                    mainName, contName,
                    outcome.method, outcome.controls,
                    psName,
                    ps.method, ps.controls) {

  stopifnot(
    "`data.rct` must be a named list containing elements 'X', 'Y', 'A', 'q', and 'est.ps'" =
      !missing(data.rct) && is.list(data.rct) &&
      all(c("X", "Y", "A", "q", "est.ps") %in% names(data.rct)),
    "`data.rwe` must be a named list containing elements 'X', 'Y', 'A', and 'q'" =
      !missing(data.rwe) && is.list(data.rwe) &&
      all(c("X", "Y", "A", "q") %in% names(data.rwe)),
    "`sieve.degree` must be provided" = !missing(sieve.degree),
    "`outcome.type` must be one of 'cont' or 'bin'" = !missing(outcome.type) &&
      .isCharacterVector(outcome.type, 1L) && outcome.type %in% c("bin", "cont"),
    "`mainName` must be NULL or a character vector" = !missing(mainName) &&
      {is.null(mainName) ||
      {.isCharacterVector(mainName) && all(mainName %in% colnames(data.rct$X)) &&
          all(mainName %in% colnames(data.rwe$X))}},
    "`contName` must be NULL or a character vector" = !missing(contName) &&
      {is.null(contName) ||
          {.isCharacterVector(contName) && all(contName %in% colnames(data.rct$X)) &&
          all(contName %in% colnames(data.rwe$X))}},
    "`outcome.method` must be provided" = !missing(outcome.method),
    "`outcome.controls` must be provided" = !missing(outcome.controls),
    "`psName` must be NULL or a character vector" = !missing(psName) &&
      {is.null(psName) ||
          {.isCharacterVector(psName) && all(psName %in% colnames(data.rct$X)) &&
              all(psName %in% colnames(data.rwe$X))}},
    "`ps.method` must be provided" = !missing(ps.method),
    "`ps.controls` must be provided" = !missing(ps.controls)
  )

  data.rwe$est.ps <- TRUE
  data.rwe <- .psiEstDataPrep(data = data.rwe,
                              sieve.degree = sieve.degree,
                              mainName = mainName,
                              contName = contName,
                              outcome.method = outcome.method,
                              outcome.controls = outcome.controls,
                              psName = psName,
                              ps.method = ps.method,
                              ps.controls = ps.controls,
                              fit.name = "RWE")

  data.rct <- .psiEstDataPrep(data = data.rct,
                              sieve.degree = sieve.degree,
                              mainName = mainName,
                              contName = contName,
                              outcome.method = outcome.method,
                              outcome.controls = outcome.controls,
                              psName = psName,
                              ps.method = ps.method,
                              ps.controls = ps.controls,
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

  psi <- matrix(NA, nrow = 3, ncol = length(contName) + 1L,
                dimnames = c(list(c("p", "eff", "rt"),
                                  c("(Intercept)", contName))))

  par <- numeric(length(contName) + 1L)
  names(par) <- c("(Intercept)", contName)

  psi["p", ] <- .rootsOfScore(X = data.integ$X[, contName, drop = FALSE],
                              Y = data.integ$Y,
                              A = data.integ$A,
                              wgt = data.integ$q,
                              mu0 = data.integ$mu0,
                              ps = data.integ$ps,
                              initial.guess = par,
                              fit.name = "Preliminary Estimator psi_p",
                              outcome.type = outcome.type)
  par[] <- psi["p", ]

  psi["eff", ] <- .rootsOfScore(X = data.integ$X[, contName, drop = FALSE],
                                Y = data.integ$Y,
                                A = data.integ$A,
                                wgt = data.integ$q,
                                mu0 = data.integ$ml.mu0,
                                ps = data.integ$ml.ps,
                                initial.guess = par,
                                fit.name = "Efficient Integrative Estimator psi_eff",
                                outcome.type = outcome.type)

  psi["rt", ] <- .rootsOfScore(X = data.rct$X[, contName, drop = FALSE],
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
                                    contName = contName)

  list("psi" = psi, "weighted.score" = weighted_score)
}
