#' Data prep for psi Estimators
#'
#' @noRd
#' @param data A named list object. Must contain {X, Y, A, q, est.ps}
#' @param sieve.degree A scalar numeric object. The degree of the polynomial
#'   used to define the sieve model.
#' @param mainName NULL or character vector. The covariates of the
#'   main effects component of the outcome model. If NULL,
#'   the main effects model is an intercept only model.
#' @param outcome.method A character. The regression method for outcomes.
#'   Must be one of \{'glm', 'SL'\}.
#' @param outcome.controls A list object. Any user specified inputs to
#'   outcome.method.
#' @param psName NULL or character vector. The covariates of the propensity
#'   score. If NULL, the propensity score model is an intercept only model.
#' @param ps.method A character. The regression method for propensity score.
#'   Must be one of \{'glm', 'SL'\}.
#' @param ps.controls A list object. Any user specified inputs to ps.method.
#' @param fit.name A character. Used to make error messages more informative.
#' @param outcome.type A character. The type of outcome. Must be one of
#'   \{"cont", "bin"\} indicating a continuous or binary outcome, respectively.
#'
#' @returns A list. Elements $ps/$ml.ps contain the estimated psName scores
#'   and $me0, $ml.me0, $ml.me0.conditional.var, and $ml.me1 the estimated outcomes
#'
#' @include outcome.R propensityScore.R
#' @keywords internal
.psiEstDataPrep <- function(data, sieve.degree,
                            mainName,
                            outcome.method,
                            outcome.controls,
                            psName,
                            ps.method,
                            ps.controls, fit.name,
                            outcome.type) {

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
      .isCharacterVector(fit.name, 1L),
    "`outcome.type` must be provided" = !missing(outcome.type)
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

  # outcome fitted values and conditional variance
  # function returns a list containing elements $me0, $ml.me0,
  # $ml.me0.conditional.var, $ml.me1, $inv.sig2, and $ml.inv.sig2
  outcome <- tryCatch(.outcome(X = data$X[, mainName, drop = FALSE],
                               Y = data$Y,
                               A = data$A,
                               wgt = data$q,
                               sieve.degree = sieve.degree,
                               method = outcome.method,
                               method.controls = outcome.controls,
                               outcome.type = outcome.type),
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
#' @include outcome.R propensityScore.R scores.R
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
                              fit.name = "RWE",
                              outcome.type = outcome.type)

  data.rct <- .psiEstDataPrep(data = data.rct,
                              sieve.degree = models$sieve.degree,
                              mainName = models$RCT$ME,
                              outcome.method = models$outcome$method,
                              outcome.controls = models$outcome$controls,
                              psName = models$RCT$PS,
                              ps.method = models$ps$method,
                              ps.controls = models$ps$controls,
                              fit.name = "RCT",
                              outcome.type = outcome.type)

  # ensure that the two lists are in the same order and combine
  data.rwe <- data.rwe[names(data.rct)]

  # we have the possibility that RCT and RWE have different X matrices now.
  # They will always have common contName, and this is all we need at this
  # point. match() logic removes names that are not common across the
  # two sets of data; it does not limit it to just the contName covariates.
  data.integ <- mapply(data.rct, data.rwe,
                       FUN = function(x, y) {
                         if (is.matrix(x)) {
                           idx <- match(colnames(x), colnames(y))
                           rbind(x[, !is.na(idx), drop = FALSE],
                                 y[, idx[!is.na(idx)], drop = FALSE])
                         } else {
                           c(x, y)
                         }
                       }, SIMPLIFY = FALSE)

  # +1 is for the intercept
  psi <- matrix(NA, nrow = 3L, ncol = length(models$contName) + 1L,
                dimnames = c(list(c("p", "eff", "rt"),
                                  c("(Intercept)", models$contName))))

  par <- numeric(length(models$contName) + 1L)
  names(par) <- c("(Intercept)", models$contName)

  # initial estimator does not include estimated variance
  psi["p", ] <- .rootsOfScore(X = data.integ$X[, models$contName, drop = FALSE],
                              Y = data.integ$Y,
                              A = data.integ$A,
                              outcome.type = outcome.type,
                              mu = data.integ$me0,
                              ps = data.integ$ps,
                              inv.sig2 = rep(1.0, nrow(data.integ$X)),
                              wgt = data.integ$q,
                              initial.guess = par,
                              score.func = "basic",
                              fit.name = "Preliminary Estimator psi_p")
  par[] <- psi["p", ]

  # effective estimator does not include estimated variance
  psi["eff", ] <- .rootsOfScore(X = data.integ$X[, models$contName, drop = FALSE],
                                Y = data.integ$Y,
                                A = data.integ$A,
                                outcome.type = outcome.type,
                                mu = data.integ$ml.me0,
                                ps = data.integ$ml.ps,
                                inv.sig2 = rep(1.0, nrow(data.integ$X)),
                                wgt = data.integ$q,
                                initial.guess = par,
                                score.func = "basic",
                                fit.name = "Efficient Integrative Estimator psi_eff")

  psi["rt", ] <- .rootsOfScore(X = data.rct$X[, models$contName, drop = FALSE],
                               Y = data.rct$Y,
                               A = data.rct$A,
                               outcome.type = outcome.type,
                               mu = data.rct$ml.me0,
                               ps = data.rct$ml.ps,
                               inv.sig2 = data.rct$ml.inv.sig2,
                               wgt = data.rct$q,
                               initial.guess = par,
                               score.func = "basic",
                               fit.name = "Efficient Estimator psi_rt")

  par[] <- psi["rt", ]
  weighted_score <- .score.no.confounding(psi = par,
                                          X = data.rwe$X[, models$contName, drop = FALSE],
                                          Y = data.rwe$Y,
                                          A = data.rwe$A,
                                          outcome.type = outcome.type,
                                          mu = data.rwe$ml.me0,
                                          ps = data.rwe$ml.ps,
                                          inv.sig2 = data.rwe$ml.inv.sig2,
                                          wgt = data.rwe$q)


  list("psi" = psi, "weighted.score" = weighted_score)
}
