#' @param data.rct A named list containing elements "X", "Y", and "A" for an RCT.
#'   Element X is a covariate matrix and should not include an intercept.
#'   Element Y is a response vector. Element A is a binary treatment vector.
#'   Element A can be factor or integer.
#'   List can also contain element "ps", a vector of propensity scores P(A=1).
#' @param data.rwe A named list containing elements "X", "Y", and "A" for an RWE
#'   study.
#'   Element X is a covariate matrix and should not include an intercept.
#'   Element Y is a response vector. Element A is a binary treatment vector.
#'   Element A can be factor or integer.
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
#' @param propensity NULL, character vector or an integer. The covariates of the
#'   propensity score model. If NULL, all covariates in
#'   `data.rct$X` are included; if a character vector, the column headers of
#'   `data.rct$X` to include in the model. Note that an intercept only model
#'   can be specified as `propensity = 1`.
#' @param thres.psi NULL or a scalar numeric. The threshold for constructing
#'   adaptive confidence interval. STH QUESTION: Is there an allowed range we
#'   should mention?
#' @param sieve.degree A positive integer. The order of the polynomial defining
#'   the sieve model. For example, `sieve.degree = 2` yields outcome and
#'   propensity models that include all covariates, the squared covariates, and
#'   all pair-wise interactions.
#' @param outcome A character. The type of outcome. Must be one of
#'   \{"cont", "bin"\} indicating a continuous or binary outcome, respectively.
#' @param outcome.method A character. The regression tool to be used for
#'   estimating the parameters of the outcome model. Must be one of
#'   \{"glm", "sl"\} indicating the stats::glm() function or the
#'   SuperLearner::SuperLearner() function.
#' @param outcome.controls A named list. Additional inputs provided to the
#'   outcome regression tool. Element names must match the formal arguments
#'   of the chosen outcome.method. Should include, at a minimum, element
#'   "family" indicating the error distribution. Please see documentation
#'   of the selected regression tool for additional input options.
#'   With the exception of Y, X, newX, and obsWeights, all inputs of
#'   SuperLearner::SuperLearner() can be modified. Similarly, all inputs beyond
#'   formula, data, and weights can be specified for stats::glm().
#' @param ps.method A character. The regression tool to be used for
#'   estimating the parameters of the propensity score model. Must be one of
#'   \{"glm", "sl"\} indicating the stats::glm() function or the
#'   SuperLearner::SuperLearner() function.
#' @param ps.controls A named list. Additional inputs provided to the
#'   propensity score regression tool. Element names must match the formal
#'   arguments of the chosen ps.method. Should include, at a minimum, element
#'   "family" indicating the error distribution. Please see documentation
#'   of the selected regression tool for additional input options.
#'   With the exception of Y, X, newX, and obsWeights, all inputs of
#'   SuperLearner::SuperLearner() can be modified. Similarly, all inputs beyond
#'   formula, data, and weights can be specified for stats::glm().
#' @param fixed A logical. How to select the tuning parameter `c.gamma`.
#'   * `FALSE` (the default): use adaptive selection strategy
#'   * `TRUE`: use fixed threshold strategy.
#'   The default fixed threshold is \code{stats::qchisq(0.95, df = p)},
#'   in which `p` is the dimension of the contrasts model.
#' @param n.pert An integer. The number of perturbations to use when
#'   estimating the variance.
#' @param n.boot An integer. The number of bootstrap samples to generate
#'   when estimating the confidence intervals.
