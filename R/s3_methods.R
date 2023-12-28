#' @describeIn elasticHTE Print summary of analysis
#' @param x An object of S3 class \code{elasticHTE}.
#' @param ... Ignored
#'
#' @export
print.elasticHTE <- function(x, ...) {

  res <- summary(x)

  cat("\npsi_elastic: \n")
  print(format(res$psi$elastic, digits = 3L))
  cat("\nTest statistic: ", format(res$Tstat, digits = 3L), "\n")
  cat("gamma: ", format(res$nuispar$gamma[1L], digits = 3L),
      "  c.gamma: ", format(res$nuispar$c.gamma[1L], digits = 3L), "\n")
  cat("\neta: \n")
  print(round(res$nuispar$eta, digits = 3L))
  cat("\n")

  invisible(x)
}

#' @describeIn elasticHTE Summary of analysis.
#' @param object An object of S3 class \code{elasticHTE}.
#'
#' @export
summary.elasticHTE <- function(object, ...) {

  # n.estimator x p-1 matrix
  est.mat <- object$psi[, -1L, drop = FALSE]
  ve.mat <- object$ve[, -1L, drop = FALSE]

  # n.estimator x p-1 matrix
  inf.mat <- object$CIs.inf[, -1L, drop = FALSE]
  sup.mat <- object$CIs.sup[, -1L, drop = FALSE]

  psi <- lapply(1L:nrow(est.mat),
                function(i) {
                  data.frame("est" = est.mat[1L, ],
                             "ve" = ve.mat[1L, ],
                             "CI_lower" = inf.mat[1L, ],
                             "CI_upper" = sup.mat[1L, ])
                })
  names(psi) <- rownames(est.mat)

  nuis <- list("gamma" = object$nuispar$gamma[1L],
               "c.gamma" = object$nuispar$c_gamma[1L],
               "Icomb" = object$nuispar$Icomb[1L],
               "Icomb.pval" = object$nuispar$Icomb.pval[1L],
               "eta" = object$nuispar$eta)

  list("psi" = psi,
       "nuispar" = object$nuispar,
       "Tstat" = object$Tstat)
}
