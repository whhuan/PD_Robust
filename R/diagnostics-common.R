# Internal diagnostic helpers ------------------------------------------------

#' @noRd
.pd_smd <- function(X, A, weights = NULL) {
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  A <- as.numeric(A)
  if (nrow(X) != length(A)) {
    .pd_stop("`X` and `A` must contain the same number of observations.")
  }
  if (!all(A %in% c(0, 1))) {
    .pd_stop("`A` must contain only 0 and 1.")
  }
  if (anyNA(X) || any(!is.finite(X))) {
    .pd_stop("SMD covariates must be complete and finite.")
  }

  if (is.null(weights)) {
    X1 <- X[A == 1, , drop = FALSE]
    X0 <- X[A == 0, , drop = FALSE]
    n1 <- nrow(X1) - 1
    n0 <- nrow(X0) - 1
    if (n1 <= 0 || n0 <= 0) {
      .pd_stop("At least two observations are required in each treatment group.")
    }
    mean1 <- colMeans(X1)
    mean0 <- colMeans(X0)
    var1 <- colSums((X1 - rep(mean1, each = nrow(X1)))^2) / n1
    var0 <- colSums((X0 - rep(mean0, each = nrow(X0)))^2) / n0
    denominator <- sqrt((n1 * var1 + n0 * var0) / (n1 + n0))
    out <- (mean1 - mean0) / denominator
  } else {
    weights <- as.numeric(weights)
    if (length(weights) != nrow(X) || any(!is.finite(weights)) ||
        any(weights < 0)) {
      .pd_stop("`weights` must be finite, nonnegative, and aligned with `X`.")
    }
    wt1 <- weights * (A == 1)
    wt0 <- weights * (A == 0)
    if (sum(wt1) <= 0 || sum(wt0) <= 0) {
      .pd_stop("Both treatment groups must have positive total weight.")
    }
    mean1 <- colSums(X * wt1) / sum(wt1)
    mean0 <- colSums(X * wt0) / sum(wt0)
    ssq1 <- colSums(wt1 * sweep(X, 2L, mean1, FUN = "-")^2)
    ssq0 <- colSums(wt0 * sweep(X, 2L, mean0, FUN = "-")^2)
    ESS1 <- (sum(wt1)^2 - sum(wt1^2)) / sum(wt1)
    ESS0 <- (sum(wt0)^2 - sum(wt0^2)) / sum(wt0)
    denominator <- sqrt((ssq1 + ssq0) / (ESS1 + ESS0))
    out <- (mean1 - mean0) / denominator
  }
  out[!is.finite(out)] <- NA_real_
  names(out) <- colnames(X)
  out
}
