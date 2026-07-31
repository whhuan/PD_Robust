# Preserve workflow metadata for ordinary row/column subsets used internally.
#' @noRd
#' @export
`[.pd_data` <- function(x, ...) {
  mapping <- attr(x, "pd_mapping", exact = TRUE)
  original_mapping <- attr(x, "pd_original_mapping", exact = TRUE)
  check <- attr(x, "pd_check", exact = TRUE)
  standardization <- attr(x, "pd_standardization", exact = TRUE)
  out <- NextMethod("[")
  if (is.data.frame(out)) {
    attr(out, "pd_mapping") <- mapping
    attr(out, "pd_original_mapping") <- original_mapping
    attr(out, "pd_check") <- check
    attr(out, "pd_standardization") <- standardization
    class(out) <- unique(c("pd_data", class(out)))
  }
  out
}

#' @noRd
#' @export
print.pd_hte_timevarying <- function(x, ...) {
  cat("Time-varying heterogeneous treatment effects\n")
  print(x$summary, row.names = FALSE)
  cat("Bootstrap:", x$bootstrap_info$successful, "/",
      x$bootstrap_info$requested, "successful;",
      x$bootstrap_info$attempts, "attempts\n")
  invisible(x)
}

#' @noRd
#' @export
print.pd_hte_pooled <- function(x, ...) {
  cat("Pooled heterogeneous treatment effects\n")
  print(x$summary, row.names = FALSE)
  cat("Bootstrap:", x$bootstrap_info$successful, "/",
      x$bootstrap_info$requested, "successful;",
      x$bootstrap_info$attempts, "attempts\n")
  if (!is.null(x$note)) cat("Note:", x$note, "\n")
  invisible(x)
}

#' @noRd
#' @export
print.PSDiag <- function(x, ...) {
  cat("Exposure-model balance diagnostics\n")
  print(x$data, row.names = FALSE)
  invisible(x)
}

#' @noRd
#' @export
print.PrinSDiag <- function(x, ...) {
  cat("Principal-score diagnostics\n")
  print(x$pripfigdat, row.names = FALSE)
  invisible(x)
}

#' @noRd
#' @export
print.odds_ratios <- function(x, ...) {
  cat("Odds ratios and confidence intervals\n")
  print(x$forestplotdat, row.names = FALSE)
  invisible(x)
}

#' @noRd
#' @export
print.QR <- function(x, ...) {
  cat("Principal-stratum weighted means\n")
  print(x$mean)
  cat("\nWeighted quantiles (NA for binary variables)\n")
  print(x$quantile)
  invisible(x)
}

#' @noRd
#' @export
print.SA <- function(x, ...) {
  cat("Sensitivity analysis\n")
  print(utils::head(x$beta_df_wide), row.names = FALSE)
  cat("  Scenarios:", length(unique(x$data$ratio)), "\n")
  invisible(x)
}

#' @noRd
#' @export
plot.pd_hte_timevarying <- function(x, ...) {
  print(x$forest_plot)
  invisible(x$forest_plot)
}
#' @noRd
#' @export
plot.pd_hte_pooled <- function(x, ...) {
  print(x$forest_plot)
  invisible(x$forest_plot)
}
#' @noRd
#' @export
plot.PSDiag <- function(x, ...) {
  print(x$plot)
  invisible(x$plot)
}
#' @noRd
#' @export
plot.PrinSDiag <- function(x, ...) {
  print(x$plot)
  invisible(x$plot)
}
#' @noRd
#' @export
plot.odds_ratios <- function(x, ...) {
  print(x$plot)
  invisible(x$plot)
}
