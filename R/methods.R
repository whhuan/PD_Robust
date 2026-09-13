#' Display and subset PDRobust objects
#'
#' Print methods display mappings, validation reports, diagnostic tables,
#' treatment-effect estimates, or sensitivity summaries. Plot methods display
#' the stored diagnostic or treatment-effect plot without refitting a model.
#'
#' @param x An object returned by a PDRobust function, of the class indicated
#'   by the method. Subsetting applies to a `pd_data` data frame returned by
#'   `DataStandard()`.
#' @param ... For subsetting, arguments passed to the next `[` method,
#'   including row and column indices and `drop`. For printing and plotting,
#'   additional arguments are accepted for generic compatibility but ignored.
#' @return Print methods return `x` invisibly. Plot methods return the stored
#'   `ggplot` object invisibly. Subsetting returns the selected data; when the
#'   result is a data frame, mapping and audit attributes and the `pd_data`
#'   class are retained.
#' @details Subsetting copies metadata without recomputing validation or audit
#'   reports. Revalidate changed data before analysis; deleting rows or columns
#'   can invalidate the required panel structure. `QR()` supplies numeric and
#'   tabular summaries and has a print method, but no package-specific plot
#'   method. Plot the returned table directly if a custom display is needed.
#' @examples
#' data("BiSample", package = "PDRobust")
#' map <- Mapping(
#'   id = "id", time = "time", treatment = "A", survival = "S", outcome = "Y",
#'   baseline_time = 0, cutoff_time = 2,
#'   covariates = c("X1", "X2", "X4"),
#'   interest_vars = c("X1", "X2"), y_type = "B"
#' )
#' print(map)
#' print(DataCheck(BiSample, map))
#' prepared <- DataStandard(BiSample, map)
#' prepared[1:3, ]
#' diagnostic <- PSDiag(prepared, A ~ X1 + X2 + X4)
#' print(diagnostic)
#' p <- plot(diagnostic)
#' @name pd_methods
#' @rdname pd_methods
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

#' @rdname pd_methods
#' @export
print.pd_hte_timevarying <- function(x, ...) {
  cat("Time-varying heterogeneous treatment effects\n")
  print(x$summary, row.names = FALSE)
  cat("Bootstrap:", x$bootstrap_info$successful, "/",
      x$bootstrap_info$requested, "successful;",
      x$bootstrap_info$attempts, "attempts\n")
  invisible(x)
}

#' @rdname pd_methods
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

#' @rdname pd_methods
#' @export
print.PSDiag <- function(x, ...) {
  cat("Exposure-model balance diagnostics\n")
  print(x$data, row.names = FALSE)
  invisible(x)
}

#' @rdname pd_methods
#' @export
print.PrinSDiag <- function(x, ...) {
  cat("Principal-score diagnostics\n")
  print(x$pripfigdat, row.names = FALSE)
  invisible(x)
}

#' @rdname pd_methods
#' @export
print.odds_ratios <- function(x, ...) {
  cat("Odds ratios and confidence intervals\n")
  print(x$forestplotdat, row.names = FALSE)
  invisible(x)
}

#' @rdname pd_methods
#' @export
print.QR <- function(x, ...) {
  cat("Principal-stratum weighted means\n")
  print(x$mean)
  cat("\nWeighted quantiles (NA for binary variables)\n")
  print(x$quantile)
  invisible(x)
}

#' @rdname pd_methods
#' @export
print.SA <- function(x, ...) {
  cat("Sensitivity analysis\n")
  print(x$beta_df_wide, row.names = FALSE)
  cat("  Scenarios:", length(unique(x$data$ratio)), "\n")
  invisible(x)
}

#' @rdname pd_methods
#' @export
plot.pd_hte_timevarying <- function(x, ...) {
  print(x$forest_plot)
  invisible(x$forest_plot)
}
#' @rdname pd_methods
#' @export
plot.pd_hte_pooled <- function(x, ...) {
  print(x$forest_plot)
  invisible(x$forest_plot)
}
#' @rdname pd_methods
#' @export
plot.PSDiag <- function(x, ...) {
  print(x$plot)
  invisible(x$plot)
}
#' @rdname pd_methods
#' @export
plot.PrinSDiag <- function(x, ...) {
  print(x$plot)
  invisible(x$plot)
}
#' @rdname pd_methods
#' @export
plot.odds_ratios <- function(x, ...) {
  print(x$plot)
  invisible(x$plot)
}
