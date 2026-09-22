#' Print, plot, and subset PDRobust results
#'
#' Provides standard ways to print analysis summaries, draw stored plots, and
#' select rows or columns from data prepared by `DataStandard()`. Plotting a
#' result does not refit its model.
#'
#' @param x An object returned by a PDRobust function. For `[`, this must be a
#'   data frame returned by `DataStandard()`.
#' @param ... For subsetting, arguments passed to the next `[` method,
#'   including row and column indices and `drop`. For printing and plotting,
#'   additional arguments are accepted for generic compatibility but ignored.
#' @return Print methods show the main result and invisibly return `x`. When a
#'   result contains a plot, printing also draws it; `SA` draws all stored
#'   sensitivity plots. Plot methods invisibly return the stored `ggplot`
#'   object. Subsetting returns the selected data and preserves its PDRobust
#'   mapping and preparation information when the result remains a data frame.
#' @details Subsetting preserves the stored information but does not check the
#'   data again. Before analyzing subsetted or edited data, validate them because
#'   removing rows or columns can break the required longitudinal structure.
#'   `QR()` has a print method but no package-specific plot method.
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
  cat("Time-varying heterogeneous treatment-effect estimates.\n")
  print(x$summary, row.names = FALSE)
  cat("Bootstrap:", x$bootstrap_info$successful, "/",
      x$bootstrap_info$requested, "successful;",
      x$bootstrap_info$attempts, "attempts\n")
  print(x$forest_plot)
  invisible(x)
}

#' @rdname pd_methods
#' @export
print.pd_hte_pooled <- function(x, ...) {
  cat("Pooled heterogeneous treatment-effect estimates.\n")
  print(x$summary, row.names = FALSE)
  cat("Bootstrap:", x$bootstrap_info$successful, "/",
      x$bootstrap_info$requested, "successful;",
      x$bootstrap_info$attempts, "attempts\n")
  if (!is.null(x$note)) cat("Note:", x$note, "\n")
  print(x$forest_plot)
  invisible(x)
}

#' @rdname pd_methods
#' @export
print.PSDiag <- function(x, ...) {
  cat("Exposure-model balance diagnostics before and after weighting.\n")
  print(x$data, row.names = FALSE)
  print(x$plot)
  invisible(x)
}

#' @rdname pd_methods
#' @export
print.PrinSDiag <- function(x, ...) {
  cat("Principal-score standardized diagnostic statistics.\n")
  print(x$pripfigdat, row.names = FALSE)
  print(x$plot)
  invisible(x)
}

#' @rdname pd_methods
#' @export
print.odds_ratios <- function(x, ...) {
  cat("Treatment-group-specific survival odds ratios and confidence intervals.\n")
  print(x$forestplotdat, row.names = FALSE)
  print(x$plot)
  invisible(x)
}

#' @rdname pd_methods
#' @export
print.QR <- function(x, ...) {
  cat("Principal-stratum weighted means and quantiles.\n")
  cat("Weighted means:\n")
  print(x$mean)
  cat("\nWeighted quantiles (NA for binary variables):\n")
  print(x$quantile)
  invisible(x)
}

#' @rdname pd_methods
#' @export
print.SA <- function(x, ...) {
  cat("Outcome-noise sensitivity estimates across variance-ratio scenarios.\n")
  print(x$beta_df_wide, row.names = FALSE)
  cat("  Scenarios:", length(unique(x$data$ratio)), "\n")
  for (plot in x$plot) print(plot)
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
