
#' Standardize longitudinal principal-stratification data
#'
#' Safely converts explicit binary encodings, maps IDs to consecutive integers,
#' maps the raw analysis time grid to `0, 1, ..., n`, sorts the panel, and
#' attaches the standardized mapping and audit reports.
#'
#' @param data A long-format data frame.
#' @param mapping A `pd_mapping` object returned by `Mapping()`.
#' @param drop If `TRUE`, remove unidentifiable rows and entire subjects with
#'   incomplete baseline-to-cutoff visits or required analysis values. Attrition is
#'   reported explicitly. If `FALSE`, such problems stop standardization.
#'
#' @return A `pd_data` frame. Attributes include the standardized mapping,
#'   original mapping, final readiness check, time/ID audit maps, and attrition.
#'   Analysis columns and computational mappings retain full precision; only
#'   returned display diagnostics and attrition percentages are rounded.
#' @examples
#' data("BiSample", package = "PDRobust")
#' map <- Mapping(
#'   id = "id", time = "time", treatment = "A",
#'   survival = "S", outcome = "Y",
#'   baseline_time = 0, cutoff_time = 2,
#'   covariates = c("X1", "X2", "X4"),
#'   interest_vars = c("X1", "X2"), y_type = "B"
#' )
#' pd_dat <- DataStandard(BiSample, map)
#' attr(pd_dat, "pd_mapping")
#' @export
DataStandard <- function(data, mapping, drop = FALSE) {
  out <- .pd_standardize_data_impl(
    data = data, mapping = mapping, drop = drop
  )
  check <- attr(out, "pd_check", exact = TRUE)
  if (inherits(check, "pd_data_check")) {
    attr(out, "pd_check") <- .pd_round_data_check(check)
  }
  standardization <- attr(out, "pd_standardization", exact = TRUE)
  if (is.list(standardization)) {
    if (is.list(standardization$attrition)) {
      standardization$attrition$retained_percent <- .pd_round_output(
        standardization$attrition$retained_percent
      )
    }
    if (inherits(standardization$initial_check, "pd_data_check")) {
      standardization$initial_check <- .pd_round_data_check(
        standardization$initial_check
      )
    }
    attr(out, "pd_standardization") <- standardization
  }
  out
}
