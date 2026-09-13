
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
#' @return A data frame inheriting from `pd_data`, retaining the input column
#'   names and additional unmapped columns. Rows outside the mapped time window
#'   are removed; retained rows are sorted by recoded ID and time. Attributes are:
#' \describe{
#'   \item{pd_mapping}{The mapping with standardized baseline and cutoff times;
#'     column roles retain their input names.}
#'   \item{pd_original_mapping}{The mapping supplied for the raw data.}
#'   \item{pd_check}{The final `pd_data_check` report, including attrition.
#'     A warning is issued if the returned data are not ready for analysis,
#'     for example if deletion removes one treatment group.}
#'   \item{pd_standardization}{A list containing `time_map`, `id_map`,
#'     `attrition`, and `initial_check`. The maps link raw values to their
#'     standardized values; the initial check describes the input data.}
#' }
#'   Both the initial and final validation are performed. Audit attributes
#'   describe this standardization call and are not recomputed when the data
#'   are subsequently edited or subsetted; see [pd_methods]. The ID map has
#'   one row per retained subject, so audit storage grows with sample size.
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
