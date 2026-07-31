
#' Validate longitudinal principal-stratification data
#'
#' Uses the column roles, baseline and cutoff endpoints, mapped covariates,
#' effect modifiers, and outcome type stored in `mapping`. Every actual observed
#' time within the mapped window belongs to the analysis grid. Input data are never
#' modified.
#'
#' @param data A long-format data frame.
#' @param mapping A `pd_mapping` object returned by `Mapping()`.
#' @param strict Stop when any analysis-blocking check fails.
#'
#' @return A `pd_data_check` object containing `ready_for_analysis`,
#'   `manual_resolution_required`, row-per-check results, settings, and detailed
#'   diagnostics. Calculated display diagnostics are rounded to three decimals;
#'   counts, row indices, identifiers, and logical flags retain their types.
#' @examples
#' data("BiSample", package = "PDRobust")
#' map <- Mapping(
#'   id = "id", time = "time", treatment = "A",
#'   survival = "S", outcome = "Y",
#'   baseline_time = 0, cutoff_time = 2,
#'   covariates = c("X1", "X2", "X4"),
#'   interest_vars = c("X1", "X2"), y_type = "B"
#' )
#' check <- DataCheck(BiSample, map)
#' check$ready_for_analysis
#' @export
DataCheck <- function(data, mapping, strict = FALSE) {
  .pd_round_data_check(
    .pd_check_data_impl(data = data, mapping = mapping, strict = strict)
  )
}
