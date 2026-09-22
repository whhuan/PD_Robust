
#' Prepare longitudinal data for PDRobust analyses
#'
#' Converts supported binary values to `0` and `1`, replaces subject IDs and
#' analysis times with consecutive integers, sorts the data by subject and time,
#' and stores the information needed by other PDRobust functions.
#'
#' @param data A long-format data frame.
#' @param mapping A `pd_mapping` object returned by `Mapping()`.
#' @param drop If `TRUE`, remove rows that cannot be assigned to a subject and
#'   remove subjects with missing visits or required values between baseline and
#'   cutoff. The returned report records what was removed. If `FALSE`, these
#'   problems stop standardization.
#'
#' @return A data frame inheriting from `pd_data`, retaining the input column
#'   names and additional unmapped columns. Rows outside the mapped time window
#'   are removed; retained rows are sorted by recoded ID and time. Attributes are:
#' \describe{
#'   \item{pd_mapping}{The mapping updated to use the standardized baseline and
#'     cutoff times. Column names remain unchanged.}
#'   \item{pd_original_mapping}{The mapping supplied for the raw data.}
#'   \item{pd_check}{The final `pd_data_check` report, including attrition.
#'     A warning is issued if the returned data are not ready for analysis,
#'     for example if deletion removes one treatment group.}
#'   \item{pd_standardization}{A list containing `time_map`, `id_map`,
#'     `attrition`, and `initial_check`. The maps link raw values to their
#'     standardized values; the initial check describes the input data.}
#' }
#'   The function checks the data both before and after preparation. The stored
#'   reports describe this call and are not recalculated if the returned data
#'   are later edited or subsetted; see [pd_methods]. The ID map has one row per
#'   retained subject, so its size increases with the number of subjects.
#'   Analysis values retain full precision; only summaries shown to users and
#'   percentages describing removed data are rounded.
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
