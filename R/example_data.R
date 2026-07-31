#' Binary longitudinal example data
#'
#' Simulated long-format data for illustrating binary-outcome analyses.
#'
#' @format A simulated long-format data frame and the following variables:
#' \describe{
#'   \item{id}{Subject identifier.}
#'   \item{time}{Analysis time.}
#'   \item{X1, X2, X3}{Continuous baseline covariates.}
#'   \item{X4, X5, X6}{Binary baseline covariates.}
#'   \item{A}{Binary treatment indicator.}
#'   \item{S}{Binary survival or intermediate-status indicator.}
#'   \item{Y}{Binary outcome, structurally missing after death.}
#' }
#'
#' @source Simulated for package examples.
#'
#' @examples
#' data("BiSample", package = "PDRobust")
#' head(BiSample)
"BiSample"


#' Imperfect Continuous Longitudinal Example Data
#'
#' A deliberately imperfect continuous-outcome longitudinal data set derived
#' from the analysis-ready \code{ConSample} data. The data mimic common issues
#' encountered in raw clinical data exports while remaining recoverable using
#' \code{\link{DataCheck}} and \code{\link{DataStandard}} with
#' \code{drop = TRUE}.
#'
#' The data include nonstandard subject identifiers, character-encoded visit
#' times and binary variables, unsorted records, an incomplete longitudinal
#' record, missing required covariate values, a missing outcome among survivors,
#' and a record with a missing subject identifier. Structural outcome
#' missingness for records with \code{alive_status = 0} is retained.
#'
#' @format A data frame in long format with one row per subject and visit,
#' containing the following variables:
#' \describe{
#' \item{\code{patient_id}}{Noncanonical character subject identifier.}
#' \item{\code{visit_month}}{Character-encoded visit time in months.}
#' \item{\code{treatment}}{Character-encoded binary treatment assignment.}
#' \item{\code{alive_status}}{Character-encoded binary survival or intermediate status.}
#' \item{X1, X2, X3}{Continuous baseline covariates.}
#' \item{X4, X5, X6}{Binary baseline covariates.}
#' \item{\code{clinical_outcome}}{Continuous longitudinal clinical outcome.}
#' }
#'
#' @source Simulated for package examples.
#' @examples
#' data("ImperfectConSample", package = "PDRobust")
#' head(ImperfectConSample)
"ImperfectConSample"
