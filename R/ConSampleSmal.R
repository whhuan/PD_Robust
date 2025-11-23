#' ConSampleSmall: Simulated Dataset for Continuous Outcomes with A Small Size
#'
#' This is a dataset sample generated for testing continuous potential outcomes.
#'
#' @format A data frame with 200 individuals observed over 3 time points,
#'         containing following variables:
#' \describe{
#'   \item{id}{Individual ID}
#'   \item{time}{Time point}
#'   \item{X1--X10}{continuous or binary variables}
#'   \item{A}{Treatment indicator (1 = treated, 0 = not treated)}
#'   \item{Pi}{Estimated propensity score}
#'   \item{S1}{Latent survival indicator under treatment}
#'   \item{S0}{Latent survival indicator under control}
#'   \item{EY1}{Linear predictor of \eqn{Y1} given covariates}
#'   \item{EY0}{Linear predictor of \eqn{Y0} given covariates}
#'   \item{Y1}{Potential outcome under treatment (continuous)}
#'   \item{Y0}{Potential outcome under control (continuous)}
#'   \item{S}{Observed survival indicator (binary)}
#'   \item{Y}{Observed outcome (continous, only defined if \code{S == 1})}
#'   \item{ind}{Lagged value of \code{S}, previous period's state within each individual}
#'   \item{U11}{Latent principal stratu1m indicator for unit with \code{S1 = 1, S0 = 1}}
#'   \item{S1minusS0}{Principal stratum category based on difference between \code{S1} and \code{S0}}
#' }
#' @source Simulated via `data-raw/DATASET.R`
#' @usage data(ConSampleSmall)
#' @keywords datasets
#' @examples
#' data(ConSampleSmall)
#' head(ConSampleSmall)
"ConSampleSmall"
