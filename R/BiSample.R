#' BiSample: Simulated Dataset for Binary Outcomes
#'
#' This is a dataset sample generated for testing binary potential outcomes.
#'
#' @format A data frame with 2000 individuals observed over 5 time points,
#'         containing following variables:
#' \describe{
#'   \item{id}{Individual ID}
#'   \item{time}{Time point}
#'   \item{X1--X10}{continuous or binary variables}
#'   \item{A}{Treatment indicator (1 = treated, 0 = not treated)}
#'   \item{Pi}{Estimated propensity score}
#'   \item{S1}{Latent survival indicator under treatment}
#'   \item{S0}{Latent survival indicator under control}
#'   \item{logit_Y1}{Linear predictor (logit) of \eqn{Y1} given covariates}
#'   \item{logit_Y0}{Linear predictor (logit) of \eqn{Y0} given covariates}
#'   \item{prob_Y1}{Probability \eqn{P(Y1 = 1)}}
#'   \item{prob_Y0}{Probability \eqn{P(Y0 = 1)}}
#'   \item{Y1}{Potential outcome under treatment (binary)}
#'   \item{Y0}{Potential outcome under control (binary)}
#'   \item{S}{Observed survival indicator (binary)}
#'   \item{Y}{Observed outcome (binary, only defined if \code{S == 1})}
#'   \item{ind}{Lagged value of \code{S}, previous period's state within each individual}
#'   \item{U11}{Latent principal stratum indicator for unit with \code{S1 = 1, S0 = 1}}
#'   \item{S1minusS0}{Principal stratum category based on difference between \code{S1} and \code{S0}}
#' }
#'
#' @source Simulated via `data-raw/DATASET.R`
#' @usage data(BiSample)
#' @keywords datasets
#' @examples
#' data(BiSample)
#' head(BiSample)
"BiSample"
