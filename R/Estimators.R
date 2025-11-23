# ----- Exposure probability -----
#' Estimate Exposure Probabilities
#'
#' Description.....
#' This function estimates exposure probabilities by fitting a logistic regression model.
#'
#' @param formula A formula for the exposure variable.
#' @param fit_dat A data used to fit the model.
#' @param pred_dat A data used to predict exposure probabilities.
#'
#' @return A vector of predicted exposure probabilities.
#'
#' @export
#' @details
#' This function uses `speedglm` for fast computation and is based on data.frame()
#' which is the default format of speedglm(). And this function does not support
#' data.table().
#'
#' @examples
#' \dontrun{
#' result <- ExpProb(
#'    formula = A ~  X1 + X2 + X3,
#'    fit_dat = data_fit,
#'    pred_dat = data_pre
#' )
#' head(result)
#' }
#'
#'
ExpProb  <- function(formula, fit_dat, pred_dat) {

  # To make sure the proper format
  fit_dat <- as.data.frame(fit_dat)
  pred_dat <- as.data.frame(pred_dat)
  fo <- as.formula(formula)
  vars <- all.vars(fo)

  fit_dat <- fit_dat[complete.cases(fit_dat[, vars]), ]

  # Using matrix helps improve speed
  y <- fit_dat[[vars[1]]]
  X_fit <- model.matrix(fo, data = fit_dat)
  X_pred <- model.matrix(fo, data = pred_dat,
                         # To ensure consistent encoding
                         contrasts.arg = attr(X_fit, "contrasts"))

  fit <- speedglm::speedglm.wfit(y = y, X = X_fit, family = binomial(link = "logit"))

  eta <- X_pred %*% coef(fit)
  prob <- 1 / (1 + exp(-eta))

  gc()
  return(as.vector(prob))
}



# ----- Principal score -----
#' Estimate Principal Score
#'
#' Description.....
#' This function estimates principal score by fitting a logistic regression model.
#'
#' @param formula A fomula for the principal score variable.
#' @param fit_dat A data used to fit the model.
#' @param pred_dat A data used to predict principal score.
#' @param trt Integer. Type of A. 1 = Treated, 0 = Not Treated.
#' @param A_col The name of the treatment status column.
#' @param ind_col The name of the indicator column.
#'
#'
#' @return A list with the following components
#' \describe{
#'   \item{e_pred}{A vector of predicted principal score.}
#'   \item{fit_ps_A}{The fitted logistic regression model object}
#' }
#' @export
#'
#' @details
#' This function uses `speedglm` for fast computation and is based on data.frame()
#' which is the default format of speedglm(). And this function does not support
#' data.table().
#'
#' @examples
#' \dontrun{
#' result <- PrinPred(
#'    formula = A ~  X1 + X2 + X3,
#'    fit_dat = data_fit,
#'    pred_dat = data_pre,
#'    trt = 1,
#'    ind_col = "ind",
#'    A_col = "A"
#' )
#' head(result)
#' summary(result$fit_ps_A)
#' }
#'
#'
PrinPred <- function(formula, fit_dat, pred_dat, trt,
                     # data
                     ind_col, A_col) {

  fit_dat <- as.data.frame(fit_dat)
  pred_dat <- as.data.frame(pred_dat)
  fo <- as.formula(formula)
  vars <- all.vars(fo)

  fit_dat <- fit_dat[complete.cases(fit_dat[, vars]), ]

  fit_sub <- fit_dat[fit_dat[[ind_col]] == 1, ]
  y_fit <- fit_sub[[vars[1]]]
  X_fit <- model.matrix(fo, data = fit_sub)
  pred_dat[[A_col]] <- trt
  X_pred <- model.matrix(fo, data = pred_dat,contrasts.arg = attr(X_fit, "contrasts") )

  fit_ps <- speedglm::speedglm.wfit(y = y_fit, X = X_fit, family = binomial(link = "logit"))
  eta <- X_pred %*% fit_ps$coefficients
  e_pred <- 1 / (1 + exp(-eta))

  gc()
  return(list(e_pred = as.vector(e_pred), fit_ps_A = fit_ps))
}

# ----- Outcome Mean -----
#' Estimate Outcome Mean
#'
#' Description.....
#' This function estimates outcome mean.
#'
#' @param formula A fomula for the outcome mean variable.
#' @param fit_dat A data used to fit the model.
#' @param pred_dat A data used to predict outcome mean.
#' @param trt Integer. Type of A. 1 = Treated, 0 = Not Treated.
#' @param Y_type Integer. Type of data. 1 = continuous data, 2 = binary data.
#' @param A_col The name of the treatment status column.
#' @param S_col The name of the survival status column.
#'
#' @return A vector of predicted outcome mean.
#' @export
#'
#' @details This function supports two types of Y and is based on data.frame()
#' which is the default format of glm().
#'
#' @examples
#' \dontrun{
#' result <- MuPred(
#'    formula = A ~  X1 + X2 + X3,
#'    fit_dat = data_fit,
#'    pred_dat = data_pre,
#'    trt = 1,
#'    Y_type = 1,
#'    S_col = "S",
#'    A_col = "A"
#' )
#' head(result)
#' }
#'
#'
MuPred  <- function(formula, fit_dat, pred_dat, trt, Y_type, A_col, S_col) {
  fit_dat <- as.data.frame(fit_dat)
  pred_dat <- as.data.frame(pred_dat)
  fo <- as.formula(formula)
  vars <- all.vars(fo)

  # na
  fit_dat <- fit_dat[!is.na(fit_dat[, vars[1]]),]
  pred_dat[[A_col]] <- trt
  pred_dat[[S_col]] <- 1

  if (Y_type == 1) {  #continuousu
    fit_mu <- lm(fo, data = fit_dat)
    mu_pred <- predict(fit_mu, newdata = pred_dat, type = "response")
    return(mu_pred)
  } else if (Y_type == 2) {  # binary
    fit_mu <- glm(fo, family = binomial(link = "logit"), data = fit_dat)
    mu_pred <- predict(fit_mu, newdata = pred_dat, type = "response")  # mu = 1/(1+exp(-Xβ))
    return(mu_pred)
  }
}

