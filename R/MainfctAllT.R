#' Title ......
#'
#' Description.....
#'
#' Computes time-varying coefficients using approach HTE,
#' handling both continuous and binary outcome types.
#'
#' @param K Integer. Number of time points.
#' @param data A `data.table`-like dataset with variables including `id`, `time`, `Y`, `S`, `A`, `U11`, etc.
#' @param exp_fo A formula object for modeling exposure probabilities, passed to `ExpProb()`.
#' @param prin_fo A formula object for modeling principal scores, passed to `PrinPred()`.
#' @param mu_fo A formula object for modeling outcome means, passed to `MuPred()`.
#' @param col_names A character vector of selected column names used as covariates.
#' @param Y_type Integer. Type of data. 1 = continuous data, 2 = binary data.
#' @param n Integer. Number of regression coefficients, typically = #selected_covariates + intercept.
#' @param S_col The name of the survival status column.
#' @param A_col The name of the treatment status column.
#' @param Y_col The name of the variable outcome column.
#' @param ind_col The name of the indicator column.
#' @param time_col The name of time point column.
#' @param id_col The name of unique individual id column.
#'
#' @return beta. A 1 by n matrix of estimated bias-corrected coefficients.
#'
#' @export
#'
#' @details
#' more details to be added.......
#' This function supports both linear and nonlinear link estimation via
#' Newton-Raphson with analytical Jacobian.
#' Besides, this function is based on the format of data.table for it help
#' save storage and improve calculation speed.
#'
#' @examples
#' \dontrun{
#' result <- MainfctAllT(
#'  i = 1, K = 4, data = BiSampleSmall,
#'  exp_fo = A ~  X1 + X2 + X3,
#'  prin_fo = S ~ (X1 + X2 + X3) * A,
#'  mu_fo = Y ~ (X1 + X2 + X3) * A + S - 1,
#'  Y_type = 1,
#'  col_names = c("X1","X2","X3"),
#'  n = 4,
#'  S_col = "S",
#'  A_col = "A",
#'  Y_col = "Y",
#'  ind_col = "ind",
#'  time_col = "time",
#'  id_col = "id"
#' )
#' result$beta
#' }
#'
MainfctAllT <- function(K, data,
                        exp_fo, prin_fo, mu_fo,
                        col_names, Y_type, n,
                        # data，
                        S_col, A_col, Y_col, ind_col, time_col, id_col
) {

  dat <- setDT(data)
  # every id have k time points
  # renumber id
  N <- nrow(dat) / K

  # ExpProb  scores: row - N_new* 1(time == 0)
  pi_pred <- ExpProb(exp_fo, dat[get(time_col) == 0], dat[get(time_col) == 0])
  pi_pred <- pmin(pmax(pi_pred, 0.01), 0.99)

  # principal scores: row - N_new* K)
  dat[, `:=`(p0 = PrinPred(prin_fo, .SD, .SD, trt = 0,ind_col = ind_col, A_col = A_col)$e_pred,
             p1 = PrinPred(prin_fo, .SD, .SD, trt = 1,ind_col = ind_col, A_col = A_col)$e_pred)]
  dat[get(time_col) == 0, `:=`(p0 = 1, p1 = 1)]
  # cumulative survive rate:row - N_new* 1(time = K-1)
  dat[, `:=`(p0_cumprod = prod(p0),p1_cumprod = prod(p1)), by = get(id_col)]

  K_p0 <- dat[get(time_col) == (K - 1), p0_cumprod]
  K_p1 <- dat[get(time_col) == (K - 1), p1_cumprod]
  # weight: psiS0 and pip1
  psiS0 <- dat[get(time_col) == (K - 1), (get(A_col) == 0) * (get(S_col) - K_p0) / (1 - pi_pred) + K_p0]
  pip1 <- pmin(pmax(K_p1 * pi_pred, 0.005), 0.995)

  # end cut(time_col = K-1)
  tmp1 <- dat[get(time_col) == (K - 1)]
  # add K_p0 and K_p1
  tmp1[, `:=`(K_p0 = K_p0, K_p1 = K_p1, psiS0 = psiS0, pip1 = pip1)]
  tmp1[, (id_col) := 1:.N]

  idx <- match(dat[[id_col]], tmp1[[id_col]])
  tmp1_all <- tmp1[idx]

  # X_base from time_col == 0, repeat for each time point
  X_base <- as.matrix(dat[get(time_col) == 0, ..col_names])
  Xtilde <- cbind(1, X_base[rep(1:N, each = K), ], dat[[time_col]])

  # pi_pred from time_col == 0, repeat for each time point
  pi_rep <- rep(pi_pred, each = K)

  last_alive_ids <- dat[get(time_col) == (K - 1) & get(S_col) == 1, get(id_col)]
  always_survivor_dat <- dat[get(id_col) %in% last_alive_ids]

  mu01 <- MuPred(mu_fo, fit_dat = always_survivor_dat, dat, trt = 0, Y_type = Y_type, A_col = A_col, S_col = S_col)
  mu11 <- MuPred(mu_fo, fit_dat = always_survivor_dat, dat, trt = 1, Y_type = Y_type, A_col = A_col, S_col = S_col)
  dat[is.na(get(Y_col)), (Y_col) := 0]

  phi1_11 <- tmp1_all$K_p0 * tmp1_all[[S_col]] * tmp1_all[[A_col]] * (dat[[Y_col]] - mu11) / tmp1_all$pip1 + mu11 * tmp1_all$psiS0
  # dat[, phi_diff := phi1_11 - phi0_11]
  phi0_11 <- (tmp1_all[[A_col]] == 0) * (dat[[Y_col]] * tmp1_all[[S_col]] - mu01 * tmp1_all$K_p0) / (1 - pi_rep) + mu01 * tmp1_all$K_p0

  dat[, phi_diff := phi1_11 - phi0_11]

  if (Y_type %in% c(2)) {

    # ---- Helper: Score_beta  ----
    Score_beta <- function(beta){
      xbeta <- as.vector(Xtilde %*% beta)
      gamma_hat <- 2 / (1 + exp(-xbeta)) - 1 # Link function
      score <- t(Xtilde) %*% (dat$phi_diff - gamma_hat * tmp1_all$psiS0)
      return(as.vector(score))
    }

    # ---- Helper: Jacobian_beta ----
    # Jacobian matrix to improve speed
    Jacobian_beta <- function(beta) {
      xbeta <- as.vector(Xtilde %*% beta)
      # first derivative
      w_vec <- 2 * exp(xbeta) / (1 + exp(xbeta))^2 * tmp1_all$psiS0
      W <- diag(w_vec)
      J <- - t(Xtilde) %*% W %*% Xtilde
      return(J)
    }

    fit_beta <- multiroot(f = Score_beta, jacfunc = Jacobian_beta,
                          start = rep(0, ncol(Xtilde)), maxiter = 100,
                          rtol = 1e-6)

    beta <- fit_beta$root
    # Check convergence
    if (fit_beta$iter == 100) warning("multiroot did not converge")

  } else{ # continuous
    XtDX <- crossprod(Xtilde, tmp1_all$psiS0 *Xtilde)
    XtY <- crossprod(Xtilde, dat$phi_diff)
    beta <- solve(XtDX, XtY)
  }

  gc()
  return(beta = beta)
}
