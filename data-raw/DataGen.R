library(data.table)
library(MASS)
library(bindata)
library(tidyverse)
library(dplyr)
library(usethis)

cov_mtx <- matrix(c(1.00, 0.04, 0.27, 0.29, 0.25,
                    0.04, 1.00, 0.09, 0.00, 0.05,
                    0.27, 0.09, 1.00, 0.13, 0.24,
                    0.29, 0.00, 0.13, 1.00, 0.10,
                    0.25, 0.05, 0.24, 0.10, 1.00),nrow=5)

# binary covariates
rho_mtx <- matrix(c(1.00, 0.07, 0.02, 0.23, 0.11,
                    0.07, 1.00, 0.17, 0.09, 0.23,
                    0.02, 0.17, 1.00, 0.29, 0.03,
                    0.23, 0.09, 0.29, 1.00, 0.01,
                    0.11, 0.23, 0.03, 0.01, 1.00),nrow=5)

a_coef <- c(0.8,-0.4, 0.6, -0.5, 0.7, 0.1,-0.2, 0.3, -0.4, 0.5)

# intermediate varibles and its logit beta
S_con_coef <- c(0.6, 0.5, 0.1, 0.4, 0.5, -0.4, 0.6, 0.7, -0.5, 0.6)
Y_coef <- c(0.2, 0.3, -0.1, 0.7, 0.2, -0.4, 0.6, 0.5, -0.4, 0.3)

A_X <- c(0.2, 0.1, 0.15, 0.2, 0.1, 0.1, 0.2, 0.20, 0.1, 0.1)
#
# cov_mtx <- matrix(c(1.00, 0.04, 0.27,
#                     0.04, 1.00, 0.09,
#                     0.27, 0.09, 1.00),nrow=3)
#
#
# rho_mtx <- matrix(c(1.00, 0.07, 0.02,
#                     0.07, 1.00, 0.17,
#                     0.02, 0.17, 1.00),nrow=3)
#
# # treatment varibles and its logit beta
# a_coef <- c(0.8,-0.4, 0.6, -0.5, 0.7, 0.1)
#
#
# S_con_coef <- c(0.6, 0.5, 0.1, 0.4, 0.5, -0.4)
#
# Y_coef <- c(0.2, 0.3, -0.1, 0.7, 0.2, -0.4)
#
# A_X <- c(0.2, 0.1, 0.15, 0.2, 0.1, 0.1)

datt_gen <- function(N,K,
                     # cov_mtx, rho_mtx, a_coef, S_con_coef,Y_coef, A_X,
                     Prob_A = 0.85, # The proportion of A =1
                     Prob_S0 = 0.85,  # The proportion of S0 = 1
                     Prob_S1 = 0.9, # The proportion of S1 = 1
                     S_sigma = 0.1, # Noise vector
                     Y_type = 1, # Type of Y
                     Y_sigma = 0.1, # Noise vector
                     # For EY0/EY1
                     beta_S = 2, # Coefficients of S
                     gamma_Y1 = 0.1, gamma_Y0 = 0.2,  # Coefficients of time
                     # if Y_type == 2
                     Prob_Y0 = 0.02, Prob_Y1 = 0.02, # The proportion of Y0/Y1 =1
                     n_var_cont = 5, n_var_bin = 5 # Nums of covariates

) {
  # ---- X and time ----
  dat <- data.table(id = rep(1:N, each = K),
                    time = rep(0:(K - 1), N))
  X_cont <- mvrnorm(N, rep(0, n_var_cont), cov_mtx)
  X_bin <- bindata::rmvbin(N, margprob = rep(0.5, n_var_bin), bincorr = rho_mtx)
  X <- cbind(X_cont, X_bin)
  colnames(X) <- paste0("X", 1: as.numeric(n_var_cont + n_var_bin))
  X_rep <- X[rep(1:N, each = K), ]
  dat <- cbind(dat, X_rep)

  # ---- A and Pi ----
  # Using given Prob_A to calculate intercept
  ProbBeta <- function(x){log(x/ (1-x))}
  a_beta0 <- ProbBeta(Prob_A)
  exp_a <- exp(a_beta0 + X %*% a_coef)
  Pi <- as.numeric(exp_a/(1+exp_a))
  A <- rbinom(N, 1, Pi)
  dat[, `:=`(A = rep(A, each = K),
             Pi= rep(Pi, each = K))]

  # ---- S1 and S0 ----
  S1_beta0 <- ProbBeta(Prob_S1)
  S0_beta0 <- ProbBeta(Prob_S0)
  exp_S1 <- exp(S1_beta0 + X %*% (S_con_coef + S_sigma ) + 1.6)
  exp_S0 <- exp(S0_beta0 + X %*% S_con_coef)
  # Every sampling repeat k times and sample N times
  dat[, S1 := rbinom(.N, 1, rep(exp_S1 / (1 + exp_S1), each = K))]
  dat[, S0 := rbinom(.N, 1, rep(exp_S0 / (1 + exp_S0), each = K))]

  # Alive when starting, using time to locate instead of index
  dat[time == 0, `:=`(S1 = 1, S0 = 1)]
  # Null when dead
  dat[, S1 := cumprod(S1), by = id]
  dat[, S0 := cumprod(S0), by = id]

  # ---- Y1, Y0 and EY1, EY0 ----
  # Y is linear
  if (Y_type == 1){
    dat[, EY1 := as.numeric(X_rep  %*% (Y_coef + Y_sigma)) + 5 + beta_S * S1 + 0.5 + gamma_Y1 * time]
    dat[, EY0 := as.numeric(X_rep  %*% Y_coef) + 5 + beta_S * S0 + gamma_Y0 * time]
    dat[, Y1 := rnorm(.N, EY1, 3)]
    dat[, Y0 := rnorm(.N, EY0, 3)]
    # Y is binary
  } else if (Y_type == 2) {
    # To control the probability of Y == 1
    beta0_Y1 <- ProbBeta(Prob_Y1)
    beta0_Y0 <- ProbBeta(Prob_Y0)
    # Using logit regression
    dat[, logit_Y1 := beta0_Y1 + X_rep  %*% (Y_coef + Y_sigma) + beta_S * S1 + gamma_Y1 * time]
    dat[, logit_Y0 := beta0_Y0 + X_rep  %*% Y_coef + beta_S * S0 + gamma_Y0 * time]
    dat[, prob_Y1 := 1 / (1 + exp(-logit_Y1))]
    dat[, prob_Y0 := 1 / (1 + exp(-logit_Y0))]
    dat[, Y1 := rbinom(.N, 1, prob_Y1)]
    dat[, Y0 := rbinom(.N, 1, prob_Y0)]
  }

  # ------- Y and S -------
  # Principal effect
  dat[, S := A * S1 + (1 - A) * S0]
  dat[, Y := A * Y1 + (1 - A) * Y0]
  dat[S == 0, Y := NA]

  # ------- ind, U11 and S1minusS0 -------
  # ind: if S == 1 when t-1, ind == 1 when t;
  #      if S == 0 when t-1, ind == 0 when t just same as value of S last period
  dat[, ind := shift(S, type = "lag", fill = 0), by = id]

  dat[, `:=`(U11 = S1 * S0, S1minusS0 = S1 - S0)]

  discard_id <- unique(dat$id[dat$S1 < dat$S0])
  dat <- dat[!dat$id %in% discard_id, ]

  # renumber id
  N <- nrow(dat) / K
  # new id
  dat[, id := rep(1:N, each = K)]

  # Release memory
  gc()

  return(list(dat = dat, exp_S1 = exp_S1, exp_S0 = exp_S0))
}


ConSampleSmall <- datt_gen(N = 100, K = 3, Y_type = 1,n_var_cont = 3, n_var_bin = 3)$dat
BiSample <- datt_gen(N = 2000, K = 5, Y_type = 2 )$dat

usethis::use_data(ConSampleSmall, overwrite = TRUE)
usethis::use_data(BiSample, overwrite = TRUE)
