# =============================================================================
# 01_reference_beta_N1M.R
#
# Purpose:
#   Generate large-sample reference estimates for continuous and binary
#   outcomes using N = 1,000,000.
#
# Workflow for each outcome:
#   generate_data_example()
#   -> Mapping()
#   -> DataStandard()
#   -> saveRDS(pd_data)
#   -> HTEAllT(B = 0)
#   -> save result
#   -> rm() + gc()
#   -> readRDS(the same pd_data)
#   -> HTESepT(B = 0)
#   -> save result
#   -> rm() + gc()
# =============================================================================

library(data.table)
library(MASS)
library(bindata)
library(PDRobust)

# =============================================================================
# Data-generating parameters
# =============================================================================

cov_mtx <- matrix(
  c(
    1.00, 0.04, 0.27,
    0.04, 1.00, 0.09,
    0.27, 0.09, 1.00
  ),
  nrow = 3,
  byrow = TRUE
)

rho_mtx <- matrix(
  c(
    1.00, 0.07, 0.02,
    0.07, 1.00, 0.17,
    0.02, 0.17, 1.00
  ),
  nrow = 3,
  byrow = TRUE
)

a_coef <- c(0.8, -0.4, 0.6, -0.5, 0.7, 0.1)
S_con_coef <- c(0.6, 0.5, 0.1, 0.4, 0.5, -0.4)
Y_coef <- c(0.2, 0.3, -0.1, 0.7, 0.2, -0.4)

# =============================================================================
# Data generator
# =============================================================================

generate_data_example <- function(
    N,
    K,
    cov_mtx,
    rho_mtx,
    a_coef,
    S_con_coef,
    Y_coef,
    Prob_A = 0.85,
    Prob_S0 = 0.85,
    Prob_S1 = 0.90,
    S_sigma = 0.10,
    Y_type = 1L,
    Y_sigma = 0.10,
    beta_S = 2,
    gamma_Y1 = 0.10,
    gamma_Y0 = 0.20,
    Prob_Y0 = 0.02,
    Prob_Y1 = 0.02,
    seed = NULL
) {
  if (!is.null(seed)) set.seed(seed)

  n_var_cont <- nrow(cov_mtx)
  n_var_bin <- nrow(rho_mtx)
  n_covariates <- n_var_cont + n_var_bin

  dat <- data.table::data.table(
    id = rep(seq_len(N), each = K),
    time = rep(0:(K - 1L), times = N)
  )

  X_cont <- MASS::mvrnorm(
    n = N,
    mu = rep(0, n_var_cont),
    Sigma = cov_mtx
  )

  X_bin <- bindata::rmvbin(
    n = N,
    margprob = rep(0.5, n_var_bin),
    bincorr = rho_mtx
  )

  X <- cbind(X_cont, X_bin)
  colnames(X) <- paste0("X", seq_len(n_covariates))

  X_rep <- X[rep(seq_len(N), each = K), , drop = FALSE]
  dat <- cbind(dat, X_rep)

  logit <- function(p) stats::qlogis(p)

  linear_A <- logit(Prob_A) + as.numeric(X %*% a_coef)
  Pi <- stats::plogis(linear_A)
  A <- stats::rbinom(n = N, size = 1L, prob = Pi)

  dat[, `:=`(
    A = rep(A, each = K),
    Pi = rep(Pi, each = K)
  )]

  linear_S0 <- logit(Prob_S0) + as.numeric(X %*% S_con_coef)
  linear_S1 <- logit(Prob_S1) + as.numeric(X %*% (S_con_coef + S_sigma))

  prob_S0 <- stats::plogis(linear_S0)
  prob_S1 <- stats::plogis(linear_S1)
  prob_S1 <- pmax(prob_S1, prob_S0)

  S0 <- matrix(1L, nrow = N, ncol = K)
  S1 <- matrix(1L, nrow = N, ncol = K)

  if (K > 1L) {
    for (k in 2:K) {
      survival_u <- stats::runif(N)

      S0[, k] <- S0[, k - 1L] *
        as.integer(survival_u < prob_S0)

      S1[, k] <- S1[, k - 1L] *
        as.integer(survival_u < prob_S1)
    }
  }

  dat[, `:=`(
    S0 = as.vector(t(S0)),
    S1 = as.vector(t(S1))
  )]

  if (Y_type == 1L) {
    dat[, EY1 :=
          as.numeric(X_rep %*% (Y_coef + Y_sigma)) +
          5 + beta_S * S1 + 0.5 + gamma_Y1 * time]

    dat[, EY0 :=
          as.numeric(X_rep %*% Y_coef) +
          5 + beta_S * S0 + gamma_Y0 * time]

    dat[, Y1 := stats::rnorm(.N, mean = EY1, sd = 3)]
    dat[, Y0 := stats::rnorm(.N, mean = EY0, sd = 3)]
  } else {
    dat[, logit_Y1 :=
          logit(Prob_Y1) +
          as.numeric(X_rep %*% (Y_coef + Y_sigma)) +
          beta_S * S1 + gamma_Y1 * time]

    dat[, logit_Y0 :=
          logit(Prob_Y0) +
          as.numeric(X_rep %*% Y_coef) +
          beta_S * S0 + gamma_Y0 * time]

    dat[, prob_Y1 := stats::plogis(logit_Y1)]
    dat[, prob_Y0 := stats::plogis(logit_Y0)]

    dat[, Y1 := stats::rbinom(.N, size = 1L, prob = prob_Y1)]
    dat[, Y0 := stats::rbinom(.N, size = 1L, prob = prob_Y0)]
  }

  dat[, S := A * S1 + (1L - A) * S0]
  dat[, Y := A * Y1 + (1L - A) * Y0]

  dat[S == 0L, Y := NA_real_]

  data.table::setorder(dat, id, time)

  output_dat <- dat[
    ,
    c(
      "id", "time", "Pi",
      "S1", "S0", "S",
      "A", "Y1", "Y0", "Y",
      paste0("X", seq_len(n_covariates))
    ),
    with = FALSE
  ]

  numeric_columns <- c(
    "Y", "Pi",
    paste0("X", seq_len(n_covariates))
  )

  output_dat[
    ,
    (numeric_columns) := lapply(
      .SD,
      function(value) {
        if (is.double(value)) round(value, digits = 3) else value
      }
    ),
    .SDcols = numeric_columns
  ]

  list(
    dat = output_dat,
    discarded_subjects = integer(0),
    retained_subjects = data.table::uniqueN(output_dat$id)
  )
}

# =============================================================================
# Common formulas
# =============================================================================

ps_fo <- A ~ X1 + X2 + X3 + X4 + X5 + X6
prin_fo <- S ~ (X1 + X2 + X3 + X4 + X5 + X6) * A
out_fo <- Y ~ (X1 + X2 + X3 + X4 + X5 + X6) * A + S

# =============================================================================
# 1. Continuous outcome, N = 1,000,000
# =============================================================================

ConSample <- generate_data_example(
  N = 1000000,
  K = 3,
  cov_mtx = cov_mtx,
  rho_mtx = rho_mtx,
  a_coef = a_coef,
  S_con_coef = S_con_coef,
  Y_coef = Y_coef,
  Y_type = 1L,
  seed = 123
)$dat

mapping_con <- Mapping(
  id = "id",
  time = "time",
  treatment = "A",
  survival = "S",
  outcome = "Y",
  baseline_time = 0,
  cutoff_time = 2,
  covariates = c("X1", "X2", "X3", "X4", "X5", "X6"),
  interest_vars = c("X1", "X2", "X3", "X4", "X5", "X6"),
  y_type = "C"
)

pd_data_con <- DataStandard(
  ConSample,
  mapping_con,
  drop = TRUE
)

saveRDS(
  pd_data_con,
  "pd_data_continuous_N1M.rds",
  compress = FALSE
)

rm(ConSample)
gc()

pooled_con <- HTEAllT(
  data = pd_data_con,
  ps_fo = ps_fo,
  prin_fo = prin_fo,
  out_fo = out_fo,
  B = 0,
  verbose = FALSE
)

beta_t0_continuous <- pooled_con$summary

write.csv(
  beta_t0_continuous,
  "beta_t0_HTEAllT_continuous_N1M.csv",
  row.names = FALSE
)

rm(
  pooled_con,
  beta_t0_continuous,
  pd_data_con
)
gc()

pd_data_con <- readRDS(
  "pd_data_continuous_N1M.rds"
)

separate_con <- HTESepT(
  data = pd_data_con,
  ps_fo = ps_fo,
  prin_fo = prin_fo,
  out_fo = out_fo,
  target_time = c(0, 1, 2),
  B = 0,
  conf_level = 0.95,
  max_attempts = NULL,
  verbose = FALSE
)

beta_0_continuous <- separate_con$summary

write.csv(
  beta_0_continuous,
  "beta_0_HTESepT_continuous_N1M.csv",
  row.names = FALSE
)

rm(
  separate_con,
  beta_0_continuous,
  pd_data_con,
  mapping_con
)
gc()

# =============================================================================
# 2. Binary outcome, N = 1,000,000
# =============================================================================

BiSample <- generate_data_example(
  N = 1000000,
  K = 3,
  cov_mtx = cov_mtx,
  rho_mtx = rho_mtx,
  a_coef = a_coef,
  S_con_coef = S_con_coef,
  Y_coef = Y_coef,
  Y_type = 2L,
  seed = 456
)$dat

mapping_bin <- Mapping(
  id = "id",
  time = "time",
  treatment = "A",
  survival = "S",
  outcome = "Y",
  baseline_time = 0,
  cutoff_time = 2,
  covariates = c("X1", "X2", "X3", "X4", "X5", "X6"),
  interest_vars = c("X1", "X2", "X3", "X4", "X5", "X6"),
  y_type = "B"
)

pd_data_bin <- DataStandard(
  BiSample,
  mapping_bin,
  drop = TRUE
)

saveRDS(
  pd_data_bin,
  "pd_data_binary_N1M.rds",
  compress = FALSE
)

rm(BiSample)
gc()

pooled_bin <- HTEAllT(
  data = pd_data_bin,
  ps_fo = ps_fo,
  prin_fo = prin_fo,
  out_fo = out_fo,
  B = 0,
  verbose = FALSE
)

beta_t0_binary <- pooled_bin$summary

write.csv(
  beta_t0_binary,
  "beta_t0_HTEAllT_binary_N1M.csv",
  row.names = FALSE
)

rm(
  pooled_bin,
  beta_t0_binary,
  pd_data_bin
)
gc()

pd_data_bin <- readRDS(
  "pd_data_binary_N1M.rds"
)

separate_bin <- HTESepT(
  data = pd_data_bin,
  ps_fo = ps_fo,
  prin_fo = prin_fo,
  out_fo = out_fo,
  target_time = c(0, 1, 2),
  B = 0,
  conf_level = 0.95,
  max_attempts = NULL,
  verbose = FALSE
)

beta_0_binary <- separate_bin$summary

write.csv(
  beta_0_binary,
  "beta_0_HTESepT_binary_N1M.csv",
  row.names = FALSE
)

rm(
  separate_bin,
  beta_0_binary,
  pd_data_bin,
  mapping_bin
)
gc()
