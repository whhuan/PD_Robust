# Rebuild the package data files used by PDRobust.
# This development script is not included in the installed package.

library(data.table)
library(MASS)
library(bindata)
library(usethis)

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

# Coefficients correspond to X1-X6.
a_coef <- c(0.8, -0.4, 0.6, -0.5, 0.7, 0.1)

S_con_coef <- c(0.6, 0.5, 0.1, 0.4, 0.5, -0.4)

Y_coef <- c(0.2, 0.3, -0.1, 0.7, 0.2, -0.4)


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
  if (!is.null(seed)) {
    set.seed(seed)
  }

  n_var_cont <- nrow(cov_mtx)
  n_var_bin <- nrow(rho_mtx)
  n_covariates <- n_var_cont + n_var_bin

  # ---- X and time ------------------------------------------------------------
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

  logit <- function(p) {
    stats::qlogis(p)
  }

  # ---- Treatment A and propensity score Pi ----------------------------------
  linear_A <- logit(Prob_A) +
    as.numeric(X %*% a_coef)

  Pi <- stats::plogis(linear_A)
  A <- stats::rbinom(
    n = N,
    size = 1L,
    prob = Pi
  )

  dat[, `:=`(
    A = rep(A, each = K),
    Pi = rep(Pi, each = K)
  )]

  # ---- Potential survival S1 and S0 -----------------------------------------
  #
  # Prob_S0 and Prob_S1 are interval-specific survival probabilities.
  # The treated survival model retains a benefit, but the previous strong
  # fixed shift of +1.6 is removed.
  linear_S0 <- logit(Prob_S0) +
    as.numeric(X %*% S_con_coef)

  linear_S1 <- logit(Prob_S1) +
    as.numeric(X %*% (S_con_coef + S_sigma))

  prob_S0 <- stats::plogis(linear_S0)
  prob_S1 <- stats::plogis(linear_S1)

  # Ensure the treated potential survival probability is never below
  # the control potential survival probability.
  prob_S1 <- pmax(prob_S1, prob_S0)

  S0 <- matrix(
    1L,
    nrow = N,
    ncol = K
  )

  S1 <- matrix(
    1L,
    nrow = N,
    ncol = K
  )

  if (K > 1L) {
    for (k in 2:K) {
      # Use the same latent random number under both potential treatments.
      # Because prob_S1 >= prob_S0, this guarantees S1 >= S0.
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

  # ---- Potential outcomes Y1 and Y0 -----------------------------------------
  if (Y_type == 1L) {
    dat[, EY1 :=
          as.numeric(X_rep %*% (Y_coef + Y_sigma)) +
          5 +
          beta_S * S1 +
          0.5 +
          gamma_Y1 * time]

    dat[, EY0 :=
          as.numeric(X_rep %*% Y_coef) +
          5 +
          beta_S * S0 +
          gamma_Y0 * time]

    dat[, Y1 := stats::rnorm(
      .N,
      mean = EY1,
      sd = 3
    )]

    dat[, Y0 := stats::rnorm(
      .N,
      mean = EY0,
      sd = 3
    )]
  } else {
    dat[, logit_Y1 :=
          logit(Prob_Y1) +
          as.numeric(X_rep %*% (Y_coef + Y_sigma)) +
          beta_S * S1 +
          gamma_Y1 * time]

    dat[, logit_Y0 :=
          logit(Prob_Y0) +
          as.numeric(X_rep %*% Y_coef) +
          beta_S * S0 +
          gamma_Y0 * time]

    dat[, prob_Y1 := stats::plogis(logit_Y1)]
    dat[, prob_Y0 := stats::plogis(logit_Y0)]

    dat[, Y1 := stats::rbinom(
      .N,
      size = 1L,
      prob = prob_Y1
    )]

    dat[, Y0 := stats::rbinom(
      .N,
      size = 1L,
      prob = prob_Y0
    )]
  }

  # ---- Observed survival and outcome ----------------------------------------
  dat[, S := A * S1 + (1L - A) * S0]
  dat[, Y := A * Y1 + (1L - A) * Y0]

  # Outcome is structurally missing after death.
  dat[S == 0L, Y := NA_real_]

  dat[, `:=`(
    U11 = S1 * S0,
    S1minusS0 = S1 - S0
  )]

  data.table::setorder(dat, id, time)

  output_dat <- dat[
    ,
    c(
      "id",
      "time",
      "Pi",
      "S1","S0",
      "S",
      "A","Y1","Y0",
      "Y",
      paste0("X", seq_len(n_covariates))
    ),
    with = FALSE
  ]

  # Round only the final returned dataset.
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

# -----------------------------------------------------------------------------
# Generate reproducible example datasets
# -----------------------------------------------------------------------------
ConSample <- generate_data_example(
  N = 200,
  K = 3,
  cov_mtx = cov_mtx,
  rho_mtx = rho_mtx,
  a_coef = a_coef,
  S_con_coef = S_con_coef,
  Y_coef = Y_coef,
  Y_type = 1L,
  seed = 123
)$dat

BiSample <- generate_data_example(
  N = 400,
  K = 3,
  cov_mtx = cov_mtx,
  rho_mtx = rho_mtx,
  a_coef = a_coef,
  S_con_coef = S_con_coef,
  Y_coef = Y_coef,
  Y_type = 2L,
  seed = 456
)$dat


# ====================== Imperfect Clinical Dataset ======================
ImperfectConSample <- as.data.frame(ConSample[,c("id","time","S","A","Y", paste0("X", 1:6))])

# Only rename structural variables.
# Covariate names X1-X6 remain unchanged.
rename_map <- c(
  id   = "patient_id",
  time = "visit_month",
  A    = "treatment",
  S    = "alive_status",
  Y    = "clinical_outcome"
)

names(ImperfectConSample)[
  match(names(rename_map), names(ImperfectConSample))
] <- unname(rename_map)


# Noncanonical patient IDs.
ImperfectConSample$patient_id <- paste0(
  "PT-",
  sprintf("%04d", as.integer(ImperfectConSample$patient_id))
)

# Map standard times 0, 1, 2 to raw clinical visit months.
raw_visit_months <- c(0, 6, 12)

ImperfectConSample$visit_month <- as.character(
  raw_visit_months[ImperfectConSample$visit_month + 1L]
)

# Noncanonical binary encodings.
ImperfectConSample$treatment <- as.character(
  ImperfectConSample$treatment
)

ImperfectConSample$alive_status <- as.character(
  ImperfectConSample$alive_status
)

# Current rule: outcome after death is structural missingness.
ImperfectConSample$clinical_outcome[
  ImperfectConSample$alive_status == "0"
] <- NA

# Add realistic administrative variables.
set.seed(781)
subject_ids <- unique(ImperfectConSample$patient_id)

incomplete_id      <- subject_ids[1L]
missing_cov_id     <- subject_ids[2L]
missing_outcome_id <- subject_ids[3L]
missing_id_subject <- subject_ids[4L]

# One incomplete subject.
ImperfectConSample <- ImperfectConSample[
  !(
    ImperfectConSample$patient_id == incomplete_id &
      ImperfectConSample$visit_month == "12"
  ),
  ,
  drop = FALSE
]

# One subject with a missing required covariate.
ImperfectConSample$X1[
  ImperfectConSample$patient_id == missing_cov_id &
    ImperfectConSample$visit_month == "6"
] <- NA_real_

# One survivor with a missing outcome.
outcome_row <- which(
  ImperfectConSample$patient_id == missing_outcome_id &
    ImperfectConSample$visit_month == "6" &
    ImperfectConSample$alive_status == "1"
)

if (length(outcome_row)) {
  ImperfectConSample$clinical_outcome[outcome_row[1L]] <- NA
}

# One row with a missing patient ID.
missing_id_row <- which(
  ImperfectConSample$patient_id == missing_id_subject &
    ImperfectConSample$visit_month == "6"
)

if (length(missing_id_row)) {
  ImperfectConSample$patient_id[missing_id_row[1L]] <- NA_character_
}

# Randomize record order.
set.seed(789)
ImperfectConSample <- ImperfectConSample[
  sample.int(nrow(ImperfectConSample)),
  ,
  drop = FALSE
]

rownames(ImperfectConSample) <- NULL


# ================ Storage ================
# Save the package data objects used by documentation and validation examples.
usethis::use_data(BiSample, overwrite = TRUE)
usethis::use_data(ImperfectConSample, overwrite = TRUE)

message("Example data rebuilt successfully.")
