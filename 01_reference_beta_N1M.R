# =============================================================================
# 01_reference_beta_N1M.R
#
# Stage 1: Large-sample reference estimates for PDRobust
#
# Great Lakes RStudio configuration
#   R:                 Rtidyverse/4.6.0
#   RStudio:           RStudio/2026.01.2
#   Slurm account:     engin1
#   Partition:         standard
#   QoS:               none
#   Walltime:          24 hours
#   Cores:             4
#   Memory:            64 GB
#   Module commands:   blank
#   Setup file:        blank
#   Email start/end:   enabled
#
# Workflow for each outcome type:
#
#   N = 1,000,000
#        |
#        v
#   generate_data_example()
#        |
#        v
#   DataStandard()
#        |
#        v
#   saveRDS(pd_data)
#        |
#        v
#   HTEAllT(B = 0)
#        |
#        v
#   save beta_t0
#        |
#        v
#   rm(...) + gc()
#        |
#        v
#   readRDS() the exact same pd_data
#        |
#        v
#   HTESepT(B = 0)
#        |
#        v
#   save beta_0
#        |
#        v
#   rm(...) + gc()
#
# Continuous outcome is completed first. Binary outcome then repeats the same
# workflow. No raw N = 1,000,000 dataset is retained after standardization.
#
# All output paths are relative to the current working directory. No username-
# specific or hard-coded /scratch path is used.
# =============================================================================


# =============================================================================
# 0. Main settings
# =============================================================================

N_REFERENCE <- 1000000L
K <- 3L
TARGET_TIMES <- c(1, 2)

OUTCOME_TYPES <- c("continuous", "binary")

REFERENCE_SEEDS <- c(
  continuous = 2026091201L,
  binary     = 2026091202L
)

# All files are saved below the directory from which this script is run.
OUTPUT_DIR <- file.path(
  getwd(),
  "reference_beta_N1M_output"
)

dir.create(
  OUTPUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

message("Output directory: ", normalizePath(OUTPUT_DIR, mustWork = FALSE))


# =============================================================================
# 1. Use the allocated 4-core session conservatively
# =============================================================================

REQUESTED_CORES <- 4L

allocated_cores <- suppressWarnings(
  as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", unset = NA_character_))
)

if (!is.na(allocated_cores) && allocated_cores < REQUESTED_CORES) {
  warning(
    "SLURM_CPUS_PER_TASK reports only ",
    allocated_cores,
    " cores, while this script was prepared for a 4-core session."
  )
}

# This stage is not Monte Carlo-parallelized. It uses one analysis at a time to
# minimize peak memory. These settings only allow underlying numerical/data
# operations to use up to the allocated core count when supported.
Sys.setenv(
  OMP_NUM_THREADS = as.character(REQUESTED_CORES),
  OPENBLAS_NUM_THREADS = as.character(REQUESTED_CORES),
  MKL_NUM_THREADS = as.character(REQUESTED_CORES),
  VECLIB_MAXIMUM_THREADS = as.character(REQUESTED_CORES),
  NUMEXPR_NUM_THREADS = as.character(REQUESTED_CORES)
)


# =============================================================================
# 2. Install/load required packages
# =============================================================================

required_packages <- c(
  "data.table",
  "MASS",
  "bindata"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages)) {
  install.packages(missing_packages)
}

# Install the current PDRobust package from GitHub only when it is unavailable.
if (!requireNamespace("PDRobust", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
  }

  remotes::install_github(
    "whhuan/PD_Robust",
    dependencies = TRUE,
    upgrade = "never"
  )
}

suppressPackageStartupMessages({
  library(PDRobust)
  library(data.table)
})

data.table::setDTthreads(REQUESTED_CORES)

message(
  "PDRobust version: ",
  as.character(utils::packageVersion("PDRobust"))
)


# =============================================================================
# 3. Data-generating parameters
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

# Coefficients correspond to X1-X6.
a_coef <- c(
  0.8, -0.4, 0.6,
  -0.5, 0.7, 0.1
)

S_con_coef <- c(
  0.6, 0.5, 0.1,
  0.4, 0.5, -0.4
)

Y_coef <- c(
  0.2, 0.3, -0.1,
  0.7, 0.2, -0.4
)


# =============================================================================
# 4. Embedded data-generating function
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

  X_rep <- X[
    rep(seq_len(N), each = K),
    ,
    drop = FALSE
  ]

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
  linear_S0 <- logit(Prob_S0) +
    as.numeric(X %*% S_con_coef)

  linear_S1 <- logit(Prob_S1) +
    as.numeric(X %*% (S_con_coef + S_sigma))

  prob_S0 <- stats::plogis(linear_S0)
  prob_S1 <- stats::plogis(linear_S1)

  # Guarantee monotonicity S1 >= S0 in the simulated potential survival process.
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

  data.table::setorder(
    dat,
    id,
    time
  )

  output_dat <- dat[
    ,
    c(
      "id",
      "time",
      "Pi",
      "S1",
      "S0",
      "S",
      "A",
      "Y1",
      "Y0",
      "Y",
      paste0("X", seq_len(n_covariates))
    ),
    with = FALSE
  ]

  # Round only the final returned dataset.
  numeric_columns <- c(
    "Y",
    "Pi",
    paste0("X", seq_len(n_covariates))
  )

  output_dat[
    ,
    (numeric_columns) := lapply(
      .SD,
      function(value) {
        if (is.double(value)) {
          round(value, digits = 3)
        } else {
          value
        }
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
# 5. PDRobust analysis specification
# =============================================================================

COVARIATES <- paste0("X", 1:6)

# The HTE coefficient vector is defined using all six generated baseline
# covariates as effect modifiers.
INTEREST_VARS <- COVARIATES

PS_FORMULA <- A ~
  X1 + X2 + X3 + X4 + X5 + X6

PRIN_FORMULA <- S ~
  (X1 + X2 + X3 + X4 + X5 + X6) * A +
  time

OUT_FORMULA <- Y ~
  (X1 + X2 + X3 + X4 + X5 + X6) * A +
  time


make_mapping <- function(outcome_type) {
  y_type <- if (identical(outcome_type, "binary")) {
    "B"
  } else {
    "C"
  }

  PDRobust::Mapping(
    id = "id",
    time = "time",
    treatment = "A",
    survival = "S",
    outcome = "Y",
    baseline_time = 0,
    cutoff_time = K - 1L,
    covariates = COVARIATES,
    interest_vars = INTEREST_VARS,
    y_type = y_type
  )
}


# =============================================================================
# 6. Small utility functions
# =============================================================================

write_csv <- function(x, filename) {
  utils::write.csv(
    x,
    file = file.path(OUTPUT_DIR, filename),
    row.names = FALSE
  )
}


capture_with_warnings <- function(expr) {
  warnings <- character()

  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      warnings <<- c(
        warnings,
        conditionMessage(w)
      )
      invokeRestart("muffleWarning")
    }
  )

  list(
    value = value,
    warnings = unique(warnings)
  )
}


save_warning_log <- function(
    warnings,
    outcome_type,
    estimator,
    filename
) {
  if (!length(warnings)) {
    return(invisible(NULL))
  }

  warning_table <- data.frame(
    outcome = outcome_type,
    estimator = estimator,
    warning = warnings,
    stringsAsFactors = FALSE
  )

  write_csv(
    warning_table,
    filename
  )
}


# =============================================================================
# 7. Run one outcome type
# =============================================================================

run_reference_analysis <- function(outcome_type) {
  if (!outcome_type %in% OUTCOME_TYPES) {
    stop("Unknown outcome type: ", outcome_type)
  }

  message("")
  message("==============================================================")
  message("Starting ", outcome_type, " reference analysis")
  message("N = ", format(N_REFERENCE, big.mark = ","))
  message("==============================================================")

  generator_y_type <- if (identical(outcome_type, "continuous")) {
    1L
  } else {
    2L
  }

  seed <- unname(
    REFERENCE_SEEDS[outcome_type]
  )

  # Relative/local filenames only; no hard-coded user path.
  pd_data_file <- file.path(
    OUTPUT_DIR,
    paste0(
      "pd_data_",
      outcome_type,
      "_N1M.rds"
    )
  )

  hteallt_beta_file <- paste0(
    "beta_t0_HTEAllT_",
    outcome_type,
    "_N1M.csv"
  )

  htesept_beta_file <- paste0(
    "beta_0_HTESepT_",
    outcome_type,
    "_N1M.csv"
  )

  # ---------------------------------------------------------------------------
  # Step 1. Generate N = 1,000,000
  # ---------------------------------------------------------------------------

  message("[1/7] Generating ", outcome_type, " data...")

  generated <- generate_data_example(
    N = N_REFERENCE,
    K = K,
    cov_mtx = cov_mtx,
    rho_mtx = rho_mtx,
    a_coef = a_coef,
    S_con_coef = S_con_coef,
    Y_coef = Y_coef,
    Y_type = generator_y_type,
    seed = seed
  )

  raw_data <- generated$dat

  message(
    "      rows = ",
    format(nrow(raw_data), big.mark = ","),
    "; subjects = ",
    format(data.table::uniqueN(raw_data$id), big.mark = ",")
  )

  rm(generated)
  gc(full = TRUE)

  # ---------------------------------------------------------------------------
  # Step 2. DataStandard()
  # ---------------------------------------------------------------------------

  message("[2/7] Standardizing data with DataStandard()...")

  mapping <- make_mapping(
    outcome_type
  )

  pd_data <- PDRobust::DataStandard(
    raw_data,
    mapping,
    drop = TRUE
  )

  # Raw generated data are not needed after standardization.
  rm(raw_data, mapping)
  gc(full = TRUE)

  # ---------------------------------------------------------------------------
  # Step 3. Save standardized pd_data
  # ---------------------------------------------------------------------------

  message("[3/7] Saving standardized pd_data...")

  saveRDS(
    pd_data,
    file = pd_data_file,
    compress = FALSE
  )

  message(
    "      saved: ",
    pd_data_file
  )

  # ---------------------------------------------------------------------------
  # Step 4. HTEAllT(B = 0): reference beta_t0
  # ---------------------------------------------------------------------------

  message("[4/7] Running HTEAllT(B = 0)...")

  all_capture <- capture_with_warnings(
    PDRobust::HTEAllT(
      data = pd_data,
      ps_fo = PS_FORMULA,
      prin_fo = PRIN_FORMULA,
      out_fo = OUT_FORMULA,
      B = 0,
      verbose = FALSE
    )
  )

  fit_all <- all_capture$value

  save_warning_log(
    warnings = all_capture$warnings,
    outcome_type = outcome_type,
    estimator = "HTEAllT",
    filename = paste0(
      "warnings_HTEAllT_",
      outcome_type,
      "_N1M.csv"
    )
  )

  beta_t0 <- fit_all$summary

  beta_t0$outcome <- outcome_type
  beta_t0$estimator <- "HTEAllT"
  beta_t0$reference_parameter <- "beta_t0"
  beta_t0$reference_N <- N_REFERENCE
  beta_t0$seed <- seed

  beta_t0 <- beta_t0[
    ,
    c(
      "outcome",
      "estimator",
      "reference_parameter",
      "reference_N",
      "seed",
      setdiff(
        names(beta_t0),
        c(
          "outcome",
          "estimator",
          "reference_parameter",
          "reference_N",
          "seed"
        )
      )
    )
  ]

  write_csv(
    beta_t0,
    hteallt_beta_file
  )

  message(
    "      beta_t0 saved: ",
    file.path(OUTPUT_DIR, hteallt_beta_file)
  )

  # ---------------------------------------------------------------------------
  # Step 5. Remove HTEAllT objects and pd_data from memory
  # ---------------------------------------------------------------------------

  message("[5/7] Clearing HTEAllT objects and pd_data from memory...")

  rm(
    fit_all,
    beta_t0,
    all_capture,
    pd_data
  )

  gc(full = TRUE)

  # ---------------------------------------------------------------------------
  # Step 6. Reload the EXACT same pd_data and run HTESepT(B = 0)
  # ---------------------------------------------------------------------------

  message("[6/7] Reloading the same pd_data and running HTESepT(B = 0)...")

  pd_data <- readRDS(
    pd_data_file
  )

  sep_capture <- capture_with_warnings(
    PDRobust::HTESepT(
      data = pd_data,
      ps_fo = PS_FORMULA,
      prin_fo = PRIN_FORMULA,
      out_fo = OUT_FORMULA,
      target_time = TARGET_TIMES,
      B = 0,
      verbose = FALSE
    )
  )

  fit_sep <- sep_capture$value

  save_warning_log(
    warnings = sep_capture$warnings,
    outcome_type = outcome_type,
    estimator = "HTESepT",
    filename = paste0(
      "warnings_HTESepT_",
      outcome_type,
      "_N1M.csv"
    )
  )

  beta_0 <- fit_sep$summary

  beta_0$outcome <- outcome_type
  beta_0$estimator <- "HTESepT"
  beta_0$reference_parameter <- "beta_0"
  beta_0$reference_N <- N_REFERENCE
  beta_0$seed <- seed

  beta_0 <- beta_0[
    ,
    c(
      "outcome",
      "estimator",
      "reference_parameter",
      "reference_N",
      "seed",
      setdiff(
        names(beta_0),
        c(
          "outcome",
          "estimator",
          "reference_parameter",
          "reference_N",
          "seed"
        )
      )
    )
  ]

  write_csv(
    beta_0,
    htesept_beta_file
  )

  message(
    "      beta_0 saved: ",
    file.path(OUTPUT_DIR, htesept_beta_file)
  )

  # ---------------------------------------------------------------------------
  # Step 7. Final cleanup
  # ---------------------------------------------------------------------------

  message("[7/7] Final cleanup for ", outcome_type, "...")

  rm(
    fit_sep,
    beta_0,
    sep_capture,
    pd_data
  )

  gc(full = TRUE)

  message("Completed: ", outcome_type)

  invisible(
    list(
      outcome = outcome_type,
      pd_data_file = pd_data_file,
      beta_t0_file = file.path(
        OUTPUT_DIR,
        hteallt_beta_file
      ),
      beta_0_file = file.path(
        OUTPUT_DIR,
        htesept_beta_file
      )
    )
  )
}


# =============================================================================
# 8. Run continuous first, then binary
# =============================================================================

run_status <- data.frame(
  outcome = character(),
  status = character(),
  started_at = character(),
  finished_at = character(),
  message = character(),
  stringsAsFactors = FALSE
)

for (outcome_type in OUTCOME_TYPES) {
  started_at <- Sys.time()

  result <- tryCatch(
    {
      run_reference_analysis(
        outcome_type
      )

      list(
        status = "success",
        message = ""
      )
    },
    error = function(e) {
      list(
        status = "failed",
        message = conditionMessage(e)
      )
    }
  )

  finished_at <- Sys.time()

  run_status <- rbind(
    run_status,
    data.frame(
      outcome = outcome_type,
      status = result$status,
      started_at = format(
        started_at,
        "%Y-%m-%d %H:%M:%S %Z"
      ),
      finished_at = format(
        finished_at,
        "%Y-%m-%d %H:%M:%S %Z"
      ),
      message = result$message,
      stringsAsFactors = FALSE
    )
  )

  write_csv(
    run_status,
    "run_status.csv"
  )

  gc(full = TRUE)

  if (!identical(result$status, "success")) {
    stop(
      "Stage 1 stopped after failure for ",
      outcome_type,
      ": ",
      result$message
    )
  }
}


# =============================================================================
# 9. Save one compact reference-beta RDS object for Stage 2
# =============================================================================

reference_beta <- list(
  continuous = list(
    beta_t0 = utils::read.csv(
      file.path(
        OUTPUT_DIR,
        "beta_t0_HTEAllT_continuous_N1M.csv"
      ),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    beta_0 = utils::read.csv(
      file.path(
        OUTPUT_DIR,
        "beta_0_HTESepT_continuous_N1M.csv"
      ),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  ),
  binary = list(
    beta_t0 = utils::read.csv(
      file.path(
        OUTPUT_DIR,
        "beta_t0_HTEAllT_binary_N1M.csv"
      ),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    beta_0 = utils::read.csv(
      file.path(
        OUTPUT_DIR,
        "beta_0_HTESepT_binary_N1M.csv"
      ),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  )
)

saveRDS(
  reference_beta,
  file = file.path(
    OUTPUT_DIR,
    "reference_beta_N1M.rds"
  ),
  compress = FALSE
)

rm(reference_beta)
gc(full = TRUE)


# =============================================================================
# 10. Save reproducibility information
# =============================================================================

analysis_settings <- list(
  N_reference = N_REFERENCE,
  K = K,
  target_times = TARGET_TIMES,
  outcome_types = OUTCOME_TYPES,
  reference_seeds = REFERENCE_SEEDS,
  covariates = COVARIATES,
  interest_vars = INTEREST_VARS,
  ps_formula = deparse(PS_FORMULA),
  prin_formula = deparse(PRIN_FORMULA),
  out_formula = deparse(OUT_FORMULA),
  PDRobust_version = as.character(
    utils::packageVersion("PDRobust")
  ),
  R_version = R.version.string,
  platform = R.version$platform,
  requested_cores = REQUESTED_CORES,
  output_directory = normalizePath(
    OUTPUT_DIR,
    mustWork = FALSE
  ),
  completed_at = format(
    Sys.time(),
    "%Y-%m-%d %H:%M:%S %Z"
  )
)

saveRDS(
  analysis_settings,
  file = file.path(
    OUTPUT_DIR,
    "analysis_settings.rds"
  )
)

writeLines(
  capture.output(
    utils::sessionInfo()
  ),
  con = file.path(
    OUTPUT_DIR,
    "sessionInfo.txt"
  )
)

message("")
message("==============================================================")
message("Stage 1 completed successfully.")
message("Saved standardized datasets:")
message(
  "  ",
  file.path(
    OUTPUT_DIR,
    "pd_data_continuous_N1M.rds"
  )
)
message(
  "  ",
  file.path(
    OUTPUT_DIR,
    "pd_data_binary_N1M.rds"
  )
)
message("Reference beta object:")
message(
  "  ",
  file.path(
    OUTPUT_DIR,
    "reference_beta_N1M.rds"
  )
)
message("==============================================================")
