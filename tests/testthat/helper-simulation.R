# Pure deterministic test-data generator adapted from
# data-raw/generate_example_data.R. It has no file, package-development, or
# global-option side effects.

simulate_pd_test_data <- function(
    n = 240L,
    times = 0:2,
    outcome = c("continuous", "binary"),
    seed = 20260728L,
    invalid = c(
      "none", "missing_column", "invalid_binary", "duplicate",
      "missing_visit", "survivor_missing", "resurrection",
      "zero_variance", "rank_deficient", "separation",
      "nonvarying_outcome"
    )) {
  outcome <- match.arg(outcome)
  invalid <- match.arg(invalid)
  stopifnot(
    length(n) == 1L, is.finite(n), n >= 30L, n == floor(n),
    is.numeric(times), length(times) >= 1L, all(is.finite(times)),
    length(seed) == 1L, !is.na(seed)
  )
  n <- as.integer(n)
  times <- sort(unique(as.numeric(times)))

  old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (old_seed_exists) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit({
    if (old_seed_exists) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed)

  continuous_covariance <- matrix(
    c(1.00, 0.04, 0.27,
      0.04, 1.00, 0.09,
      0.27, 0.09, 1.00),
    nrow = 3L, byrow = TRUE
  )
  binary_correlation <- matrix(
    c(1.00, 0.07, 0.02,
      0.07, 1.00, 0.17,
      0.02, 0.17, 1.00),
    nrow = 3L, byrow = TRUE
  )
  continuous <- matrix(stats::rnorm(n * 3L), ncol = 3L) %*%
    chol(continuous_covariance)
  latent_binary <- matrix(stats::rnorm(n * 3L), ncol = 3L) %*%
    chol(binary_correlation)
  binary <- 1L * (latent_binary > 0)
  X <- cbind(continuous, binary)
  colnames(X) <- paste0("X", seq_len(6L))

  a_coef <- c(0.45, -0.30, 0.25, -0.20, 0.30, 0.10)
  s_coef <- c(-0.25, 0.20, 0.08, 0.15, 0.10, -0.12)
  y_coef <- c(0.45, -0.25, 0.12, 0.15, 0.08, -0.10)
  propensity <- stats::plogis(-0.10 + drop(X %*% a_coef))
  A <- stats::rbinom(n, 1L, propensity)
  if (length(unique(A)) < 2L) A[c(1L, n)] <- c(0L, 1L)

  s0 <- s1 <- vector("list", length(times))
  if (length(times) == 1L) {
    p0 <- stats::plogis(0.35 + drop(X %*% s_coef))
    p1 <- stats::plogis(0.55 + drop(X %*% (s_coef + 0.03)))
    s0[[1L]] <- stats::rbinom(n, 1L, p0)
    s1[[1L]] <- stats::rbinom(n, 1L, p1)
  } else {
    s0[[1L]] <- s1[[1L]] <- rep(1L, n)
    for (j in 2:length(times)) {
      p0 <- stats::plogis(
        0.85 - 0.18 * (j - 1L) + drop(X %*% s_coef)
      )
      p1 <- stats::plogis(
        1.05 - 0.18 * (j - 1L) + drop(X %*% (s_coef + 0.03))
      )
      transition0 <- stats::rbinom(n, 1L, p0)
      transition1 <- stats::rbinom(n, 1L, p1)
      s0[[j]] <- s0[[j - 1L]] * transition0
      s1[[j]] <- s1[[j - 1L]] * transition1
    }
  }

  rows <- lapply(seq_along(times), function(j) {
    S0 <- s0[[j]]
    S1 <- s1[[j]]
    S <- A * S1 + (1L - A) * S0
    eta0 <- 0.40 + drop(X %*% y_coef) + 0.10 * (j - 1L)
    eta1 <- eta0 + 0.35 + 0.08 * X[, 1L]
    if (outcome == "binary") {
      Y0 <- stats::rbinom(n, 1L, stats::plogis(eta0 - 0.65))
      Y1 <- stats::rbinom(n, 1L, stats::plogis(eta1 - 0.65))
    } else {
      Y0 <- eta0 + stats::rnorm(n, sd = 0.75)
      Y1 <- eta1 + stats::rnorm(n, sd = 0.75)
    }
    Y <- A * Y1 + (1L - A) * Y0
    Y[S == 0L] <- NA
    data.frame(
      id = seq_len(n), time = times[[j]],
      X1 = X[, 1L], X2 = X[, 2L], X3 = X[, 3L],
      X4 = X[, 4L], X5 = X[, 5L], X6 = X[, 6L],
      A = A, S = S, Y = Y,
      stringsAsFactors = FALSE
    )
  })
  data <- do.call(rbind, rows)
  data <- data[order(data$id, data$time), , drop = FALSE]
  rownames(data) <- NULL

  if (invalid == "missing_column") data$X2 <- NULL
  if (invalid == "invalid_binary") data$A[1L] <- 2L
  if (invalid == "duplicate") data <- rbind(data, data[1L, , drop = FALSE])
  if (invalid == "missing_visit" && length(times) > 1L) {
    data <- data[!(data$id == 1L & data$time == times[[2L]]), , drop = FALSE]
  }
  if (invalid == "survivor_missing") {
    row <- which(data$S == 1L)[1L]
    data$Y[row] <- NA
  }
  if (invalid == "resurrection" && length(times) > 1L) {
    idx <- which(data$id == 1L)
    data$S[idx] <- c(0L, rep(1L, length(idx) - 1L))
    data$Y[idx[1L]] <- NA
  }
  if (invalid == "zero_variance") data$X1 <- 1
  if (invalid == "rank_deficient") data$X2 <- data$X1
  if (invalid == "separation") data$A <- as.integer(data$X1 > 0)
  if (invalid == "nonvarying_outcome") {
    data$Y[data$S == 1L] <- if (outcome == "binary") 1L else 2
  }
  data
}

make_pd_test_mapping <- function(data, outcome = c("continuous", "binary")) {
  outcome <- match.arg(outcome)
  Mapping(
    id = "id", time = "time", treatment = "A",
    survival = "S", outcome = "Y",
    baseline_time = min(data$time, na.rm = TRUE),
    cutoff_time = max(data$time, na.rm = TRUE),
    covariates = intersect(c("X1", "X2", "X4"), names(data)),
    interest_vars = intersect(c("X1", "X2"), names(data)),
    y_type = if (outcome == "binary") "B" else "C"
  )
}
