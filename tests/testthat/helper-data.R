make_pd_raw <- function(n = 240L, times = 0:2, binary_outcome = FALSE) {
  simulate_pd_test_data(
    n = n,
    times = times,
    outcome = if (binary_outcome) "binary" else "continuous",
    seed = 20260728L
  )
}

make_pd_workflow <- function(times = 0:2, binary_outcome = FALSE,
                             n = 240L) {
  raw <- make_pd_raw(
    n = n, times = times, binary_outcome = binary_outcome
  )
  mapping <- Mapping(
    id = "id", time = "time", treatment = "A",
    survival = "S", outcome = "Y",
    baseline_time = min(times),
    cutoff_time = max(times),
    covariates = c("X1", "X2", "X4"),
    interest_vars = c("X1", "X2"),
    y_type = if (binary_outcome) "B" else "C"
  )
  prepared <- DataStandard(raw, mapping)
  list(
    raw = raw,
    mapping = mapping,
    data = prepared,
    ps_fo = A ~ X1 + X2 + X4,
    prin_fo = if (length(times) <= 2L) {
      S ~ X1 + X2 + X4 + A
    } else {
      S ~ X1 + X2 + X4 + A + time
    },
    out_fo = Y ~ X1 + X2 + A
  )
}

make_extreme_diagnostic_workflow <- function(n = 320L) {
  set.seed(20260729)
  id <- seq_len(n)
  Z <- seq(-3.5, 3.5, length.out = n)
  X1 <- stats::rnorm(n)
  X2 <- stats::rnorm(n)
  X4 <- stats::rbinom(n, size = 1, prob = 0.42)
  latent_treatment <- 3 * Z + stats::rnorm(n, sd = 0.9)
  A <- as.integer(latent_treatment > 0)
  flip <- c(20L, 40L, n - 39L, n - 19L)
  A[flip] <- 1L - A[flip]

  S0 <- rep(1L, n)
  S1 <- stats::rbinom(
    n, size = 1,
    prob = stats::plogis(
      0.25 - 0.15 * X1 + 0.20 * X2 + 0.15 * X4 + 0.10 * A
    )
  )
  S2 <- S1 * stats::rbinom(
    n, size = 1,
    prob = stats::plogis(
      0.10 - 0.10 * X1 + 0.15 * X2 + 0.10 * X4 + 0.10 * A
    )
  )
  if (length(unique(S1)) < 2L) S1[c(1L, n)] <- c(0L, 1L)
  at_risk_2 <- which(S1 == 1L)
  if (length(unique(S2[at_risk_2])) < 2L && length(at_risk_2) >= 2L) {
    S2[at_risk_2[1:2]] <- c(0L, 1L)
  }

  make_rows <- function(time, S) {
    Y <- 0.5 + 0.3 * X1 - 0.2 * X2 + 0.25 * A +
      0.1 * time + stats::rnorm(n, sd = 0.8)
    Y[S == 0L] <- NA
    data.frame(
      id = id, time = time, Z = Z, X1 = X1, X2 = X2, X4 = X4,
      A = A, S = S, Y = Y
    )
  }

  raw <- rbind(make_rows(0, S0), make_rows(1, S1), make_rows(2, S2))
  mapping <- Mapping(
    id = "id", time = "time", treatment = "A",
    survival = "S", outcome = "Y",
    baseline_time = 0,
    cutoff_time = 2,
    covariates = c("Z", "X1", "X2", "X4"),
    interest_vars = c("X1", "X2"),
    y_type = "C"
  )
  list(
    raw = raw,
    mapping = mapping,
    data = DataStandard(raw, mapping),
    ps_fo = A ~ Z,
    prin_fo = S ~ X1 + X2 + X4 + A + time
  )
}
