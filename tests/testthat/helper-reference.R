reference_hte_components <- function(workflow) {
  panel <- as.data.frame(workflow$data)
  map <- attr(workflow$data, "pd_mapping")
  panel <- panel[order(panel[[map$id_col]], panel[[map$time_col]]), , drop = FALSE]
  baseline <- panel[panel[[map$time_col]] == map$baseline_time, , drop = FALSE]
  baseline <- baseline[order(baseline[[map$id_col]]), , drop = FALSE]
  ids <- baseline[[map$id_col]]
  cutoff <- panel[panel[[map$time_col]] == map$cutoff_time, , drop = FALSE]
  cutoff <- cutoff[match(ids, cutoff[[map$id_col]]), , drop = FALSE]

  pi <- pmin(pmax(as.numeric(PDRobust:::.pd_pspred_impl(
    workflow$ps_fo, panel, baseline, map
  )), 0.01), 0.99)
  p0_all <- PDRobust:::.pd_prinpred_impl(
    workflow$prin_fo, panel, panel, 0, map
  )
  p1_all <- PDRobust:::.pd_prinpred_impl(
    workflow$prin_fo, panel, panel, 1, map
  )
  cutoff_idx <- which(panel[[map$time_col]] == map$cutoff_time)
  score_ids <- panel[[map$id_col]][cutoff_idx]
  alignment <- match(ids, score_ids)
  p0 <- as.numeric(p0_all[cutoff_idx][alignment])
  p1 <- as.numeric(p1_all[cutoff_idx][alignment])

  A <- as.numeric(cutoff[[map$A_col]])
  S <- as.numeric(cutoff[[map$S_col]])
  psi_s0 <- (A == 0) * (S - p0) / (1 - pi) + p0
  pip1 <- pmin(pmax(p1 * pi, 0.005), 0.995)
  X <- cbind(
    Intercept = 1,
    as.matrix(baseline[map$interest_vars])
  )
  storage.mode(X) <- "double"

  list(
    panel = panel, mapping = map, baseline = baseline, cutoff = cutoff,
    ids = ids, pi = pi, p0 = p0, p1 = p1, A = A, S = S,
    psi_s0 = psi_s0, pip1 = pip1, X = X
  )
}

reference_binary_root <- function(X, phi_diff, weight) {
  score <- function(beta) {
    eta <- as.vector(X %*% beta)
    gamma_hat <- 2 / (1 + exp(-eta)) - 1
    as.vector(crossprod(X, phi_diff - gamma_hat * weight))
  }
  jacobian <- function(beta) {
    eta <- as.vector(X %*% beta)
    derivative_weight <- 2 * exp(eta) / (1 + exp(eta))^2 * weight
    -crossprod(X, X * derivative_weight)
  }
  rootSolve::multiroot(
    f = score, jacfunc = jacobian,
    start = rep(0, ncol(X)), maxiter = 100, rtol = 1e-6
  )$root
}

reference_time_phi <- function(components, workflow, time_value) {
  map <- components$mapping
  current <- components$panel[
    components$panel[[map$time_col]] == time_value, , drop = FALSE
  ]
  current <- current[
    match(components$ids, current[[map$id_col]]), , drop = FALSE
  ]
  always_ids <- components$cutoff[[map$id_col]][components$S == 1]
  fit_dat <- current[current[[map$id_col]] %in% always_ids, , drop = FALSE]
  mu0 <- PDRobust:::.pd_outpred_impl(
    workflow$out_fo, fit_dat, current, 0, map
  )
  mu1 <- PDRobust:::.pd_outpred_impl(
    workflow$out_fo, fit_dat, current, 1, map
  )
  y <- current[[map$Y_col]]
  y[is.na(y)] <- 0
  phi1 <- components$p0 * components$S * components$A *
    (y - mu1) / components$pip1 + mu1 * components$psi_s0
  phi0 <- (components$A == 0) *
    (y * components$S - mu0 * components$p0) /
    (1 - components$pi) + mu0 * components$p0
  as.numeric(phi1 - phi0)
}

reference_htesep <- function(workflow, target_time) {
  components <- reference_hte_components(workflow)
  estimate <- vapply(target_time, function(time_value) {
    phi_diff <- reference_time_phi(components, workflow, time_value)
    if (identical(components$mapping$y_type, "B")) {
      reference_binary_root(components$X, phi_diff, components$psi_s0)
    } else {
      drop(solve(
        crossprod(components$X, components$X * components$psi_s0),
        crossprod(components$X, phi_diff)
      ))
    }
  }, numeric(ncol(components$X)))
  estimate <- t(estimate)
  rownames(estimate) <- as.character(target_time)
  colnames(estimate) <- colnames(components$X)
  estimate
}

reference_hteall <- function(workflow) {
  components <- reference_hte_components(workflow)
  map <- components$mapping
  subject_index <- match(
    components$panel[[map$id_col]], components$ids
  )
  X <- cbind(
    Intercept = 1,
    as.matrix(components$baseline[map$interest_vars])[
      subject_index, , drop = FALSE
    ]
  )
  analysis_times <- sort(unique(components$panel[[map$time_col]]))
  if (length(analysis_times) > 1L) {
    X <- cbind(X, `Time Effect` = components$panel[[map$time_col]])
  }
  storage.mode(X) <- "double"

  A <- components$A[subject_index]
  S <- components$S[subject_index]
  pi <- components$pi[subject_index]
  p0 <- components$p0[subject_index]
  psi_s0 <- components$psi_s0[subject_index]
  pip1 <- components$pip1[subject_index]
  always_ids <- components$cutoff[[map$id_col]][components$S == 1]
  fit_dat <- components$panel[
    components$panel[[map$id_col]] %in% always_ids, , drop = FALSE
  ]
  mu0 <- PDRobust:::.pd_outpred_impl(
    workflow$out_fo, fit_dat, components$panel, 0, map
  )
  mu1 <- PDRobust:::.pd_outpred_impl(
    workflow$out_fo, fit_dat, components$panel, 1, map
  )
  y <- components$panel[[map$Y_col]]
  y[is.na(y)] <- 0
  phi1 <- p0 * S * A * (y - mu1) / pip1 + mu1 * psi_s0
  phi0 <- (A == 0) * (y * S - mu0 * p0) / (1 - pi) + mu0 * p0
  phi_diff <- as.numeric(phi1 - phi0)

  beta <- if (identical(map$y_type, "B")) {
    reference_binary_root(X, phi_diff, psi_s0)
  } else {
    drop(solve(crossprod(X, X * psi_s0), crossprod(X, phi_diff)))
  }
  stats::setNames(as.numeric(beta), colnames(X))
}
