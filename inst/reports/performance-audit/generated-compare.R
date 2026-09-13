root <- "inst/reports/performance-audit"
new <- new.env(parent = globalenv())
old <- new.env(parent = globalenv())
for (env in list(new, old)) {
  for (file in list.files("R", "[.]R$", full.names = TRUE)) sys.source(file, env)
}
for (file in list.files(file.path(root, "baseline"), "[.]R$", full.names = TRUE)) {
  if (basename(file) != "generate_example_data.R") sys.source(file, old)
}
g <- new.env(parent = globalenv())
definitions <- c("generate_data_example", "cov_mtx", "rho_mtx", "a_coef", "S_con_coef", "Y_coef")
for (expr in parse(file.path(root, "baseline/generate_example_data.R"))) {
  if (is.call(expr) && identical(expr[[1L]], as.name("<-")) &&
      as.character(expr[[2L]])[1L] %in% definitions) eval(expr, g)
}
data.table::setDTthreads(1L)
for (type in 1:2) for (k in c(1L, 3L, 6L)) {
  n <- if (k == 3L) 10000L else 1000L
  raw <- with(g, generate_data_example(n, k, cov_mtx, rho_mtx, a_coef,
    S_con_coef, Y_coef, Y_type = type, seed = 321))$dat
  # Every generated column, including the unused potential outcomes, is retained.
  map <- new$Mapping("id", "time", "A", "S", "Y", 0, k - 1,
                    paste0("X", 1:6), paste0("X", 1:6), if (type == 1) "C" else "B")
  original <- data.table::copy(raw)
  for (shuffle in c(FALSE, TRUE)) {
    if (shuffle) { set.seed(42); raw <- raw[sample(nrow(raw))] }
    stopifnot(identical(old$DataCheck(raw, map), new$DataCheck(raw, map)))
    set.seed(77); before <- .Random.seed
    a <- old$DataStandard(raw, map, drop = TRUE)
    stopifnot(identical(before, .Random.seed))
    b <- new$DataStandard(raw, map, drop = TRUE)
    stopifnot(identical(before, .Random.seed), identical(a, b))
  }
  data.table::setorder(raw, id, time)
  stopifnot(identical(raw, original))
  # Noninteger observed visit grids and narrower windows retain their audit maps.
  if (k == 6L) {
    raw$time <- c(-1.2, 0.12345, 1.3333, 5.1, 9.87654, 22)[raw$time + 1L]
    map$baseline_time <- 0.12345
    map$cutoff_time <- 9.87654
    stopifnot(identical(old$DataStandard(raw, map), new$DataStandard(raw, map)))
  }
  cat("PASS generated outcome", type, "K", k, "N", n, "ordered/shuffled and RNG\n")
}
