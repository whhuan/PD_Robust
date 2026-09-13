# Additional numerical comparisons before public output rounding.
root <- "inst/reports/performance-audit"
new <- new.env(parent = globalenv())
old <- new.env(parent = globalenv())
for (env in list(new, old)) for (file in list.files("R", "[.]R$", full.names = TRUE)) sys.source(file, env)
sys.source(file.path(root, "baseline/data_validation.R"), old)
sys.source("tests/testthat/helper-simulation.R", new)
sys.source("tests/testthat/helper-data.R", new)
for (binary in c(FALSE, TRUE)) {
  w <- new$make_pd_workflow(n = 240L, binary_outcome = binary)
  a <- old$DataStandard(w$raw, w$mapping)
  b <- new$DataStandard(w$raw, w$mapping)
  stopifnot(identical(a, b))
  for (method in c("ps", "prin", "out", "all", "sep")) {
    run <- function(dat) switch(method,
      ps = new$.pd_pspred_impl(w$ps_fo, dat, dat, attr(dat, "pd_mapping")),
      prin = new$.pd_prinpred_impl(w$prin_fo, dat, dat, 0, attr(dat, "pd_mapping")),
      out = new$.pd_outpred_impl(w$out_fo, dat, dat, 1, attr(dat, "pd_mapping")),
      all = new$.pd_hteall_once(dat, w$ps_fo, w$prin_fo, w$out_fo),
      sep = new$.pd_htesep_once(dat, w$ps_fo, w$prin_fo, w$out_fo, c(1, 2)))
    set.seed(42)
    rng <- .Random.seed
    x <- new$.pd_capture_conditions(run(a))
    stopifnot(identical(rng, .Random.seed))
    y <- new$.pd_capture_conditions(run(b))
    stopifnot(identical(x, y), identical(rng, .Random.seed), !inherits(x$value, "error"))
    cat("PASS unrounded", method, "outcome", if (binary) "B" else "C", "\n")
  }
}
