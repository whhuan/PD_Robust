# Run from the package root. Compare with the exact pre-audit working source.
root <- "inst/reports/performance-audit"
new <- new.env(parent = globalenv())
for (file in list.files("R", "[.]R$", full.names = TRUE)) sys.source(file, new)
old <- new.env(parent = globalenv())
for (file in list.files("R", "[.]R$", full.names = TRUE)) sys.source(file, old)
for (file in list.files(file.path(root, "baseline"), "[.]R$", full.names = TRUE)) {
  if (basename(file) != "generate_example_data.R") sys.source(file, old)
}
capture <- function(expr) {
  warnings <- messages <- list()
  value <- tryCatch(withCallingHandlers(expr,
    warning = function(w) {
      warnings[[length(warnings) + 1L]] <<- list(class = class(w), message = conditionMessage(w))
      invokeRestart("muffleWarning")
    }, message = function(m) {
      messages[[length(messages) + 1L]] <<- list(class = class(m), message = conditionMessage(m))
      invokeRestart("muffleMessage")
    }), error = function(e) list(error_class = class(e), error_message = conditionMessage(e)))
  list(value = value, warnings = warnings, messages = messages)
}
comparisons <- 0L
same <- function(a, b, label) {
  if (!identical(a, b)) {
    saveRDS(list(old = a, new = b), file.path(root, "comparison-failure.rds"))
    stop(label, ": ", paste(all.equal(a, b), collapse = "; "))
  }
  comparisons <<- comparisons + 1L
}
sys.source("tests/testthat/helper-simulation.R", new)
sys.source("tests/testthat/helper-data.R", new)
check_case <- function(raw, map, label) {
  before <- if (inherits(raw, "data.table")) data.table::copy(raw) else unserialize(serialize(raw, NULL))
  for (strict in c(FALSE, TRUE)) {
    same(capture(old$DataCheck(raw, map, strict)), capture(new$DataCheck(raw, map, strict)),
         paste(label, "check", strict))
  }
  for (drop in c(FALSE, TRUE)) {
    same(capture(old$DataStandard(raw, map, drop)), capture(new$DataStandard(raw, map, drop)),
         paste(label, "standard", drop))
  }
  same(raw, before, paste(label, "input unchanged"))
}
for (binary in c(FALSE, TRUE)) {
  cat("Comparing outcome:", if (binary) "binary" else "continuous", "\n")
  w <- new$make_pd_workflow(n = 80L, binary_outcome = binary)
  raw <- w$raw
  map <- w$mapping
  check_case(raw, map, "clean")
  check_case(data.table::as.data.table(raw), map, "data.table")
  check_case(w$data, attr(w$data, "pd_mapping"), "restandardize")
  check_case(raw[FALSE, ], map, "empty")
  check_case(raw[, setdiff(names(raw), "A")], map, "missing column")
  for (kind in c("character", "factor", "logical", "empty_id", "factor_id", "date_id",
                 "char_cov", "factor_cov", "matrix_cov", "list_cov", "date_cov", "names_cov")) {
    x <- raw
    if (kind %in% c("character", "factor")) {
      cols <- c("time", "A", "S", "Y")
      x[cols] <- lapply(x[cols], if (kind == "factor") factor else as.character)
    } else if (kind == "logical") {
      x$A <- as.logical(x$A); x$S <- as.logical(x$S)
    } else if (kind == "empty_id") x$id <- ifelse(x$id == 1, "", as.character(x$id))
    else if (kind == "factor_id") x$id <- factor(x$id, levels = rev(unique(x$id)))
    else if (kind == "date_id") x$id <- as.Date(x$id, origin = "2000-01-01")
    else if (kind == "char_cov") x$X1 <- as.character(x$X1)
    else if (kind == "factor_cov") x$X1 <- factor(x$X1)
    else if (kind == "matrix_cov") x$X1 <- I(cbind(x$X1, x$X1))
    else if (kind == "list_cov") x$X1 <- as.list(x$X1)
    else if (kind == "date_cov") x$X1 <- as.Date(x$X1, origin = "2000-01-01")
    else if (kind == "names_cov") names(x$X1) <- as.character(seq_len(nrow(x)))
    check_case(x, map, kind)
  }
  # Seeded malformed panels exercise error precedence, duplicate/missing visits,
  # missing-time ordering, ties, conversion failures and diagnostic ID ordering.
  for (seed in 1:150) {
    set.seed(seed)
    x <- raw[sample(nrow(raw), sample(20:nrow(raw), 1), replace = TRUE), ]
    x$id <- sample(c(as.character(1:15), NA_character_, "", "a\rb", "é"), nrow(x), TRUE)
    x$time <- sample(c(-1, 0:3, NA, NaN), nrow(x), TRUE)
    x$A <- sample(c(0L, 1L, NA_integer_), nrow(x), TRUE)
    x$S <- sample(c(0L, 1L, NA_integer_), nrow(x), TRUE)
    x$X1[sample(nrow(x), 3)] <- NA_real_
    if (seed %% 5L == 0L) x$A[1] <- 2L
    if (seed %% 7L == 0L) x$time[1] <- Inf
    if (seed %% 11L == 0L) x$Y[1] <- Inf
    check_case(x, map, paste("fuzz", binary, seed))
  }
  # Fixable attrition without fatal survival/treatment/duplicate failures.
  for (seed in 1:30) {
    set.seed(seed)
    x <- raw[-sample(nrow(raw), 8), ]
    x$X1[sample(nrow(x), 4)] <- NA_real_
    x$A[sample(nrow(x), 2)] <- NA_integer_
    x$S[sample(nrow(x), 2)] <- NA_integer_
    x$time[sample(nrow(x), 2)] <- NA_real_
    x$id[sample(nrow(x), 2)] <- NA_integer_
    check_case(x, map, paste("attrition", binary, seed))
  }
  # Use identical prepared panels with all downstream implementations unchanged.
  # Source environments differ, so compare HTE numerical/results components
  # without formula-environment/call identity artifacts from separate namespaces.
  w <- new$make_pd_workflow(n = 240L, binary_outcome = binary)
  a <- old$DataStandard(w$raw, w$mapping)
  b <- new$DataStandard(w$raw, w$mapping)
  same(a, b, "downstream input")
  for (fun in c("PSPred", "PrinPred", "OutPred", "HTEAllT", "HTESepT", "PSDiag", "PrinSDiag", "ORCI", "QR")) {
    run <- function(dat) switch(fun,
      PSPred = new$PSPred(w$ps_fo, dat, dat, attr(dat, "pd_mapping")),
      PrinPred = new$PrinPred(w$prin_fo, dat, dat, 0, attr(dat, "pd_mapping")),
      OutPred = new$OutPred(w$out_fo, dat, dat, 1, attr(dat, "pd_mapping")),
      HTEAllT = new$HTEAllT(dat, w$ps_fo, w$prin_fo, w$out_fo, B = 0, verbose = FALSE),
      HTESepT = new$HTESepT(dat, w$ps_fo, w$prin_fo, w$out_fo, target_time = c(1, 2), B = 0, verbose = FALSE),
      PSDiag = new$PSDiag(dat, w$ps_fo),
      PrinSDiag = new$PrinSDiag(dat, w$ps_fo, w$prin_fo),
      ORCI = new$ORCI(dat, w$prin_fo, a = 0),
      QR = new$QR(dat, w$prin_fo))
    set.seed(321); result_a <- capture(run(a)); rng_a <- .Random.seed
    set.seed(321); result_b <- capture(run(b)); rng_b <- .Random.seed
    # Independently constructed plots and GLM family closures have fresh
    # environments even on repeated unchanged inputs. Compare plot data and
    # closure definitions, preserving all model values and condition behavior.
    strip_plots <- function(x) {
      if (inherits(x, "ggplot")) return(x$data)
      if (is.function(x)) return(list(formals = formals(x), body = body(x)))
      if (is.list(x) && !is.data.frame(x)) {
        for (i in seq_along(x)) x[i] <- list(strip_plots(x[[i]]))
      }
      x
    }
    same(strip_plots(result_a), strip_plots(result_b), paste("downstream", binary, fun))
    same(rng_a, rng_b, paste("RNG", fun))
    if (fun %in% c("PSPred", "PrinPred", "OutPred", "HTEAllT", "HTESepT")) {
      stopifnot(!(is.list(result_a$value) && !is.null(result_a$value$error_class)))
    }
  }
}
cat("PASS:", comparisons, "exact comparisons (including input immutability and conditions).\n")
