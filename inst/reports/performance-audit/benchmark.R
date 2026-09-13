# Run from the package root: Rscript inst/reports/performance-audit/benchmark.R old 1000 3000 10000
# Optional: PD_AUDIT_PROFILE=1 for allocation and sampled CPU profiles.
args <- commandArgs(TRUE)
version <- args[1L]
sizes <- as.integer(args[-1L])
root <- "inst/reports/performance-audit"
env <- new.env(parent = globalenv())
for (file in list.files("R", "[.]R$", full.names = TRUE)) sys.source(file, env, keep.source = TRUE)
if (version == "old") {
  for (file in list.files(file.path(root, "baseline"), "[.]R$", full.names = TRUE)) {
    if (basename(file) != "generate_example_data.R") sys.source(file, env, keep.source = TRUE)
  }
}
# Evaluate definitions only: never execute the generator script's rebuild/install calls.
generator_env <- new.env(parent = globalenv())
definitions <- c("generate_data_example", "cov_mtx", "rho_mtx", "a_coef", "S_con_coef", "Y_coef")
for (expr in parse(file.path(root, "baseline/generate_example_data.R"))) {
  if (is.call(expr) && identical(expr[[1L]], as.name("<-")) &&
      as.character(expr[[2L]])[1L] %in% definitions) eval(expr, generator_env)
}
data.table::setDTthreads(1L)
map <- env$Mapping("id", "time", "A", "S", "Y", 0, 2, paste0("X", 1:6), paste0("X", 1:6), "C")
profile <- identical(Sys.getenv("PD_AUDIT_PROFILE"), "1")
allocation_threshold <- as.numeric(Sys.getenv("PD_AUDIT_ALLOCATION_MIN_BYTES", "1000"))
results <- list()
for (n in sizes) {
  raw <- with(generator_env, generate_data_example(n, 3L, cov_mtx, rho_mtx, a_coef,
    S_con_coef, Y_coef, seed = 20260912L))$dat
  for (fun in c("Mapping", "DataCheck", "DataStandard")) {
    run <- switch(fun,
      Mapping = function() env$Mapping("id", "time", "A", "S", "Y", 0, 2, paste0("X", 1:6), paste0("X", 1:6), "C"),
      DataCheck = function() env$DataCheck(raw, map),
      DataStandard = function() env$DataStandard(raw, map, drop = TRUE))
    # Warm compilation before profiling, outside both CPU/allocation recording.
    if (profile) {
      for (warmup in seq_len(3L)) invisible(run())
      gc()
    }
    for (replicate in seq_len(if (profile) 1L else 3L)) {
      before <- gc(reset = TRUE)
      prefix <- file.path(root, paste(version, n, fun, sep = "-"))
      if (profile) {
        Rprof(paste0(prefix, ".Rprof"), interval = 0.01, memory.profiling = TRUE,
              line.profiling = TRUE)
        Rprofmem(paste0(prefix, ".mem"), threshold = allocation_threshold)
      }
      elapsed <- system.time(value <- run())[["elapsed"]]
      if (profile) { Rprof(NULL); Rprofmem(NULL) }
      after <- gc()
      row <- data.frame(version, n, rows = nrow(raw), fun, replicate, elapsed,
        input_MiB = as.numeric(object.size(raw)) / 2^20,
        output_MiB = as.numeric(object.size(value)) / 2^20,
        heap_highwater_MiB = sum(after[, ncol(after)]),
        heap_increment_MiB = sum(after[, ncol(after)]) - sum(before[, 2L]))
      results[[length(results) + 1L]] <- row
      print(row, row.names = FALSE)
      rm(value)
      write.csv(do.call(rbind, results), file.path(root,
        paste0(version, if (profile) "-profile" else "-benchmark", "-", paste(sizes, collapse = "_"), ".csv")), row.names = FALSE)
    }
  }
  rm(raw)
}
writeLines(capture.output(sessionInfo()), file.path(root, "sessionInfo.txt"))
