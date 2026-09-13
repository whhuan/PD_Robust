root <- "inst/reports/performance-audit"
old <- read.csv(file.path(root, "old-benchmark-1000_3000_10000_30000.csv"))
new <- read.csv(file.path(root, "new-benchmark-1000_3000_10000_30000_100000.csv"))
summary <- aggregate(cbind(elapsed, heap_highwater_MiB, heap_increment_MiB,
                           input_MiB, output_MiB) ~ version + n + rows + fun,
                     rbind(old, new), median)
write.csv(summary, file.path(root, "benchmark-summary.csv"), row.names = FALSE)
allocations <- profiles <- list()
for (n in c(3000, 10000)) for (v in c("old", "new")) {
  for (f in c("Mapping", "DataCheck", "DataStandard")) {
    prefix <- file.path(root, paste(v, n, f, sep = "-"))
    lines <- readLines(paste0(prefix, ".mem"))
    bytes <- suppressWarnings(as.numeric(sub(" .*", "", lines)))
    allocations[[length(allocations) + 1L]] <- data.frame(
      version = v, n, fun = f, minimum_bytes = if (n == 3000) 0 else 1000,
      allocated_MiB = sum(bytes, na.rm = TRUE) / 2^20,
      allocations = sum(!is.na(bytes)), new_page_events = sum(grepl("^new page", lines)))
    profile <- tryCatch(summaryRprof(paste0(prefix, ".Rprof"), lines = "show"),
                        error = function(e) list(note = conditionMessage(e)))
    profiles[[paste(v, n, f)]] <- list(self = head(profile$by.self, 12),
      total = head(profile$by.total, 15), note = profile$note)
  }
}
write.csv(do.call(rbind, allocations), file.path(root, "allocation-summary.csv"), row.names = FALSE)
writeLines(capture.output(profiles), file.path(root, "cpu-profile-summary.txt"))
print(summary)
print(do.call(rbind, allocations))
