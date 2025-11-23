# 扫描 R/ 源码中出现的函数调用名
files <- list.files("R", "\\.[Rr]$", full.names = TRUE)
exprs <- lapply(files, parse, keep.source = FALSE)
calls <- unique(unlist(lapply(exprs, function(e) all.names(e, functions = TRUE))))

# 过滤常见内建命名空间
base_like <- unique(c(ls(getNamespace("base")),
                      ls(getNamespace("stats")),
                      ls(getNamespace("utils")),
                      ls(getNamespace("graphics")),
                      ls(getNamespace("grDevices")),
                      c("if","for","while","repeat","function","{","}","<-","=")))
cand <- setdiff(calls, base_like)

# 映射每个函数可能来自哪个包
origin <- lapply(cand, function(f) {
  ga <- suppressWarnings(getAnywhere(f))
  pkgs <- unique(gsub("^.*?namespace:", "", ga$where))
  pkgs[pkgs != ""]
})
names(origin) <- cand

# 只保留你关心的依赖集合
keep_pkgs <- c("data.table","dplyr","MASS","nnet","rootSolve","speedglm","splines2","VGAM","stats")
origin <- origin[sapply(origin, function(pk) any(pk %in% keep_pkgs))]

# 选择优先包（有重名时优先用这几个）
prefer <- c("data.table","dplyr","stats","MASS","speedglm","splines2","VGAM","nnet","rootSolve")
pick_pkg <- function(pk) pk[match(TRUE, pk %in% prefer, nomatch = 1)]

pairs <- data.frame(fun = names(origin),
                    pkg = sapply(origin, pick_pkg),
                    stringsAsFactors = FALSE)

# 汇总为 roxygen 的 @importFrom 片段
by_pkg <- split(pairs$fun, pairs$pkg)
cat("# 自动建议的 @importFrom：\n")
for (p in names(by_pkg)) {
  cat(sprintf("#' @importFrom %s %s\n", p, paste(sort(unique(by_pkg[[p]])), collapse = " ")))
}
