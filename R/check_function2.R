# 1) 解析 R/ 下所有源文件，提取用到的函数名
files <- list.files("R", "\\.[Rr]$", full.names = TRUE)
exprs <- lapply(files, parse, keep.source = FALSE)
calls <- unique(unlist(lapply(exprs, function(e) all.names(e, functions = TRUE))))

# 2) 去掉 base / utils / graphics / grDevices 这些常见内建符号
base_like <- unique(c(ls(getNamespace("base")),
                      ls(getNamespace("utils")),
                      ls(getNamespace("graphics")),
                      ls(getNamespace("grDevices")),
                      c("if","for","while","repeat","function","{","}","<-","=")))

cand <- setdiff(calls, base_like)

# 3) 找每个符号可能来自哪些包
origin <- lapply(cand, function(f) {
  ga <- suppressWarnings(getAnywhere(f))
  pkgs <- unique(gsub("^.*?namespace:", "", ga$where))
  pkgs[pkgs != ""]
})
names(origin) <- cand

# 4) 只关心你 DESCRIPTION 里列的包 + stats
keep_pkgs <- c("data.table","dplyr","MASS","nnet","rootSolve","speedglm","splines2","VGAM","bindata","stats")
pairs <- do.call(rbind, lapply(names(origin), function(f) {
  pk <- intersect(origin[[f]], keep_pkgs)
  if (length(pk)) cbind(fun = f, pkg = pk) else NULL
}))
res <- as.data.frame(pairs, stringsAsFactors = FALSE)

# 5) 汇总报告
if (nrow(res)) {
  used <- aggregate(fun ~ pkg, res, function(x) sort(unique(x)))
  used$n_funcs <- lengths(used$fun)
  used
} else {
  message("未检测到使用上述依赖包的未限定函数。")
}
