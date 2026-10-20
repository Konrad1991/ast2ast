# Runs every benchmark in this folder; reports how much faster ast2ast is than R.
# Each script has to store its microbenchmark result in `res` with the
# expressions named R / ast2ast (/ rcpp), or as pairs <name>_r / <name>_a2a.
# Run from the package root: source("development/Benchmarks/run_all.R")
library(ast2ast)

compiler::enableJIT(3)
bench_dir <- "development/Benchmarks"
files <- setdiff(list.files(bench_dir, pattern = "\\.R$"), "run_all.R")

speedups <- function(res) {
  t <- tapply(res$time, res$expr, stats::median)
  out <- list()
  if (all(c("R", "ast2ast") %in% names(t))) {
    rcpp <- if ("rcpp" %in% names(t)) t[["ast2ast"]] / t[["rcpp"]] else NA_real_
    out[[1]] <- data.frame(case = "", r_ms = t[["R"]] / 1e6, a2a_ms = t[["ast2ast"]] / 1e6,
                           speedup = t[["R"]] / t[["ast2ast"]], vs_rcpp = rcpp)
  }
  for (r_name in grep("_r$", names(t), value = TRUE)) {
    a_name <- sub("_r$", "_a2a", r_name)
    c_name <- sub("_r$", "_rcpp", r_name)
    if (!(a_name %in% names(t))) next
    rcpp <- if (c_name %in% names(t)) t[[a_name]] / t[[c_name]] else NA_real_
    out[[length(out) + 1]] <- data.frame(case = sub("_r$", "", r_name), r_ms = t[[r_name]] / 1e6,
                                         a2a_ms = t[[a_name]] / 1e6,
                                         speedup = t[[r_name]] / t[[a_name]], vs_rcpp = rcpp)
  }
  do.call(rbind, out)
}

rows <- lapply(files, function(f) {
  cat("running", f, "\n")
  env <- new.env()
  utils::capture.output(sys.source(file.path(bench_dir, f), envir = env))
  data.frame(benchmark = sub("\\.R$", "", f), speedups(env$res))
})
result <- do.call(rbind, rows)
result

commit <- system("git rev-parse --short HEAD", intern = TRUE)
dirty <- length(system("git status --porcelain -- R inst/include src", intern = TRUE)) > 0
result <- data.frame(date = format(Sys.time(), "%Y-%m-%d %H:%M"),
                     commit = paste0(commit, if (dirty) "+dirty" else ""), result)

print(result[, c("benchmark", "case", "r_ms", "a2a_ms", "speedup", "vs_rcpp")], digits = 3, row.names = FALSE)

csv <- file.path(bench_dir, "results.csv")
utils::write.table(result, csv, sep = ",", row.names = FALSE,
                   col.names = !file.exists(csv), append = file.exists(csv))
