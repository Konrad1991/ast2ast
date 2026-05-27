out_dir <- "development/Coverage"
cov <- covr::package_coverage(path = ".", type = "tests", quiet = FALSE)
cat(sprintf("\nOverall line coverage: %.1f%%\n", covr::percent_coverage(cov)))

## ---- per-file summary, worst-covered first ---------------------------------
tc <- covr::tally_coverage(cov) # one row per measurable line
files <- unique(tc$filename)
rows <- lapply(files, function(f) {
  v <- tc$value[tc$filename == f]
  data.frame(
    pct = round(100 * sum(v > 0) / length(v), 1),
    covered = sum(v > 0),
    lines = length(v),
    file = f,
    stringsAsFactors = FALSE
  )
})
summ <- do.call(rbind, rows)
summ <- summ[order(summ$pct, summ$file), ]
summ
summary_path <- file.path(out_dir, "coverage_summary.txt")
con <- file(summary_path, "w")
writeLines(sprintf("Overall line coverage: %.1f%%\n", covr::percent_coverage(cov)), con)
writeLines(sprintf("%6s  %8s  %s", "pct", "cov/tot", "file"), con)
writeLines(
  apply(summ, 1, function(r)
    sprintf("%5s%%  %4s/%-4s  %s",
      trimws(r["pct"]), trimws(r["covered"]), trimws(r["lines"]),
      trimws(r["file"]))
  ), con)
close(con)

## ---- library-only coverage + headers with no coverage data -----------------
lib <- tc[grepl("inst/include", tc$filename, fixed = TRUE), ]
lib_pct <- round(100 * sum(lib$value > 0) / nrow(lib), 1)
cat(sprintf("Library-only (inst/include) line coverage: %.1f%%\n", lib_pct))
con <- file(summary_path, "a")
writeLines(sprintf("\nLibrary-only (inst/include) line coverage: %.1f%%", lib_pct), con)
close(con)

# Headers on disk that produced NO coverage data: never compiled (#include'd out),
# include-only umbrella files, or templates no test ever instantiates --
# the last group is a real but otherwise invisible gap.
all_hpp <- normalizePath(
  list.files("inst/include", pattern = "\\.hpp$", recursive = TRUE, full.names = TRUE))
seen_hpp <- normalizePath(unique(tc$filename[grepl("\\.hpp$", tc$filename)]), mustWork = FALSE)
missing_hpp <- sort(setdiff(all_hpp, seen_hpp))
missing_path <- file.path(out_dir, "coverage_missing.txt")
con <- file(missing_path, "w")
writeLines("Headers with NO coverage data (uncompiled, include-only, or never instantiated):\n", con)
writeLines(sub(".*/(etr_bits/.*|etr\\.hpp)$", "\\1", missing_hpp), con)
close(con)

## ---- never-executed lines in the etr_bits headers --------------------------
zc <- covr::zero_coverage(cov)
zc <- zc[grepl("etr_bits", zc$filename, fixed = TRUE), ]
zero_path <- file.path(out_dir, "coverage_zero.txt")
con <- file(zero_path, "w")
writeLines("Never-executed lines in inst/include/etr_bits (file:line):\n", con)
if (nrow(zc)) {
  short <- sub(".*/(etr_bits/.*)$", "\\1", zc$filename)
  writeLines(sprintf("%s:%d", short, zc$line), con)
} else {
  writeLines("(none)", con)
}
close(con)

## ---- browsable HTML report -------------------------------------------------
report_path <- file.path(out_dir, "coverage_report.html")
covr::report(cov, file = report_path, browse = FALSE)
