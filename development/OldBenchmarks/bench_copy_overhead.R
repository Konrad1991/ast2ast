# Isolates R-buffer-copy overhead from computation.
#
# Same trivial sum-loop, three variants:
#   ast2ast_value  - type(vec(double))         copies the R vector into ast2ast's
#                    own buffer on entry (value semantics)
#   ast2ast_borrow - type(borrow_vec(double))   operates directly on R's REAL()
#                    buffer, no copy
#   quickr         - declare(type(x = double(NA)))
#
# ast2ast_value vs ast2ast_borrow isolates exactly the "copy R's buffer into our
# own array" cost Konrad flagged after seeing quickr/Fortran handle this fast.

library(ast2ast)
library(quickr)
library(microbenchmark)

sumloop <- function(x) {
  n <- length(x)
  s <- 0.0
  for (i in seq_len(n)) {
    s <- s + x[i]
  }
  return(s)
}

# keep sumloop as the pure-R baseline; inject the argtypes() prologue per variant
add_args <- function(f, arg_block) {
  body(f) <- as.call(c(as.name("{"), arg_block, as.list(body(f))[-1]))
  f
}
sumloop_value <- ast2ast::translate(add_args(sumloop, quote(argtypes(x |> type(vec(double))))))
sumloop_borrow <- ast2ast::translate(add_args(sumloop, quote(argtypes(x |> type(borrow_vec(double))))))

sumloop_quickr <- quickr::quick(function(x) {
  declare(type(x = double(NA)))
  n <- length(x)
  s <- 0.0
  for (i in seq_len(n)) {
    s <- s + x[i]
  }
  s
})

set.seed(1234)

bench_one <- function(N) {
  x <- rnorm(N)
  ref <- sumloop(x)
  stopifnot(all.equal(ref, sumloop_value(x)))
  stopifnot(all.equal(ref, sumloop_borrow(x)))
  stopifnot(all.equal(ref, sumloop_quickr(x)))
  microbenchmark(
    R              = sumloop(x),
    ast2ast_value  = sumloop_value(x),
    ast2ast_borrow = sumloop_borrow(x),
    quickr         = sumloop_quickr(x),
    times = 30
  )
}

for (N in c(1000, 100000, 5000000, 50000000)) {
  cat(sprintf("\n--- N = %d ---\n", N))
  print(bench_one(N))
}
