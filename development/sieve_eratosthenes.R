library(ast2ast)
argtypes <- function(...) {}
returntype <- function(ReturnValue) {}

sieve <- function(n) {
  argtypes(
    n |> type(int)
  )
  composite <- logical(n)
  count <- 0L

  i <- 2L
  while (i <= n) {
    if (!composite[[i]]) {
      count <- count + 1L
      if (i <= n %/% i) {
        j <- i * i
        while (j <= n) {
          composite[[j]] <- TRUE
          j <- j + i
        }
      }
    }
    i <- i + 1L
  }

  return(count)
}

fcpp <- translate(sieve)

n <- 1000000L
res <- fcpp(n)
res
#> expected: 78498
identical(res, sieve(n))

microbenchmark::microbenchmark(
  sieve(n), fcpp(n)
)
