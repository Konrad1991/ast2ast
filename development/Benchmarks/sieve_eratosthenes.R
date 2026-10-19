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

fcpp <- translate(sieve, debug = FALSE)

Rcpp::cppFunction("
int sieve_rcpp(int n) {
  LogicalVector composite(n);
  int count = 0;
  for (int i = 2; i <= n; i++) {
    if (!composite[i - 1]) {
      count++;
      if (i <= n / i) {
        for (int j = i * i; j <= n; j += i) composite[j - 1] = true;
      }
    }
  }
  return count;
}
")

n <- 1000000L
res <- fcpp(n)
res
#> expected: 78498
identical(res, sieve(n))
identical(res, sieve_rcpp(n))

res <- microbenchmark::microbenchmark(
  R = sieve(n), ast2ast = fcpp(n), rcpp = sieve_rcpp(n)
)
fold <- function(res) {
  temp <- aggregate(time ~ expr, res, mean)
  temp$time[[1]] / temp$time[[2]]
}
fold(res)
