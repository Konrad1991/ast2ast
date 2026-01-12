# system('find -name "*.o" | xargs rm')
# system('find -name "*.so" | xargs rm')
# install.packages(".", types = "source", repo = NULL,)
# tinytest::test_package("ast2ast")

f1 <- function(a) {
  size <- length(a)
  for (i in 1L:size) {
    for (j in 1L:(size - 1L)) {
      if (a[j] > a[j + 1L]) {
        temp <- a[j]
        a[j] <- a[j + 1L]
        a[j + 1L] <- temp
      }
    }
  }
  return(a)
}

f2 <- function(a) {
  size <- length(a)
  for (i in 1:size) {
    for (j in 1:(size - 1)) {
      if (a[[j]] > a[[j + 1]]) {
        temp <- a[[j]]
        a[[j]] <- a[[j + 1]]
        a[[j + 1]] <- temp
      }
    }
  }
  return(a)
}

fcpp <- ast2ast::translate(f1, verbose = TRUE)
fcpp2 <- ast2ast::translate(f2)
a <- runif(100)
microbenchmark::microbenchmark(
  f1(a), f2(a), fcpp(a), fcpp2(a)
)
