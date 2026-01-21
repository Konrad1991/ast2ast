# system('find -name "*.o" | xargs rm')
# system('find -name "*.so" | xargs rm')
# Rcpp::compileAttributes()
install.packages(".", types = "source", repo = NULL)
tinytest::test_package("ast2ast")
# trash <- list.files("./R", full.names = TRUE) |> lapply(source)

f <- function() {
  a <- 1
  a[1] <- 2
  b <- c(1, 2, 3)
  # b[1, 2] <- 4
  a[[1]] <- 3L
  at(a, 1) <- 4L
  c <- matrix(1:12, 3, 4)
  c[1, 2] <- 101
  d[1] <- 4 # Results in runtime error: no memory is allocated
  # e <- 1
  # e[d[[1]], b[a[1]]] <- 100
}
fcpp <- ast2ast::translate(f, verbose = TRUE)
fcpp()
