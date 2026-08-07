system('find -name "*.o" | xargs rm')
system('find -name "*.so" | xargs rm')
Rcpp::compileAttributes()
install.packages(".", types = "source", repo = NULL)
tinytest::test_package("ast2ast")

f <- function() {
  y <- TRUE
  x <- y
  y <- x + 1L
  x <- 3.145
}
fcpp <- ast2ast::translate(f, getsource = TRUE)
cat(fcpp, "\n")


f <- function() {
  x <- c(1.1, 2.2)
  g <- fn(
    args_f = function(v) v |> type(vec(double)) |> ref() |> const(),
    return_value = type(double),
    block = function(v) {
      return(v[1])
    }
  )
  y <- g(x)
}
fcpp <- ast2ast::translate(f, getsource = TRUE)
cat(fcpp, "\n")
