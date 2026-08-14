system('find -name "*.o" | xargs rm')
system('find -name "*.so" | xargs rm')
Rcpp::compileAttributes()
install.packages(".", types = "source", repo = NULL)
tinytest::test_package("ast2ast")

files <- list.files("./R", full.names = TRUE)
invisible(lapply(files, source))
types_f_point <- function() {
  new_type(
    Point,
    slots(
      x |> type(double),
      y |> type(double)
    )
  )
}
f <- function() {
  p |> type(Point)
  g <- fn(
    f_args = function(a, b) {
      a |> type(double)
      b |> type(double)
    },
    return_value = type(int),
    block = function(a, b) {
      c <- as.integer(a + b)
      return(c)
    }
  )
  c <- vector("Point", 5L)
  a <- c(3.14, dfg)
}
fcpp <- translate(
  f,
  types_f = types_f_point
)
