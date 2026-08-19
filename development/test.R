fystem('find -name "*.o" | xargs rm')
system('find -name "*.so" | xargs rm')
Rcpp::compileAttributes()
install.packages(".", types = "source", repo = NULL)
tinytest::test_package("ast2ast")

files <- list.files("./R", full.names = TRUE)
invisible(lapply(files, source))

types <- function() {
  new_type(
    Node,
    slots(
      x |> type(double),
      y |> type(vec(int))
    )
  )
}
f <- function() {
  n |> type(Node)
  print(n)
}
fcpp <- translate(f, types_f = types, verbose = TRUE)
fcpp()

