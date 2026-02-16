system('find -name "*.o" | xargs rm')
system('find -name "*.so" | xargs rm')
Rcpp::compileAttributes()
install.packages(".", types = "source", repo = NULL)
tinytest::test_package("ast2ast")

trash <- list.files("./R", full.names = TRUE) |> lapply(source)
f <- function() {
  for (i in seq_len(10)) {
    print(i)
  }
  for (i in seq_along(1:3)) {
    print(i)
  }
}
translate(f, getsource = TRUE) |> cat("\n")
fcpp <- ast2ast::translate(f, verbose = TRUE)
fcpp()
