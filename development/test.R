system('find -name "*.o" | xargs rm')
system('find -name "*.so" | xargs rm')
Rcpp::compileAttributes()
install.packages(".", types = "source", repo = NULL)
tinytest::test_package("ast2ast")

trash <- list.files("./R", full.names = TRUE) |> lapply(source)
f <- function() {
  a <- 1L
  l <- length(a)
  d <- dim(a)
}
cat(translate(f, getsource = TRUE), "\n")
