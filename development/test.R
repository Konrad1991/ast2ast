system('find -name "*.o" | xargs rm')
system('find -name "*.so" | xargs rm')
Rcpp::compileAttributes()
install.packages(".", types = "source", repo = NULL)
tinytest::test_package("ast2ast")

trash <- list.files("./R", full.names = TRUE) |> lapply(source)
f <- function(a) {
  for (i in seq_len(length(a))) {
    b <- i
    print(b)
  }
  for (j in a) {
    print(j)
  }
}
fcpp <- translate(f, getsource = TRUE)
cat(fcpp)

fcpp <- translate(f)
fcpp(runif(10))
