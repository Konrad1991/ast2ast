system('find -name "*.o" | xargs rm')
system('find -name "*.so" | xargs rm')
Rcpp::compileAttributes()
install.packages(".", types = "source", repo = NULL)
tinytest::test_package("ast2ast")

trash <- list.files("./R", full.names = TRUE) |> lapply(source)
f <- function() {
  b <- 1L
  if (b == 2) {
    print("blub")
  }
  if (b == 2) print("bla") else print("blabla")
  for (i in 1:10) print(i)
  while(i < 10) print(i)
  repeat print("bla")
}

f <- function() print("Hello")
translate(f, getsource = TRUE) |> cat("\n")
