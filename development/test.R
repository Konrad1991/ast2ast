system('find -name "*.o" | xargs rm')
system('find -name "*.so" | xargs rm')
Rcpp::compileAttributes()
install.packages(".", types = "source", repo = NULL,)
tinytest::test_package("ast2ast")

rmarkdown::render("vignettes/DetailedDocumentation.Rmd")

f <- function() {
  a |> type(int)
  a <- 3.14
  return(a)
}

fcpp <- ast2ast::translate(f)
fcpp()
