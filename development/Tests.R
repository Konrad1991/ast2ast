# Rcpp::compileAttributes(".")
# library(ast2ast)

if (!grepl("R", getwd())) {
  setwd("./R/")
}
files <- list.files()
files <- files[files != "Tests.R"]
files <- sapply(files, function(x) paste0(getwd(), "/", x))
trash <- lapply(files, source)

fct <- function(a) {
  for (i in 1:2) {
    print(i)
  }
  return(a)
}
f_args <- function(a) {
  a |> type(vec(double)) |> ref()
}
result <- translate(
  fct,
  f_args,
  output = "R", verbose = TRUE
)

result(1)

f_args <- function(a) {
  a |> type(borrow_vec(double)) |> ref()
}
result <- translate(
  fct,
  f_args,
  output = "XPtr"
)

.traceback()

ast2ast:::test_basestore() # Error: No memory was allocated
ast2ast:::test_arithmetic()
ast2ast:::test_comparison()
ast2ast:::test_borrow()
ast2ast:::test_cmr()
ast2ast:::test_c()
ast2ast:::test_colon() #Error: : accepts only vector of length 1
ast2ast:::test_concepts()
ast2ast:::isNa_tests()
ast2ast:::isInf_tests() # Error: object 'isInf_tests' not found
ast2ast:::length_tests()
ast2ast:::dim_tests()
ast2ast:::test_matrices()
ast2ast:::test_rep() # Error: times in rep has to be a vector of length 1
ast2ast:::test_vector_subsetting() # Error: Length does not match nrow*ncol
ast2ast:::test_arithmetic_unary()
ast2ast:::test_numeric() # Error: test invalid times argument of vector
ast2ast:::test_int() # Error: test invalid times argument of vector
ast2ast:::test_bool() # Error: test invalid times argument of vector
ast2ast:::test_vector() # Error: test vector as length argument with size > 1
