f <- function(c_db, r, t_dvp, z) {
  a_db <- 1.2
  a_it <- 2
  a_lg <- TRUE
  a_dbs <- 1.2
  a_its <- 2
  a_lgs <- TRUE
  r <- 5
  print(a_db)
  r <- c(1, 2, 3, 4)
  print(r)
}

fcpp <- ast2ast::translate(f,
  verbose = TRUE, reference = TRUE,
  output = "XPtr", return_type = "void"
)

Rcpp::sourceCpp("test.cpp", verbose = TRUE)
test(fcpp)
