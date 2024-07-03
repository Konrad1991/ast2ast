setwd("/home/konrad/Documents/GitHub/RProjects/ast2ast_supplement/ast2ast/.development/")
Rcpp::sourceCpp("PSO.cpp")


rosenbrock <- function(parameter) {
  value <- 0
  for (i in 1:(length(parameter) - 1)) {
    value <- value +
      100 * (parameter[i + 1] - parameter[i]^2)^2 +
      (1 - parameter[i])^2
  }
  return(value)
}


lb <- -10000
ub <- 10000
error_threshold <- 0.0000001
npop <- 400

pso(rep(lb, 3), rep(ub, 3), rosenbrock, 10000, npop, error_threshold)


rosenbrock_cpp <- function(parameter) {
  value::double <- 0
  sz::int <- length(parameter)
  for (i in 1:(sz - 1)) {
    value <- value +
      100 * (parameter[i + 1] - parameter[i]^2)^2 +
      (1 - parameter[i])^2
  }
  return(value)
}

rosenbrock_cpp <- ast2ast::translate(rosenbrock_cpp,
  output = "XPtr",
  handle_inputs = "",
  references = TRUE,
  verbose = TRUE
)

pso_xptr(rep(lb, 3), rep(ub, 3), rosenbrock_cpp, 10000, npop, error_threshold)


res <- microbenchmark::microbenchmark(
  pso(rep(lb, 3), rep(ub, 3), rosenbrock, 10000, npop, error_threshold),
  pso_xptr(rep(lb, 3), rep(ub, 3), rosenbrock_cpp, 10000, npop, error_threshold)
)
res

boxplot(res)
