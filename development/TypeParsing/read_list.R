Rcpp::sourceCpp("./development/TypeParsing/read_list.cpp")
# Construction in R
ct <- structure(
  list(a = 1L, b = 3.1415, c = c(1L, 2L, 3L)),
  class = "cool_type"
)
inspect_list(ct)
