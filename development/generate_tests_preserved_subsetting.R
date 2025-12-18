# for preserved subsettings
# I. Generate a vector and test whether every possible type can be used as indices
N <- 60L
dim <- N
# Double_arr will be subsetted
Double_arr <- array(double(N), dim)
for (i in 0:(N - 1L)) {
  Double_arr[i + 1] <- i + 0.1
}
# Double_arr will be subsetted with one of the following objects
Logical_arr <- array(logical(N), dim)
for (i in 0:(N - 1L)) {
  if (i %% 3 == 0) Logical_arr[i + 1] <- TRUE else Logical_arr[i + 1] <- FALSE
}
Integer_arr <- array(c(1L, 10L, 60L), 3L)
Dual_arr <- array(c(1.1, 1.0, 10.0, 59.9, 60.0), 5L)
Dual_arr_dot <- array(rep(0.0, 5L), 5L) # Irrelevant for subsetting

Double_arr2 <- array(c(-1.0, -2.0), 2L)
Double_arr3 <- array(c(1.0, 2.0, 3.0), 3L)

# Combinations for binary operations
values <- list(
  logical_etr = TRUE, integer_etr = 10L, double_etr = 3.14, dual_etr = 3.14,
  Logical_arr = Logical_arr, Integer_arr = Integer_arr, Double_arr = Double_arr, Dual_arr = Dual_arr,

  Logical_r_arr = Logical_arr, Integer_r_arr = Integer_arr,
  Double_r_arr = Double_arr3, Dual_r_arr = Dual_arr,

  Logical_borrowed_arr = Logical_arr, Integer_borrowed_arr = Integer_arr,
  Double_borrowed_arr = Double_arr3, Dual_borrowed_arr = Dual_arr,

  unary = -(Double_arr2), binary = Double_arr3 + Double_arr3,
  sub = Integer_arr, const_sub = Integer_arr, R_value_arr = Integer_arr
)

# I.1 Subset an L value
sub1 <- function(arg, arg_name, what, idx) {
  convert_string <- function(s) {
    if (s == "const_sub") return("subset(create_r_array(), true)")
    if (s == "R_value_arr") return("create_r_array()")
    if (s == "Double_arr") return("Double_arr3")
    s
  }
  convert_res_to_str_vec <- function(vec, idx, string = "expected") {
    vec <- paste(vec, collapse = ", ")
    sprintf("std::vector<double> %s{%s};", paste0(string, idx), vec)
  }
  arg_cpp_string <- convert_string(arg_name)
  result <- what[arg]
  result <- convert_res_to_str_vec(result, idx)
  sprintf("  %s\n  compare_result(subset(Double_arr, %s), %s);",
    result, arg_cpp_string, paste0("expected", idx)
  )
}

n_args <- length(values)
what <- lapply(seq_len(n_args), \(x) Double_arr)
string <- Map(
  function(arg, arg_name, what, idx) {
    sub1(arg, arg_name, what, idx)
  }, values, names(values), what, seq_len(length(values))
)

pipe <- pipe("xclip -selection clipboard", "w")
writeLines(paste(string, collapse = "\n"), pipe)
close(pipe)

# I.2 Subset a subset
sub1 <- function(arg, arg_name, what, idx) {
  convert_string <- function(s) {
    if (s == "const_sub") return("subset(create_r_array(), true)")
    if (s == "R_value_arr") return("create_r_array()")
    if (s == "Double_arr") return("Double_arr3")
    s
  }
  convert_res_to_str_vec <- function(vec, idx, string = "expected") {
    vec <- paste(vec, collapse = ", ")
    sprintf("std::vector<double> %s{%s};", paste0(string, idx), vec)
  }
  arg_cpp_string <- convert_string(arg_name)
  result <- what[TRUE][arg]
  result <- convert_res_to_str_vec(result, idx)
  sprintf("  %s\n  compare_result(subset(subset(Double_arr, true), %s), %s);",
    result, arg_cpp_string, paste0("expected", idx)
  )
}

n_args <- length(values)
what <- lapply(seq_len(n_args), \(x) Double_arr)
string <- Map(
  function(arg, arg_name, what, idx) {
    sub1(arg, arg_name, what, idx)
  }, values, names(values), what, seq_len(length(values))
)

pipe <- pipe("xclip -selection clipboard", "w")
writeLines(paste(string, collapse = "\n"), pipe)
close(pipe)
