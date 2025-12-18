# Results for unary calculations
N <- 60L
dim <- c(3L, 4L, 5L)

Logical_arr <- logical(N)
for (i in 0:(N - 1L)) {
  if (i %% 3 == 0) Logical_arr[i + 1] <- TRUE else Logical_arr[i + 1] <- FALSE
}

Integer_arr <- integer(N)
for (i in 0:(N - 1L)) {
  Integer_arr[i + 1] <- i
}

Double_arr <- double(N)
for (i in 0:(N - 1L)) {
  Double_arr[i + 1] <- i + 0.1
}
Dual_arr <- Double_arr
Dual_arr_dot <- rep(2.0, N)

# Combinations for binary operations
types <- data.frame(
  objects = c(
    "logical_etr", "integer_etr", "double_etr", "dual_etr",
    "Logical_arr", "Integer_arr", "Double_arr", "Dual_arr",
    "Logical_r_arr", "Integer_r_arr", "Double_r_arr", "Dual_r_arr",
    "Logical_borrowed_arr", "Integer_borrowed_arr", "Double_borrowed_arr", "Dual_borrowed_arr",
    "unary", "binary", "sub", "const_sub", "R_value_arr"
  ),
  scalar = c(
    TRUE, TRUE, TRUE, TRUE,
    rep(FALSE, 17)
  )
)
grid <- merge(types, types, by = NULL, suffixes = c("_l", "_r"))
grid <- grid[-which(grid$scalar_l & grid$scalar_r), ]
grid <- grid[, c(1, 3)]
head(grid)
values <- list(
  logical_etr = TRUE, integer_etr = 101L, double_etr = 3.14, dual_etr = 3.14,
  Logical_arr = Logical_arr, Integer_arr = Integer_arr, Double_arr = Double_arr, Dual_arr = Dual_arr,
  Logical_r_arr = Logical_arr, Integer_r_arr = Integer_arr, Double_r_arr = Double_arr, Dual_r_arr = Dual_arr,
  Logical_borrowed_arr = Logical_arr, Integer_borrowed_arr = Integer_arr, Double_borrowed_arr = Double_arr, Dual_borrowed_arr = Dual_arr,
  unary = sin(Double_arr), binary = Double_arr + Double_arr, sub = Double_arr, const_sub = Double_arr,
  R_value_arr = Double_arr
)
mul <- function(l, r, idx, string_res) {
  convert_string <- function(s) {
    if (s == "const_sub") return("subset(create_r_array(), true, true, true)")
    if (s == "R_value_arr") return("create_r_array()")
    s
  }
  l_cpp_string <- convert_string(l)
  r_cpp_string <- convert_string(r)
  val_l <- values[[l]]
  val_r <- values[[r]]
  convert_res_to_str_vec <- function(vec, idx, string = "expected") {
    vec <- paste(vec, collapse = ", ")
    sprintf("std::vector<double> %s{%s};", paste0(string, idx), vec)
  }
  result <- val_l * val_r
  result <- convert_res_to_str_vec(result, idx)

  is_dual <- function(obj) grepl("Dual", obj) || grepl("dual", obj)
  if (is_dual(l) || is_dual(r)) {
    if (is_dual(l) && is_dual(r)) { # I set dot everywhere to 2.0
      dot_l <- 2.0
      dot_r <- 2.0
    } else if (is_dual(l) && !is_dual(r)) {
      dot_l <- 2.0
      dot_r <- 0.0
    } else if (!is_dual(l) && is_dual(r)) {
      dot_l <- 0.0
      dot_r <- 2.0
    }
    result_dot <- dot_l * val_r + val_l * dot_r
    result_dot <- convert_res_to_str_vec(result_dot, idx, "expected_dot")
    string_res <- c(
      string_res,
      sprintf(
        '  %s\n  %s\n  ass<"deriv of: %s * %s">(compare_result_dual(%s * %s, %s, %s));
        ',
        result,
        result_dot,
        l_cpp_string, r_cpp_string, l_cpp_string, r_cpp_string,
        paste0("expected", idx), paste0("expected_dot", idx)
      )
    )
    return(string_res)
  }

  string_res <- c(
    string_res,
    sprintf(
      '  %s\n  ass<"%s * %s">(compare_result(%s * %s, %s));
      ',
      result,
      l_cpp_string, r_cpp_string, l_cpp_string, r_cpp_string, paste0("expected", idx)
    )
  )

  string_res
}

string_res <- ""
n <- nrow(grid)
for (i in seq_len(n)) {
  string_res <- mul(grid$objects_l[i], grid$objects_r[i], i, string_res)
}
head(string_res)

pipe <- pipe("xclip -selection clipboard", "w")
writeLines(string_res, pipe)
close(pipe)

