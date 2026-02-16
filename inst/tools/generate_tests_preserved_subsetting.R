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
  logical_etr = TRUE, integer_etr = 10L, double_etr = 3.14, dual_etr = 3.14, rev_ad_etr = 3.14,
  Logical_arr = Logical_arr, Integer_arr = Integer_arr, Double_arr = Double_arr, Dual_arr = Dual_arr, rev_ad_arr = Double_arr,

  Logical_r_arr = Logical_arr, Integer_r_arr = Integer_arr,
  Double_r_arr = Double_arr3, Dual_r_arr = Dual_arr, rev_ad_r_arr = Double_arr3,

  Logical_borrowed_arr = Logical_arr, Integer_borrowed_arr = Integer_arr,
  Double_borrowed_arr = Double_arr3, Dual_borrowed_arr = Dual_arr,

  unary = -(Double_arr2), binary = Double_arr3 + Double_arr3,
  sub = Integer_arr, const_sub = Integer_arr, R_value_arr = Integer_arr
)

# ------------------------------------------------------------------------------
# I. Generate a vector and test whether every possible type can be used as indices
# ------------------------------------------------------------------------------
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

# ------------------------------------------------------------------------------
# II.1 Check that everything can be subsetted
# ------------------------------------------------------------------------------
sub2 <- function(what, what_name, idx) {
  convert_string <- function(s) {
    if (s == "const_sub") return("subset(create_r_array(), true)")
    if (s == "R_value_arr") return("create_r_array()")
    if (s == "Double_arr") return("Double_arr3")
    if (s == "unary") return("-Double_arr2")
    if (s == "binary") return("Double_arr3 + Double_arr3")
    s
  }
  convert_res_to_str_vec <- function(vec, idx, string = "expected") {
    if (is.logical(vec)) vec <- tolower(as.character(vec))
    vec <- paste(vec, collapse = ", ")
    sprintf("std::vector<double> %s{%s};", paste0(string, idx), vec)
  }
  what_cpp_string <- convert_string(what_name)
  result <- what[TRUE]
  result <- convert_res_to_str_vec(result, idx)
  sprintf("  %s\n  compare_result(subset(%s, etr::Integer(true)), %s);",
    result, what_cpp_string, paste0("expected", idx)
  )
}
values_without_scalar <- values[6:21]
n_args <- length(values_without_scalar)
what <- lapply(seq_len(n_args), \(x) Double_arr)
string <- Map(
  function(what, what_name, idx) {
    sub2(what, what_name, idx)
  },values_without_scalar, names(values_without_scalar), seq_len(length(values_without_scalar))
)

pipe <- pipe("xclip -selection clipboard", "w")
writeLines(paste(string, collapse = "\n"), pipe)
close(pipe)

# ------------------------------------------------------------------------------
# II.2 Check that subset of subset works
# ------------------------------------------------------------------------------
sub2 <- function(arg, arg_name, what, idx) {
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
  sprintf("  %s\n  compare_result(subset(subset(Double_arr, Logical(true)), %s), %s);",
    result, arg_cpp_string, paste0("expected", idx)
  )
}

n_args <- length(values)
what <- lapply(seq_len(n_args), \(x) Double_arr)
string <- Map(
  function(arg, arg_name, what, idx) {
    sub2(arg, arg_name, what, idx)
  }, values, names(values), what, seq_len(length(values))
)

pipe <- pipe("xclip -selection clipboard", "w")
writeLines(paste(string, collapse = "\n"), pipe)
close(pipe)

# ------------------------------------------------------------------------------
# III. Check calculation of indices based on args
# ------------------------------------------------------------------------------
dim <- c(2L, 3L, 3L)
N <- prod(dim)
What <- array(integer(N), dim)
for (i in seq_len(N)) {
  What[i] <- i
}
What

idx <- function(kind, value) {
  list(kind = kind, value = value)
}

d1 <- list(
  true = idx("true", TRUE),
  logical_full = idx("logical", c(TRUE, FALSE)),
  logical_short = idx("logical", c(TRUE)),
  int_scalar = idx("int_scalar", 2L),
  int_vec = idx("int_vec", c(1L, 2L))
)
d2 <- list(
  true = idx("true", TRUE),
  logical_full = idx("logical", c(TRUE, FALSE, TRUE)),
  logical_short = idx("logical", c(TRUE, FALSE)),
  int_scalar = idx("int_scalar", 2L),
  int_vec = idx("int_vec", c(1L, 3L))
)
d3 <- list(
  true = idx("true", TRUE),
  logical_full = idx("logical", c(TRUE, TRUE, FALSE)),
  logical_short = idx("logical", c(FALSE, TRUE)),
  int_scalar = idx("int_scalar", 1L),
  int_vec = idx("int_vec", c(1L, 3L, 2L))
)

cases <- list(
  # A) Single-dimension varied
  list(d1$true, d2$true, d3$true),
  list(d1$logical_full, d2$true, d3$true),
  list(d1$logical_short, d2$true, d3$true),
  list(d1$int_scalar, d2$true, d3$true),
  list(d1$int_vec, d2$true, d3$true),

  list(d1$true, d2$logical_full, d3$true),
  list(d1$true, d2$logical_short, d3$true),
  list(d1$true, d2$int_scalar, d3$true),
  list(d1$true, d2$int_vec, d3$true),

  list(d1$true, d2$true, d3$logical_full),
  list(d1$true, d2$true, d3$logical_short),
  list(d1$true, d2$true, d3$int_scalar),
  list(d1$true, d2$true, d3$int_vec),

  # B) Other stuff
  list(d1$logical_short, d2$int_scalar, d3$true),
  list(d1$int_scalar, d2$logical_short, d3$true),
  list(d1$int_vec, d2$logical_short, d3$true),

  list(d1$logical_full, d2$int_vec, d3$true),
  list(d1$int_vec, d2$logical_full, d3$true),

  list(d1$logical_short, d2$logical_short, d3$true),
  list(d1$int_vec, d2$int_vec, d3$true),

  list(d1$logical_short, d2$true, d3$int_scalar),
  list(d1$int_vec, d2$true, d3$logical_short),

  list(d1$logical_full, d2$int_scalar, d3$int_scalar),
  list(d1$int_scalar, d2$logical_full, d3$int_vec),
  list(d1$int_vec, d2$int_vec, d3$int_vec),

  # C) Recycling stress
  list(d1$logical_short, d2$logical_short, d3$logical_short),
  list(d1$logical_short, d2$int_scalar, d3$logical_short),
  list(d1$int_scalar, d2$logical_short, d3$logical_short),

  list(d1$logical_short, d2$true, d3$logical_short),
  list(d1$true, d2$logical_short, d3$logical_short),
  list(d1$logical_short, d2$logical_full, d3$logical_short)
)

eval_case <- function(case) {
 args <- lapply(case, `[[`, "value")
  tryCatch(
    do.call(`[`, c(list(What), args, list(drop = FALSE))),
    error = function(e) e
  )
}
results <- lapply(cases, eval_case)

create_names <- function(args, idx) {
  names <- character(length(args))
  for (i in seq_len(length(args))) {
    names[i] <- sprintf("idx%s_dim%s", idx, i)
  }
  names
}
construct_vector <- function(l, vec, name) {
  if (is.logical(vec)) {
    vec <- tolower(as.character(vec))
    decl <- sprintf("Array<Logical, Buffer<Logical, LBufferTrait>> %s(SI{%s});", name, l)
    appending <- lapply(vec, function(elem, name) {
      inner_inner_idx <- parent.frame()$i[]
      sprintf("%s.set(%s, Logical(%s));", name, inner_inner_idx - 1L, tolower(elem))
    }, name)
    return(paste0(decl, "\n", paste(appending, collapse = "\n")))
  }
  decl <- sprintf("Array<Integer, Buffer<Integer, LBufferTrait>> %s(SI{%s});", name, l)
  appending <- lapply(vec, function(elem, name) {
    inner_inner_idx <- parent.frame()$i[]
    sprintf("%s.set(%s, Integer(%s));", name, inner_inner_idx - 1L, tolower(elem))
  }, name)
  return(paste0(decl, "\n", paste(appending, collapse = "\n")))
}
construct_subset <- function(idx, names) {
  sprintf("compare_result_ints(subset(What, %s, %s, %s), expected%s, expected_dim%s);",
    names[[1]], names[[2]], names[[3]], idx, idx
  )
}
construct_expected <- function(idx, result) {
  sprintf("std::vector<int> expected%s{%s};\nstd::vector<std::size_t> expected_dim%s{%s};",
    idx, paste0(result, collapse = ", "), idx, paste(dim(result), collapse = ", ")
  )
}
convert_to_string <- function(vec, idx, name) {
  if (length(vec) == 1) {
    if (is.logical(vec)) {
       return(sprintf("Logical %s(%s);", name, tolower(vec)))
    } else {
       return(sprintf("Integer %s(%s);", name, vec))
    }
  }
  l <- length(vec)
  construct_vector(l, vec, name)
}

cpp_emit <- function(case, results) {
  idx <- parent.frame()$i[]
  current_res <- results[[idx]]
  args <- lapply(case, `[[`, "value")
  names <- create_names(args, idx)
  res <- list()
  for (i in seq_len(length(args))) {
    res[[length(res) + 1L]] <- convert_to_string(args[[i]], i, names[[i]])
  }
  res[[length(res) + 1L]] <- construct_expected(idx, current_res)
  res[[length(res) + 1L]] <- construct_subset(idx, names)
  paste0(res, collapse = "\n")
}
string_res <- lapply(cases, cpp_emit, results)

pipe <- pipe("xclip -selection clipboard", "w")
writeLines(paste(string_res, collapse = "\n"), pipe)
close(pipe)

# ------------------------------------------------------------------------------
# III. Check calculation of indices + assignment
# ------------------------------------------------------------------------------
dim <- c(2L, 3L, 3L)
N <- prod(dim)
What <- array(integer(N), dim)
for (i in seq_len(N)) {
  What[i] <- i
}
What

idx <- function(kind, value) {
  list(kind = kind, value = value)
}

d1 <- list(
  true = idx("true", TRUE),
  logical_full = idx("logical", c(TRUE, FALSE)),
  logical_short = idx("logical", c(TRUE)),
  int_scalar = idx("int_scalar", 2L),
  int_vec = idx("int_vec", c(1L, 2L))
)
d2 <- list(
  true = idx("true", TRUE),
  logical_full = idx("logical", c(TRUE, FALSE, TRUE)),
  logical_short = idx("logical", c(TRUE, FALSE)),
  int_scalar = idx("int_scalar", 2L),
  int_vec = idx("int_vec", c(1L, 3L))
)
d3 <- list(
  true = idx("true", TRUE),
  logical_full = idx("logical", c(TRUE, TRUE, FALSE)),
  logical_short = idx("logical", c(FALSE, TRUE)),
  int_scalar = idx("int_scalar", 1L),
  int_vec = idx("int_vec", c(1L, 3L, 2L))
)

cases <- list(
  # A) Single-dimension varied
  list(d1$true, d2$true, d3$true),
  list(d1$logical_full, d2$true, d3$true),
  list(d1$logical_short, d2$true, d3$true),
  list(d1$int_scalar, d2$true, d3$true),
  list(d1$int_vec, d2$true, d3$true),

  list(d1$true, d2$logical_full, d3$true),
  list(d1$true, d2$logical_short, d3$true),
  list(d1$true, d2$int_scalar, d3$true),
  list(d1$true, d2$int_vec, d3$true),

  list(d1$true, d2$true, d3$logical_full),
  list(d1$true, d2$true, d3$logical_short),
  list(d1$true, d2$true, d3$int_scalar),
  list(d1$true, d2$true, d3$int_vec),

  # B) Other stuff
  list(d1$logical_short, d2$int_scalar, d3$true),
  list(d1$int_scalar, d2$logical_short, d3$true),
  list(d1$int_vec, d2$logical_short, d3$true),

  list(d1$logical_full, d2$int_vec, d3$true),
  list(d1$int_vec, d2$logical_full, d3$true),

  list(d1$logical_short, d2$logical_short, d3$true),
  list(d1$int_vec, d2$int_vec, d3$true),

  list(d1$logical_short, d2$true, d3$int_scalar),
  list(d1$int_vec, d2$true, d3$logical_short),

  list(d1$logical_full, d2$int_scalar, d3$int_scalar),
  list(d1$int_scalar, d2$logical_full, d3$int_vec),
  list(d1$int_vec, d2$int_vec, d3$int_vec),

  # C) Recycling stress
  list(d1$logical_short, d2$logical_short, d3$logical_short),
  list(d1$logical_short, d2$int_scalar, d3$logical_short),
  list(d1$int_scalar, d2$logical_short, d3$logical_short),

  list(d1$logical_short, d2$true, d3$logical_short),
  list(d1$true, d2$logical_short, d3$logical_short),
  list(d1$logical_short, d2$logical_full, d3$logical_short)
)

eval_case <- function(case) {
 args <- lapply(case, `[[`, "value")
  tryCatch(
    do.call(`[`, c(list(What), args, list(drop = FALSE))),
    error = function(e) e
  )
}
results <- lapply(cases, eval_case)

create_names <- function(args, idx) {
  names <- character(length(args))
  for (i in seq_len(length(args))) {
    names[i] <- sprintf("idx%s_dim%s", idx, i)
  }
  names
}
construct_vector <- function(l, vec, name) {
  if (is.logical(vec)) {
    vec <- tolower(as.character(vec))
    decl <- sprintf("Array<Logical, Buffer<Logical, LBufferTrait>> %s(SI{%s});", name, l)
    appending <- lapply(vec, function(elem, name) {
      inner_inner_idx <- parent.frame()$i[]
      sprintf("%s.set(%s, Logical(%s));", name, inner_inner_idx - 1L, tolower(elem))
    }, name)
    return(paste0(decl, "\n", paste(appending, collapse = "\n")))
  }
  decl <- sprintf("Array<Integer, Buffer<Integer, LBufferTrait>> %s(SI{%s});", name, l)
  appending <- lapply(vec, function(elem, name) {
    inner_inner_idx <- parent.frame()$i[]
    sprintf("%s.set(%s, Integer(%s));", name, inner_inner_idx - 1L, tolower(elem))
  }, name)
  return(paste0(decl, "\n", paste(appending, collapse = "\n")))
}
construct_subset <- function(idx, names) {
  sprintf("subset(What, %s, %s, %s) = subset(What, %s, %s, %s) + Integer(1000);\n
    compare_result_ints(subset(What, %s, %s, %s), expected%s, expected_dim%s);\n
    reset(What);",
    names[[1]], names[[2]], names[[3]], names[[1]], names[[2]], names[[3]],
    names[[1]], names[[2]], names[[3]],
    idx, idx
  )
}
construct_expected <- function(idx, result) {
  sprintf("std::vector<int> expected%s{%s};\nstd::vector<std::size_t> expected_dim%s{%s};",
    idx, paste0(result + 1000, collapse = ", "), idx, paste(dim(result), collapse = ", ")
  )
}
convert_to_string <- function(vec, idx, name) {
  if (length(vec) == 1) {
    if (is.logical(vec)) {
       return(sprintf("Logical %s(%s);", name, tolower(vec)))
    } else {
       return(sprintf("Integer %s(%s);", name, vec))
    }
  }
  l <- length(vec)
  construct_vector(l, vec, name)
}

cpp_emit <- function(case, results) {
  idx <- parent.frame()$i[]
  current_res <- results[[idx]]
  args <- lapply(case, `[[`, "value")
  names <- create_names(args, idx)
  res <- list()
  for (i in seq_len(length(args))) {
    res[[length(res) + 1L]] <- convert_to_string(args[[i]], i, names[[i]])
  }
  res[[length(res) + 1L]] <- construct_expected(idx, current_res)
  res[[length(res) + 1L]] <- construct_subset(idx, names)
  paste0(res, collapse = "\n")
}
string_res <- lapply(cases, cpp_emit, results)

pipe <- pipe("xclip -selection clipboard", "w")
writeLines(paste(string_res, collapse = "\n"), pipe)
close(pipe)
