library(tinytest)

# --- helpers ---------------------------------------------------------------
run_fr_checks <- function(fct, args_fct, r_fct = TRUE, known_types) {
  real_type <- "etr::Double"
  env <- new.env(parent = emptyenv())
  env$r_fct <- r_fct
  env$real_type <- real_type
  env$known_types <- known_types
  AST <- ast2ast:::parse_body(body(fct), env, ast2ast:::function_registry_global)
  AST <- ast2ast:::sort_args(AST, ast2ast:::function_registry_global)
  vars_types_list <- ast2ast:::infer_types(
    AST, fct, args_fct, r_fct, real_type, ast2ast:::function_registry_global, known_types
  )
  ast2ast:::type_checking(AST, vars_types_list, r_fct, real_type, ast2ast:::function_registry_global, known_types)
}
test_checks <- function(f, args_f, r_fct, error_message, info = "", known_types = list()) {
  e <- try(run_fr_checks(f, args_f, r_fct, known_types), silent = TRUE)
  e <- attributes(e)[["condition"]]$message
  expect_equal(as.character(e), error_message, info = info)
}

# --- subsetting stuff which cannot be subsetted ----------------------------------------------------------
args_fct <- function() {}

## preserved subsetting of a collection
types_fct <- function() {
  new_type(
    Point,
    slots(
      x |> type(double),
      y |> type(double)
    )
  )
}
known_types <- ast2ast:::make_known_types(types_fct, TRUE, "etr::Double")
f <- function() {
  pts <- vector(mode = "Point", 5)
  pts[1:3]
}
test_checks(
  f, args_fct, TRUE,
  "pts[1.0 : 3.0]\nFound unsupported subsetting: pts[1.0 : 3.0]",
  info = "",
  known_types
)

## subsetting a class
types_fct <- function() {
  new_type(
    Point,
    slots(
      x |> type(double),
      y |> type(double)
    )
  )
}
known_types <- ast2ast:::make_known_types(types_fct, TRUE, "etr::Double")
f <- function() {
  a |> type(Point)
  a[[1L]]
}
test_checks(
  f, args_fct, TRUE,
  "a[[1L]]\nFound unsupported left type Point in: a[[1L]]",
  info = "",
  known_types
)
f <- function() {
  a |> type(Point)
  a[1L]
}
test_checks(
  f, args_fct, TRUE,
  "a[1L]\nFound unsupported left type Point in: a[1L]",
  info = "",
  known_types
)
f <- function() {
  a |> type(Point)
  at(a, a)
}
test_checks(
  f, args_fct, TRUE,
  "at(a, a)\nFound unsupported left type Point in: at(a, a)",
  info = "",
  known_types
)
f <- function() {
  a |> type(Point)
  at(c(1, 2, 3), a)
}
test_checks(
  f, args_fct, TRUE,
  "at(c(1.0, 2.0, 3.0), a)\nFound unsupported right type Point in: at(c(1.0, 2.0, 3.0), a)",
  info = "",
  known_types
)

f <- function() {
  a |> type(double)
  a <- 3.14
  a[1]
}
test_checks(
  f, args_fct, TRUE,
  "a[1.0]\nYou cannot subset a scalar value"
)

f <- function() {
  a |> type(double)
  a <- 3.14
  a[[1L]]
}
test_checks(
  f, args_fct, TRUE,
  "a[[1L]]\nYou cannot subset a scalar value"
)

f <- function() {
  a |> type(double)
  a <- 3.14
  at(a, 1L)
}
test_checks(
  f, args_fct, TRUE,
  "at(a, 1L)\nYou cannot subset a scalar value"
)

# --- chained subsetting of a vector: the intermediate is a scalar --------------
# `[` with a single scalar index takes the fast path and yields a scalar, so a
# second subset (of any kind) has nothing left to index.

f <- function() {
  a <- numeric(10)
  a[[1L]][[1L]]
}
test_checks(
  f, args_fct, TRUE,
  "a[[1L]][[1L]]\nYou cannot subset a scalar value: [[ always yields a scalar, so the following [[ has nothing to index."
)

f <- function() {
  a <- numeric(10)
  a[1L][1L]
}
test_checks(
  f, args_fct, TRUE,
  "a[1L][1L]\nYou cannot subset a scalar value: with a single scalar index, [ takes the fast path and behaves like [[, yielding a scalar, so the following [ has nothing to index."
)

f <- function() {
  a <- numeric(10)
  a[1L][[1L]]
}
test_checks(
  f, args_fct, TRUE,
  "a[1L][[1L]]\nYou cannot subset a scalar value: with a single scalar index, [ takes the fast path and behaves like [[, yielding a scalar, so the following [[ has nothing to index."
)

f <- function() {
  a <- numeric(10)
  a[[1L]][1L]
}
test_checks(
  f, args_fct, TRUE,
  "a[[1L]][1L]\nYou cannot subset a scalar value: [[ yields a scalar, so the following [ has nothing to index."
)

# a single subset of a vector stays fine
f <- function() {
  a <- numeric(10)
  a[[1L]]
}
e <- try(run_fr_checks(f, args_fct, TRUE, list()), silent = TRUE)
expect_false(inherits(e, "try-error"))

# --- assignment ------------------------------------------------------------
f <- function() {
 for (i in 1:10) {
    i <- 3
  }
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "i <- 3.0\nYou cannot assign to an index variable"
)
f <- function() {
 for (i in 1:10) {
    i = 3
  }
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "i = 3.0\nYou cannot assign to an index variable"
)

f <- function(a) {
  a <- 100
}
args_fct <- function(a) {
  a |> type(int) |> const()
}
test_checks(
  f, args_fct, TRUE,
  "a <- 100.0\nYou cannot assign to a constant variable"
)

f <- function(a) {
  a = 100
}
args_fct <- function(a) {
  a |> type(int) |> const()
}
test_checks(
  f, args_fct, TRUE,
  "a = 100.0\nYou cannot assign to a constant variable"
)

# --- subsetting array ----------------------------------------------------------
f <- function() {
  array(0, c(2, 2))[NA, NA]
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "array(0.0, c(2.0, 2.0))[NA, NA]\nYou cannot use character/NA/NaN/Inf entries for subsetting"
)

f <- function() {
  array(0, c(2, 2))[NA]
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
"array(0.0, c(2.0, 2.0))[NA]\nYou cannot use character/NA/NaN/Inf entries in right type in: array(0.0, c(2.0, 2.0))[NA]"
)

# --- subsetting matrix ----------------------------------------------------------
f <- function() {
  matrix(0, 2, 2)[NA, NA]
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "matrix(0.0, 2.0, 2.0)[NA, NA]\nYou cannot use character/NA/NaN/Inf entries for subsetting"
)
f <- function() {
  matrix(0, 2, 2)[NA]
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "matrix(0.0, 2.0, 2.0)[NA]\nYou cannot use character/NA/NaN/Inf entries in right type in: matrix(0.0, 2.0, 2.0)[NA]"
)
f <- function() {
  a <- matrix(0, 5, 5)
  print(a[NA, NA])
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "print(a[NA, NA])\nYou cannot use character/NA/NaN/Inf entries for subsetting"
)
f <- function() {
  a <- matrix(0, 5, 5)
  print(a[NA, 2])
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
"print(a[NA, 2.0])\nYou cannot use character/NA/NaN/Inf entries for subsetting"
)
f <- function() {
  a <- matrix(0, 5, 5)
  print(a[2, NA])
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
"print(a[2.0, NA])\nYou cannot use character/NA/NaN/Inf entries for subsetting"
)

# --- subsetting vector ----------------------------------------------------------
f <- function() {
  c(1, 2, 3)[NA]
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "c(1.0, 2.0, 3.0)[NA]\nYou cannot use character/NA/NaN/Inf entries in right type in: c(1.0, 2.0, 3.0)[NA]"
)
f <- function() {
  a |> type(int) <- 1L
  print(a[1, 1])
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "print(a[1.0, 1.0])\nYou cannot subset a scalar value"
)
f <- function() {
  a |> type(int) <- 1L
  print(a[1])
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
"print(a[1.0])\nYou cannot subset a scalar value"
)
f <- function() {
  a |> type(int) <- 1L
  print(a[[1]])
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
"print(a[[1.0]])\nYou cannot subset a scalar value"
)
f <- function() {
  a |> type(int) <- 1L
  print(at(a, 1))
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
"print(at(a, 1.0))\nYou cannot subset a scalar value"
)
f <- function() {
  a <- numeric(10)
  print(a["Invalid"])
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "print(a[\"Invalid\"])\nYou cannot use character/NA/NaN/Inf entries in right type in: a[\"Invalid\"]"
)
f <- function() {
  a <- numeric(10)
  print(a[["Invalid"]])
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "print(a[[\"Invalid\"]])\nYou cannot use character/NA/NaN/Inf entries in right type in: a[[\"Invalid\"]]"
)
f <- function() {
  a <- numeric(10)
  print(at(a, "Invalid"))
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
"print(at(a, \"Invalid\"))\nYou cannot use character/NA/NaN/Inf entries in right type in: at(a, \"Invalid\")"
)

# --- for loop --------------------------------------------------------------
f <- function() {
  for(i in "asdsagf") {}
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "for (i in \"asdsagf\") {\nYou cannot use character/NA/NaN/Inf entries in sequence type in: \"asdsagf\""
)

# --- c ---------------------------------------------------------------------
f <- function() {
  a <- c()
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "a <- c()\nYou cannot use c without any arguments"
)
f <- function() {
  a <- c(1, 2, "Invalid")
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "a <- c(1.0, 2.0, \"Invalid\")\nYou cannot use character/NA/NaN/Inf entries in type in: c(1.0, 2.0, \"Invalid\")"
)

args_fct <- function() {}
types_fct <- function() {
  new_type(
    Point,
    slots(
      x |> type(double),
      y |> type(double)
    )
  )
}
known_types <- ast2ast:::make_known_types(types_fct, TRUE, "etr::Double")
f <- function() {
  pts <- vector(mode = "Point", 5)
  a <- c(3.14, pts)
}
test_checks(
  f, args_fct, TRUE,
  "a <- c(3.14, pts)\nFound unsupported type collection(Point) in: c(3.14, pts)",
  info = "",
  known_types
)

# --- : ---------------------------------------------------------------------
f <- function() {
  a <- "a":"b"
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
"a <- \"a\" : \"b\"\nYou cannot use character/NA/NaN/Inf entries in left type in: \"a\" : \"b\""
)
# --- rep -------------------------------------------------------------------
f <- function() {
  a <- rep("a", 3)
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
"a <- rep(\"a\", 3.0)\nYou cannot use character/NA/NaN/Inf entries in left type in: rep(\"a\", 3.0)"
)
f <- function() {
  a <- rep("a", "b")
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
"a <- rep(\"a\", \"b\")\nYou cannot use character/NA/NaN/Inf entries in left type in: rep(\"a\", \"b\")"
)
# --- seq_len ----------------------------------------------------------------
f <- function() {
  a <- seq_len(TRUE)
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
"a <- seq_len(true)\nYou can only call seq_len on variables of type integer or double"
)
# --- seq_len ----------------------------------------------------------------
f <- function() {
  a <- seq_along("Bla")
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
"a <- seq_along(\"Bla\")\nYou cannot use character/NA/NaN/Inf entries in type in: seq_along(\"Bla\")"
)
# --- unary math--------------------------------------------------------------
args_fct <- function() {}
fcts <- list()
fcts[[1]] <- function() {a <- sin("a")}
fcts[[2]] <- function() {a <- asin("a")}
fcts[[3]] <- function() {a <- sinh("a")}
fcts[[4]] <- function() {a <- cos("a")}
fcts[[5]] <- function() {a <- acos("a")}
fcts[[6]] <- function() {a <- cosh("a")}
fcts[[7]] <- function() {a <- tan("a")}
fcts[[8]] <- function() {a <- atan("a")}
fcts[[9]] <- function() {a <- tanh("a")}
fcts[[10]] <- function() {a <- log("a")}
fcts[[11]] <- function() {a <- sqrt("a")}
fcts[[12]] <- function() {a <- exp("a")}
fcts[[13]] <- function() {a <- -"a"}
checks <- logical(13)
fct_strings <- c("sin", "asin", "sinh", "cos", "acos", "cosh", "tan", "atan", "tanh", "log", "sqrt", "exp", "-")
for (i in 1:13) {
  message <- sprintf(
    "a <- %s(\"a\")\nYou cannot use character/NA/NaN/Inf entries in type in: %s(\"a\")",
    fct_strings[i], fct_strings[i])
  e <- try(run_fr_checks(fcts[[i]], args_fct, TRUE, list()), silent = TRUE)
  e <- attributes(e)[["condition"]]$message
  if (i == 13) {
    message <- sprintf(
      "a <- %s\"a\"\nYou cannot use character/NA/NaN/Inf entries in type in: %s\"a\"",
      fct_strings[i], fct_strings[i])
    checks[i] <- message == e
  } else {
    checks[i] <- message == e
  }
}
expect_true(all(checks), info = "Test functions in function registry for unary math")
# --- binary math--------------------------------------------------------------
args_fct <- function() {}
f <- function() {
  1^Inf
}
test_checks(
  f, args_fct, TRUE,
"1.0 ^ Inf\nYou cannot use character/NA/NaN/Inf entries in right type in: 1.0 ^ Inf"
)
args_fct <- function() {}
f <- function() {
  a <- 1L
  c <- a + NA
}
test_checks(
  f, args_fct, TRUE,
"c <- a + NA\nYou cannot use character/NA/NaN/Inf entries in right type in: a + NA"
)
args_fct <- function() {}
f <- function() {
  a <- 1L
  c <- a - NA
}
test_checks(
  f, args_fct, TRUE,
"c <- a - NA\nYou cannot use character/NA/NaN/Inf entries in right type in: a - NA"
)
args_fct <- function() {}
f <- function() {
  a <- 1L
  c <- a * NA
}
test_checks(
  f, args_fct, TRUE,
"c <- a * NA\nYou cannot use character/NA/NaN/Inf entries in right type in: a * NA"
)
args_fct <- function() {}
f <- function() {
  a <- 1L
  c <- a / NA
}
test_checks(
  f, args_fct, TRUE,
"c <- a / NA\nYou cannot use character/NA/NaN/Inf entries in right type in: a / NA"
)
args_fct <- function() {}
f <- function() {
  a <- 1L
  c <- a %% NA
}
test_checks(
  f, args_fct, TRUE,
"c <- a %% NA\nYou cannot use character/NA/NaN/Inf entries in right type in: a %% NA"
)
args_fct <- function() {}
f <- function() {
  a <- 1L
  c <- a %/% NA
}
test_checks(
  f, args_fct, TRUE,
"c <- a %/% NA\nYou cannot use character/NA/NaN/Inf entries in right type in: a %/% NA"
)
args_fct <- function() {}
f <- function() {
  a <- 1L
  a == NA
  a != NA
  a > NA
  a >= NA
  a < NA
  a <= NA
  NA && a
  NA || a
  NA & a
  NA | a
}
e <- try(run_fr_checks(f, args_fct, TRUE, list()), silent = TRUE)
e <- attributes(e)[["condition"]]$message
got <- strsplit(e, "\n\n")[[1]] |> as.list()
expected <- list(
"a == NA\nYou cannot use character/NA/NaN/Inf entries in right type in: a == NA",
"a != NA\nYou cannot use character/NA/NaN/Inf entries in right type in: a != NA",
"a > NA\nYou cannot use character/NA/NaN/Inf entries in right type in: a > NA",
"a >= NA\nYou cannot use character/NA/NaN/Inf entries in right type in: a >= NA",
"a < NA\nYou cannot use character/NA/NaN/Inf entries in right type in: a < NA",
"a <= NA\nYou cannot use character/NA/NaN/Inf entries in right type in: a <= NA",
"NA && a\nYou cannot use character/NA/NaN/Inf entries in left type in: NA && a",
"NA || a\nYou cannot use character/NA/NaN/Inf entries in left type in: NA || a",
"NA & a\nYou cannot use character/NA/NaN/Inf entries in left type in: NA & a",
"NA | a\nYou cannot use character/NA/NaN/Inf entries in left type in: NA | a"
)
checks <- Map(function(g, e) {
  g == e
}, got, expected) |> unlist()
expect_true(all(checks), info = "check functions for comparison and logical functions")

# --- vector/logical/integer/numeric & matrix --------------------------------------------------------
f <- function() {
  a <- logical(NA)
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "a <- logical(NA)\nYou cannot use character/NA/NaN/Inf entries in type in: logical(NA)"
)
f <- function() {
  a <- integer(NA)
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "a <- integer(NA)\nYou cannot use character/NA/NaN/Inf entries in type in: integer(NA)"
)
f <- function() {
  a <- numeric(NA)
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "a <- numeric(NA)\nYou cannot use character/NA/NaN/Inf entries in type in: numeric(NA)"
)
f <- function() {
  a <- vector("integer", "Bla")
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "a <- vector(\"integer\", \"Bla\")\nYou cannot use character/NA/NaN/Inf entries in right type in: vector(\"integer\", \"Bla\")"
)
f <- function() {
  m <- matrix(1, "nrow", "ncol")
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "m <- matrix(1.0, \"nrow\", \"ncol\")\nYou cannot use character/NA/NaN/Inf entries in type in: matrix(1.0, \"nrow\", \"ncol\")"
)
f <- function() {
  m <- matrix(1, "nrow", 1)
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "m <- matrix(1.0, \"nrow\", 1.0)\nYou cannot use character/NA/NaN/Inf entries in type in: matrix(1.0, \"nrow\", 1.0)"
)
f <- function() {
  m <- array("invalid", c(2, 2))
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
"m <- array(\"invalid\", c(2.0, 2.0))\nYou cannot use character/NA/NaN/Inf entries in type in: array(\"invalid\", c(2.0, 2.0))"
)

# --- length, dim, nrow and ncol ---------------------------------------------
f <- function() {
  a <- vector("integer", 10)
  l <- length(a)
  d <- dim(a)
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "d <- dim(a)\nYou can only call dim on variables of type array or matrix"
)
f <- function() {
  a <- 1L
  l <- length(a)
  d <- dim(a)
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
"l <- length(a)\nYou can only call length on variables of type array, matrix, vector or collection\n\nd <- dim(a)\nYou can only call dim on variables of type array or matrix"
)
f <- function() {
  a <- 1L
  l <- nrow(a)
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
"l <- nrow(a)\nYou can only call nrow on variables of type array or matrix"
)
f <- function() {
  a <- 1L
  l <- ncol(a)
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
"l <- ncol(a)\nYou can only call ncol on variables of type array or matrix"
)

# --- cmr --------------------------------------------------------------------
f <- function() {
  cmr(TRUE, c(1, 2, 3), c(1, 2, 3))
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "cmr(true, c(1.0, 2.0, 3.0), c(1.0, 2.0, 3.0))\nThe first argument of cmr has to have the base type double"
)

f <- function() {
  g <- cmr(TRUE, c(1, 2, 3), c(1, 2, 3))
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "g <- cmr(true, c(1.0, 2.0, 3.0), c(1.0, 2.0, 3.0))\nThe first argument of cmr has to have the base type double"
)
f <- function() {
  g <- cmr(1, 2L, c(1, 2, 3))
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "g <- cmr(1.0, 2L, c(1.0, 2.0, 3.0))\nThe second argument of cmr has to be a vector"
)
f <- function() {
  g <- cmr(1, c(1), TRUE)
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "g <- cmr(1.0, c(1.0), true)\nThe third argument of cmr has to be a vector"
)
f <- function() {
  a <- logical(10)
  b <- numeric(1)
  g <- cmr(1, a, b)
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "g <- cmr(1.0, a, b)\nThe second argument of cmr has to have the base type double"
)
f <- function() {
  a <- numeric(10)
  b <- logical(1)
  g <- cmr(1, a, b)
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
"g <- cmr(1.0, a, b)\nThe third argument of cmr has to have the base type double"
)

# --- reductions (min/max/which.max/which.min/all/any) -------------------------
args_fct <- function() {}
red_fcts <- list(
  function() {a <- max("a")},
  function() {a <- min("a")},
  function() {a <- which.max("a")},
  function() {a <- which.min("a")},
  function() {a <- all("a")},
  function() {a <- any("a")}
)
red_names <- c("max", "min", "which.max", "which.min", "all", "any")
red_checks <- logical(length(red_fcts))
for (i in seq_along(red_fcts)) {
  message <- sprintf(
    "a <- %s(\"a\")\nYou cannot use character/NA/NaN/Inf entries in type in: %s(\"a\")",
    red_names[i], red_names[i])
  e <- try(run_fr_checks(red_fcts[[i]], args_fct, TRUE, list()), silent = TRUE)
  e <- attributes(e)[["condition"]]$message
  red_checks[i] <- message == e
}
expect_true(all(red_checks), info = "Test check functions for reductions")

# --- rbind / cbind ----------------------------------------------------------
args_fct <- function() {}
f <- function() {
  a <- rbind(1, 2, "Invalid")
}
test_checks(
  f, args_fct, TRUE,
  "a <- rbind(1.0, 2.0, \"Invalid\")\nYou cannot use character/NA/NaN/Inf entries in type in: rbind(1.0, 2.0, \"Invalid\")"
)
f <- function() {
  a <- cbind(1, 2, "Invalid")
}
test_checks(
  f, args_fct, TRUE,
  "a <- cbind(1.0, 2.0, \"Invalid\")\nYou cannot use character/NA/NaN/Inf entries in type in: cbind(1.0, 2.0, \"Invalid\")"
)

# --- floor / ceiling / trunc ------------------------------------------------
args_fct <- function() {}
rt_fcts <- list(
  function() {a <- floor("a")},
  function() {a <- ceiling("a")},
  function() {a <- trunc("a")}
)
rt_names <- c("floor", "ceiling", "trunc")
rt_checks <- logical(length(rt_fcts))
for (i in seq_along(rt_fcts)) {
  message <- sprintf(
    "a <- %s(\"a\")\nYou cannot use character/NA/NaN/Inf entries in type in: %s(\"a\")",
    rt_names[i], rt_names[i])
  e <- try(run_fr_checks(rt_fcts[[i]], args_fct, TRUE, list()), silent = TRUE)
  e <- attributes(e)[["condition"]]$message
  rt_checks[i] <- message == e
}
expect_true(all(rt_checks), info = "Test check functions for floor/ceiling/trunc")

# --- sum / prod -------------------------------------------------------------
args_fct <- function() {}
sp_fcts <- list(
  function() {a <- sum("a")},
  function() {a <- prod("a")}
)
sp_names <- c("sum", "prod")
sp_checks <- logical(length(sp_fcts))
for (i in seq_along(sp_fcts)) {
  message <- sprintf(
    "a <- %s(\"a\")\nYou cannot use character/NA/NaN/Inf entries in type in: %s(\"a\")",
    sp_names[i], sp_names[i])
  e <- try(run_fr_checks(sp_fcts[[i]], args_fct, TRUE, list()), silent = TRUE)
  e <- attributes(e)[["condition"]]$message
  sp_checks[i] <- message == e
}
expect_true(all(sp_checks), info = "Test check functions for sum/prod")

# --- chol -------------------------------------------------------------------
args_fct <- function() {}
f <- function() {
  a <- chol("a")
}
test_checks(
  f, args_fct, TRUE,
  "a <- chol(\"a\")\nYou cannot use character/NA/NaN/Inf entries in type in: chol(\"a\")"
)
f <- function() {
  s <- 5.0
  m <- chol(s)
}
test_checks(
  f, args_fct, TRUE,
  "m <- chol(s)\nYou can only call chol on a matrix"
)
f <- function() {
  v <- c(1.0, 2.0, 3.0)
  m <- chol(v)
}
test_checks(
  f, args_fct, TRUE,
  "m <- chol(v)\nYou can only call chol on a matrix"
)

# --- get_diag ---------------------------------------------------------------
f <- function() {
  a <- get_diag("a")
}
test_checks(
  f, args_fct, TRUE,
  "a <- get_diag(\"a\")\nYou cannot use character/NA/NaN/Inf entries in type in: get_diag(\"a\")"
)
f <- function() {
  s <- 5.0
  m <- get_diag(s)
}
test_checks(
  f, args_fct, TRUE,
  "m <- get_diag(s)\nYou can only call get_diag on a matrix"
)
f <- function() {
  v <- c(1.0, 2.0, 3.0)
  m <- get_diag(v)
}
test_checks(
  f, args_fct, TRUE,
  "m <- get_diag(v)\nYou can only call get_diag on a matrix"
)

# --- crossprod --------------------------------------------------------------
f <- function() {
  a <- crossprod("a")
}
test_checks(
  f, args_fct, TRUE,
  "a <- crossprod(\"a\")\nYou cannot use character/NA/NaN/Inf entries in type in: crossprod(\"a\")"
)
f <- function() {
  s <- 5.0
  m <- crossprod(s)
}
test_checks(
  f, args_fct, TRUE,
  "m <- crossprod(s)\nYou can only call crossprod on a matrix"
)
f <- function() {
  v <- c(1.0, 2.0, 3.0)
  m <- crossprod(v)
}
test_checks(
  f, args_fct, TRUE,
  "m <- crossprod(v)\nYou can only call crossprod on a matrix"
)

# --- tcrossprod -------------------------------------------------------------
f <- function() {
  a <- tcrossprod("a")
}
test_checks(
  f, args_fct, TRUE,
  "a <- tcrossprod(\"a\")\nYou cannot use character/NA/NaN/Inf entries in type in: tcrossprod(\"a\")"
)
f <- function() {
  s <- 5.0
  m <- tcrossprod(s)
}
test_checks(
  f, args_fct, TRUE,
  "m <- tcrossprod(s)\nYou can only call tcrossprod on a matrix"
)
f <- function() {
  v <- c(1.0, 2.0, 3.0)
  m <- tcrossprod(v)
}
test_checks(
  f, args_fct, TRUE,
  "m <- tcrossprod(v)\nYou can only call tcrossprod on a matrix"
)

# --- solve ------------------------------------------------------------------
# character argument is rejected in either position
f <- function() {
  a <- solve("a")
}
test_checks(
  f, args_fct, TRUE,
  "a <- solve(\"a\")\nYou cannot use character/NA/NaN/Inf entries in type in: solve(\"a\")"
)
f <- function() {
  m <- matrix(1.0, 2, 2)
  x <- solve(m, "b")
}
test_checks(
  f, args_fct, TRUE,
  "x <- solve(m, \"b\")\nYou cannot use character/NA/NaN/Inf entries in type in: solve(m, \"b\")"
)

# --- backsolve / forwardsolve -----------------------------------------------
f <- function() {
  m <- matrix(1.0, 2, 2)
  x <- backsolve(m, "b")
}
test_checks(
  f, args_fct, TRUE,
  "x <- backsolve(m, \"b\")\nYou cannot use character/NA/NaN/Inf entries in type in: backsolve(m, \"b\")"
)
f <- function() {
  m <- matrix(1.0, 2, 2)
  x <- forwardsolve(m, "b")
}
test_checks(
  f, args_fct, TRUE,
  "x <- forwardsolve(m, \"b\")\nYou cannot use character/NA/NaN/Inf entries in type in: forwardsolve(m, \"b\")"
)
