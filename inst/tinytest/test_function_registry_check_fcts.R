library(tinytest)

# --- helpers ---------------------------------------------------------------
run_fr_checks <- function(fct, args_fct, r_fct = TRUE) {
  AST <- ast2ast:::parse_body(body(fct), r_fct)
  AST <- ast2ast:::sort_args(AST)
  vars_types_list <- ast2ast:::infer_types(AST, fct, args_fct, r_fct)
  ast2ast:::type_checking(AST, vars_types_list, r_fct)
}
test_checks <- function(f, args_f, r_fct, error_message, info = "") {
  e <- try(run_fr_checks(f, args_f, r_fct), silent = TRUE)
  expect_equal(as.character(e), error_message, info = info)
}

# --- assignment ------------------------------------------------------------
f <- function() {
 for (i in 1:10) {
    i <- 3
  }
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "Error in ast2ast:::type_checking(AST, vars_types_list, r_fct) : \n  i <- etr::Double(3.0)\nYou cannot assign to an index variable\n"
)
f <- function() {
 for (i in 1:10) {
    i = 3
  }
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "Error in ast2ast:::type_checking(AST, vars_types_list, r_fct) : \n  i = etr::Double(3.0)\nYou cannot assign to an index variable\n"
)

# --- subsetting matrix ----------------------------------------------------------
f <- function() {
  matrix(0, 2, 2)[NA, NA]
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "Error in ast2ast:::type_checking(AST, vars_types_list, r_fct) : \n  [(matrix(etr::Double(0.0), etr::Double(2.0), etr::Double(2.0)), etr::Double::NA(), etr::Double::NA())\nYou cannot use character/NA/NaN/Inf entries for subsetting\n"
)
f <- function() {
  matrix(0, 2, 2)[NA]
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "Error in ast2ast:::type_checking(AST, vars_types_list, r_fct) : \n  matrix(etr::Double(0.0), etr::Double(2.0), etr::Double(2.0)) [ etr::Double::NA()\nYou cannot use character/NA/NaN/Inf entries for subsetting\n"
)
f <- function() {
  a <- matrix(0, 5, 5)
  print(a[NA, NA])
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "Error in ast2ast:::type_checking(AST, vars_types_list, r_fct) : \n  print([(a, etr::Double::NA(), etr::Double::NA()))\nYou cannot use character/NA/NaN/Inf entries for subsetting\n"
)
f <- function() {
  a <- matrix(0, 5, 5)
  print(a[NA, 2])
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
"Error in ast2ast:::type_checking(AST, vars_types_list, r_fct) : \n  print([(a, etr::Double::NA(), etr::Double(2.0)))\nYou cannot use character/NA/NaN/Inf entries for subsetting\n"
)
f <- function() {
  a <- matrix(0, 5, 5)
  print(a[2, NA])
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
"Error in ast2ast:::type_checking(AST, vars_types_list, r_fct) : \n  print([(a, etr::Double(2.0), etr::Double::NA()))\nYou cannot use character/NA/NaN/Inf entries for subsetting\n"
)

# --- subsetting vector ----------------------------------------------------------
f <- function() {
  c(1, 2, 3)[NA]
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "Error in ast2ast:::type_checking(AST, vars_types_list, r_fct) : \n  c(etr::Double(1.0), etr::Double(2.0), etr::Double(3.0)) [ etr::Double::NA()\nYou cannot use character/NA/NaN/Inf entries for subsetting\n"
)
f <- function() {
  a |> type(int) <- 1L
  print(a[1, 1])
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "Error in ast2ast:::type_checking(AST, vars_types_list, r_fct) : \n  print([(a, etr::Double(1.0), etr::Double(1.0)))\nYou can only subset variables of type matrix or vector\n"
)
f <- function() {
  a |> type(int) <- 1L
  print(a[1])
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
"Error in ast2ast:::type_checking(AST, vars_types_list, r_fct) : \n  print(a [ etr::Double(1.0))\nYou can only subset variables of type matrix or vector\n"
)
f <- function() {
  a |> type(int) <- 1L
  print(a[[1]])
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
"Error in ast2ast:::type_checking(AST, vars_types_list, r_fct) : \n  print(a [[ etr::Double(1.0))\nYou can only subset variables of type matrix or vector\n"
)
f <- function() {
  a |> type(int) <- 1L
  print(at(a, 1))
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
"Error in ast2ast:::type_checking(AST, vars_types_list, r_fct) : \n  print(a at etr::Double(1.0))\nYou can only subset variables of type matrix or vector\n"
)
f <- function() {
  a <- numeric(10)
  print(a["Invalid"])
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "Error in ast2ast:::type_checking(AST, vars_types_list, r_fct) : \n  print(a [ \"Invalid\")\nYou cannot use character/NA/NaN/Inf entries for subsetting\n"
)
f <- function() {
  a <- numeric(10)
  print(a[["Invalid"]])
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "Error in ast2ast:::type_checking(AST, vars_types_list, r_fct) : \n  print(a [[ \"Invalid\")\nYou cannot use character/NA/NaN/Inf entries for subsetting\n"
)
f <- function() {
  a <- numeric(10)
  print(at(a, "Invalid"))
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
"Error in ast2ast:::type_checking(AST, vars_types_list, r_fct) : \n  print(a at \"Invalid\")\nYou cannot use character/NA/NaN/Inf entries for subsetting\n"
)

# --- for loop --------------------------------------------------------------
f <- function() {
  for(i in "asdsagf") {}
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "Error in ast2ast:::type_checking(AST, vars_types_list, r_fct) : \n  for (i in \"asdsagf\") {\nYou cannot sequence over characters/NA/NaN/Inf\n"
)
# --- c ---------------------------------------------------------------------
f <- function() {
  a <- c(1, 2, "Invalid")
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "Error in ast2ast:::type_checking(AST, vars_types_list, r_fct) : \n  a <- c(etr::Double(1.0), etr::Double(2.0), \"Invalid\")\nYou cannot use character entries in c\n"
)
# --- : ---------------------------------------------------------------------
f <- function() {
  a <- "a":"b"
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
"Error in ast2ast:::type_checking(AST, vars_types_list, r_fct) : \n  a <- \"a\" : \"b\"\nYou cannot use character/NA/NaN/Inf entries in :\n"
)
# --- rep -------------------------------------------------------------------
f <- function() {
  a <- rep("a", 3)
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
"Error in ast2ast:::type_checking(AST, vars_types_list, r_fct) : \n  a <- \"a\" rep etr::Double(3.0)\nYou cannot use character/NA/NaN/Inf entries in rep\n"
)
f <- function() {
  a <- rep("a", "b")
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
"Error in ast2ast:::type_checking(AST, vars_types_list, r_fct) : \n  a <- \"a\" rep \"b\"\nYou cannot use character/NA/NaN/Inf entries in rep\n"
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
    "Error in ast2ast:::type_checking(AST, vars_types_list, r_fct) : \n  a <- %s(\"a\")\nYou cannot use character/NA/NaN/Inf entries in %s\n",
    fct_strings[i], fct_strings[i])
  e <- try(run_fr_checks(fcts[[i]], args_fct, TRUE), silent = TRUE)
  if (i == 13) {
    message <- sprintf(
      "Error in ast2ast:::type_checking(AST, vars_types_list, r_fct) : a <- %s\"a\"\nYou cannot use character/NA/NaN/Inf entries in %s\n",
      fct_strings[i], fct_strings[i])
    checks[i] <- message == as.character(e)
  } else {
    checks[i] <- message == as.character(e)
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
"Error in ast2ast:::type_checking(AST, vars_types_list, r_fct) : \n  etr::Double(1.0) ^ etr::Double::Inf()\nYou cannot use character/NA/NaN/Inf entries in ^\n"
)
args_fct <- function() {}
f <- function() {
  a <- 1L
  c <- a + NA
}
test_checks(
  f, args_fct, TRUE,
"Error in ast2ast:::type_checking(AST, vars_types_list, r_fct) : \n  c <- a + etr::Double::NA()\nYou cannot use character/NA/NaN/Inf entries in +\n"
)
args_fct <- function() {}
f <- function() {
  a <- 1L
  c <- a - NA
}
test_checks(
  f, args_fct, TRUE,
"Error in ast2ast:::type_checking(AST, vars_types_list, r_fct) : \n  c <- a - etr::Double::NA()\nYou cannot use character/NA/NaN/Inf entries in -\n"
)
args_fct <- function() {}
f <- function() {
  a <- 1L
  c <- a * NA
}
test_checks(
  f, args_fct, TRUE,
"Error in ast2ast:::type_checking(AST, vars_types_list, r_fct) : \n  c <- a * etr::Double::NA()\nYou cannot use character/NA/NaN/Inf entries in *\n"
)
args_fct <- function() {}
f <- function() {
  a <- 1L
  c <- a / NA
}
test_checks(
  f, args_fct, TRUE,
"Error in ast2ast:::type_checking(AST, vars_types_list, r_fct) : \n  c <- a / etr::Double::NA()\nYou cannot use character/NA/NaN/Inf entries in /\n"
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
e <- try(run_fr_checks(f, args_fct, TRUE), silent = TRUE)
got <- strsplit(e, "\n")[[1]] |> as.list()
expected <- list(
"Error in ast2ast:::type_checking(AST, vars_types_list, r_fct) : ",
"  a == etr::Double::NA()"                                                     ,"You cannot use character/NA/NaN/Inf entries in =="                  ,"",
"a != etr::Double::NA()"                                                       ,"You cannot use character/NA/NaN/Inf entries in !="                  ,"",
"a > etr::Double::NA()"                                                        ,"You cannot use character/NA/NaN/Inf entries in >"                   ,"",
"a >= etr::Double::NA()"                                                       ,"You cannot use character/NA/NaN/Inf entries in >="                  ,"",
"a < etr::Double::NA()"                                                        ,"You cannot use character/NA/NaN/Inf entries in <"                   ,"",
"a <= etr::Double::NA()"                                                       ,"You cannot use character/NA/NaN/Inf entries in <="                  ,"",
"etr::Double::NA() && a"                                                       ,"You cannot use character/NA/NaN/Inf entries in &&"                  ,"",
"etr::Double::NA() || a"                                                       ,"You cannot use character/NA/NaN/Inf entries in ||"                  ,"",
"etr::Double::NA() & a"                                                        ,"You cannot use character/NA/NaN/Inf entries in &"                   ,"",
"etr::Double::NA() | a"                                                        ,"You cannot use character/NA/NaN/Inf entries in |"
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
  "Error in ast2ast:::type_checking(AST, vars_types_list, r_fct) : \n  a <- logical(etr::Double::NA())\nYou cannot use character/NA/NaN/Inf entries in logical\n"
)
f <- function() {
  a <- integer(NA)
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "Error in ast2ast:::type_checking(AST, vars_types_list, r_fct) : \n  a <- integer(etr::Double::NA())\nYou cannot use character/NA/NaN/Inf entries in integer\n"
)
f <- function() {
  a <- numeric(NA)
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "Error in ast2ast:::type_checking(AST, vars_types_list, r_fct) : \n  a <- numeric(etr::Double::NA())\nYou cannot use character/NA/NaN/Inf entries in numeric\n"
)
f <- function() {
  a <- vector("integer", "Bla")
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "Error in ast2ast:::type_checking(AST, vars_types_list, r_fct) : \n  a <- vector(\"integer\", \"Bla\")\nFound unallowed length type in vector\n"
)
f <- function() {
  m <- matrix(1, "nrow", "ncol")
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "Error in ast2ast:::type_checking(AST, vars_types_list, r_fct) : \n  m <- matrix(etr::Double(1.0), \"nrow\", \"ncol\")\nFound unallowed ncol type in matrix\n"
)
f <- function() {
  m <- matrix(1, "nrow", 1)
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "Error in ast2ast:::type_checking(AST, vars_types_list, r_fct) : \n  m <- matrix(etr::Double(1.0), \"nrow\", etr::Double(1.0))\nFound unallowed nrow type in matrix\n"
)

# --- length & dim ----------------------------------------------------------
f <- function() {
  a <- vector("integer", 10)
  l <- length(a)
  d <- dim(a)
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "Error in ast2ast:::type_checking(AST, vars_types_list, r_fct) : \n  d <- dim(a)\nYou can only call dim on variables of type matrix\n"
)
f <- function() {
  a <- 1L
  l <- length(a)
  d <- dim(a)
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
"Error in ast2ast:::type_checking(AST, vars_types_list, r_fct) : \n  l <- length(a)\nYou can only call length on variables of type matrix or vector\n\nd <- dim(a)\nYou can only call dim on variables of type matrix\n"
)

# --- cmr --------------------------------------------------------------------
f <- function() {
  cmr(TRUE, c(1, 2, 3), c(1, 2, 3))
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "Error in ast2ast:::type_checking(AST, vars_types_list, r_fct) : \n  cmr(etr::Logical(true), c(etr::Double(1.0), etr::Double(2.0), etr::Double(3.0)), c(etr::Double(1.0), etr::Double(2.0), etr::Double(3.0)))\nThe first argument of cmr has to have the base type double\n"
)

f <- function() {
  g <- cmr(TRUE, c(1, 2, 3), c(1, 2, 3))
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "Error in ast2ast:::type_checking(AST, vars_types_list, r_fct) : \n  g <- cmr(etr::Logical(true), c(etr::Double(1.0), etr::Double(2.0), etr::Double(3.0)), c(etr::Double(1.0), etr::Double(2.0), etr::Double(3.0)))\nThe first argument of cmr has to have the base type double\n"
)
f <- function() {
  g <- cmr(1, 2L, c(1, 2, 3))
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "Error in ast2ast:::type_checking(AST, vars_types_list, r_fct) : \n  g <- cmr(etr::Double(1.0), etr::Integer(2), c(etr::Double(1.0), etr::Double(2.0), etr::Double(3.0)))\nThe second argument of cmr has to be a vector\n"
)
f <- function() {
  g <- cmr(1, c(1), TRUE)
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "Error in ast2ast:::type_checking(AST, vars_types_list, r_fct) : \n  g <- cmr(etr::Double(1.0), c(etr::Double(1.0)), etr::Logical(true))\nThe third argument of cmr has to be a vector\n"
)
f <- function() {
  a <- logical(10)
  b <- numeric(1)
  g <- cmr(1, a, b)
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
  "Error in ast2ast:::type_checking(AST, vars_types_list, r_fct) : \n  g <- cmr(etr::Double(1.0), a, b)\nThe second argument of cmr has to have the base type double\n"
)
f <- function() {
  a <- numeric(10)
  b <- logical(1)
  g <- cmr(1, a, b)
}
args_fct <- function() {}
test_checks(
  f, args_fct, TRUE,
"Error in ast2ast:::type_checking(AST, vars_types_list, r_fct) : \n  g <- cmr(etr::Double(1.0), a, b)\nThe third argument of cmr has to have the base type double\n"
)
