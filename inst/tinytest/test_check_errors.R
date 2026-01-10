library(tinytest)

# --- helpers ---------------------------------------------------------------
get_checks <- function(f, r_fct = TRUE) {
  ast <- ast2ast:::parse_body(body(f), r_fct)
  ast2ast:::run_checks(ast, r_fct)
}
test_action_error <- function(f, error_message, r_fct = TRUE) {
  e <- try(get_checks(f), silent = TRUE)
  expect_equal(as.character(e), error_message)
}

# --- type declarations -----------------------------------------------------
f <- function() {
  type(1, int)
}
test_action_error(
  f,
  "Error in ast2ast:::run_checks(ast, r_fct) : \ntype(1.0, int)\nInvalid type declaration for: 1.0\n"
)
f <- function() {
  type(a, 3.14)
}
test_action_error(
  f,
  "Error in ast2ast:::run_checks(ast, r_fct) : \ntype(a, 3.14)\nFound unsupported base type: 3.14\nInvalid type declaration: 3.14 for variable a\n"
)
f <- function() {
  type(a, integr)
}
test_action_error(
  f,
  "Error in ast2ast:::run_checks(ast, r_fct) : \ntype(a, integr)\nFound unsupported base type: integr\nInvalid type declaration: integr for variable a\n"
)
f <- function() {
  type(a, vec(integr))
}
test_action_error(
  f,
  "Error in ast2ast:::run_checks(ast, r_fct) : \ntype(a, vec(integr))\nFound unsupported base type: integr\nInvalid type declaration: integr for variable a\n"
)
f <- function() {
  type(a, vectr(integr))
}
test_action_error(
  f,
  "Error in ast2ast:::run_checks(ast, r_fct) : \ntype(a, vectr(integr))\nFound unsupported base type: integr\nFound unsupported data structure: vectr\nInvalid type declaration: integr for variable a\n"
)
f <- function() {
  type(a, vectr(int))
}
test_action_error(
  f,
  "Error in ast2ast:::run_checks(ast, r_fct) : \ntype(a, vectr(int))\nFound unsupported data structure: vectr\nInvalid type declaration: vectr for variable a\n"
)

# --- variable names --------------------------------------------------------
f <- function() {
  int <- 2
  a |> type(int) <- 3L
}
test_action_error(
  f,
  "Error in ast2ast:::run_checks(ast, r_fct) : \nint <- 2.0\nInvalid variable name (reserved internally) int found in int\n"
)
f <- function() {
  vec <- 2
  a |> type(int) <- 3L
}
test_action_error(
  f,
  "Error in ast2ast:::run_checks(ast, r_fct) : \nvec <- 2.0\nInvalid variable name (reserved internally) vec found in vec\n"
)
f <- function() {
  class <- 3
}
test_action_error(
  f,
  "Error in ast2ast:::run_checks(ast, r_fct) : \nclass <- 3.0\nInvalid variable name: is a C++ keyword --> class\n"
)

f <- function() {
  a.b <- 2
}
test_action_error(
  f,
  "Error in ast2ast:::run_checks(ast, r_fct) : \na.b <- 2.0\nInvalid variable name: contains forbidden character --> . found in a.b\n"
)
f <- function() {
  .a <- 2
}
test_action_error(
  f,
  "Error in ast2ast:::run_checks(ast, r_fct) : \n.a <- 2.0\nInvalid variable name: contains forbidden character --> . found in .a\n"
)
f <- function() {
  aSEXP <- 2
}
test_action_error(
  f,
  "Error in ast2ast:::run_checks(ast, r_fct) : \naSEXP <- 2.0\nInvalid variable name: contains forbidden character --> SEXP found in aSEXP\n"
)
f <- function() {
  SEXP <- 2
}
test_action_error(
  f,
  "Error in ast2ast:::run_checks(ast, r_fct) : \nSEXP <- 2.0\nInvalid variable name: contains forbidden character --> SEXP found in SEXP\n"
)
f <- function() {
  agetXPtr <- 2
}
test_action_error(
  f,
  "Error in ast2ast:::run_checks(ast, r_fct) : \nagetXPtr <- 2.0\nInvalid variable name: contains forbidden character --> getXPtr found in agetXPtr\n"
)
f <- function() {
  a_fct_ptr <- 3
}
test_action_error(
  f,
  "Error in ast2ast:::run_checks(ast, r_fct) : \na_fct_ptr <- 3.0\nInvalid variable name: contains forbidden character --> fct_ptr found in a_fct_ptr\n"
)

# --- ops at lhs ------------------------------------------------------------
f <- function() {
  m <- matrix(0, 2, 3)
  m[1] <- 3
  m[1, 1] <- 4
  m[[1]] <- 5
  at(m, 5, 6) <- 10
  g |> type(double) <- 314
  print(m) <- 1
}
test_action_error(
  f,
  "Error in ast2ast:::run_checks(ast, r_fct) : \nprint(m) <- 1.0\nFound invalid expression at left side of assignment: <-\n"
)

# --- only permitted functions ----------------------------------------------
f <- function() {
  a <- bla()
}
test_action_error(
  f,
  "Error in ast2ast:::run_checks(ast, r_fct) : \na <- bla()\nInvalid function bla\n"
)

f <- function() {
  return(bla())
}
test_action_error(
  f,
  "Error in ast2ast:::run_checks(ast, r_fct) : \nreturn(bla())\nInvalid function bla\n"
)

f <- function() {
  a %+% b
}
test_action_error(
  f,
  "Error in ast2ast:::run_checks(ast, r_fct) : \na %+% b\nInvalid function %+%\n"
)

f <- function() {
  a <- cmr(1, 2, 3, 4)
}
test_action_error(
  f,
  "Error in ast2ast:::run_checks(ast, r_fct) : \na <- cmr(1.0, 2.0, 3.0, 4.0)\nWrong number of arguments for: cmr\n"
)

# --- arity ----------------------------------------------------------------
f <- function() {
  return(1, 2)
}
test_action_error(
  f,
  "Error in ast2ast:::run_checks(ast, r_fct) : \nreturn(1.0, 2.0)\nWrong number of arguments for: return\n"
)

f <- function() {
  `+`(1, 2, 3)
}
test_action_error(
  f,
  "Error in ast2ast:::run_checks(ast, r_fct) : \n+(1.0, 2.0, 3.0)\nWrong number of arguments for: +\n"
)

f <- function() {
  a <- cmr(1, 2, 3, 4)
}
test_action_error(
  f,
  "Error in ast2ast:::run_checks(ast, r_fct) : \na <- cmr(1.0, 2.0, 3.0, 4.0)\nWrong number of arguments for: cmr\n"
)

# --- named args -----------------------------------------------------------
f <- function() {
  vector(mod = "integer", 2L)
}
test_action_error(
  f,
  "Error in ast2ast:::run_checks(ast, r_fct) : \nvector(\"integer\", 2L)\nFound wrong named argument for: vector\n"
)
f <- function() {
  vector(mode = "integer", lengt = 2L)
}
test_action_error(
  f,
  "Error in ast2ast:::run_checks(ast, r_fct) : \nvector(\"integer\", 2L)\nFound wrong named argument for: vector\n"
)
f <- function() {
  vector(mde = "integer", lengt = 2L)
}
test_action_error(
  f,
  "Error in ast2ast:::run_checks(ast, r_fct) : \nvector(\"integer\", 2L)\nFound wrong named argument for: vector\n"
)

f <- function() {
  matrix(dat = 3, nrow = 2, ncol = 2)
}
test_action_error(
  f,
  "Error in ast2ast:::run_checks(ast, r_fct) : \nmatrix(3.0, 2.0, 2.0)\nFound wrong named argument for: matrix\n"
)
f <- function() {
  matrix(data = 3, now = 2, ncol = 2)
}
test_action_error(
  f,
  "Error in ast2ast:::run_checks(ast, r_fct) : \nmatrix(3.0, 2.0, 2.0)\nFound wrong named argument for: matrix\n"
)
f <- function() {
  matrix(dta = 3, nrw = 2, ncol = 2)
}
test_action_error(
  f,
  "Error in ast2ast:::run_checks(ast, r_fct) : \nmatrix(3.0, 2.0, 2.0)\nFound wrong named argument for: matrix\n"
)
f <- function() {
  matrix(data = 3, nrow = 2, ncl = 2)
}
test_action_error(
  f,
  "Error in ast2ast:::run_checks(ast, r_fct) : \nmatrix(3.0, 2.0, 2.0)\nFound wrong named argument for: matrix\n"
)
f <- function() {
  matrix(dta = 3, nrow = 2, ncl = 2)
}
test_action_error(
  f,
  "Error in ast2ast:::run_checks(ast, r_fct) : \nmatrix(3.0, 2.0, 2.0)\nFound wrong named argument for: matrix\n"
)
f <- function() {
  matrix(data = 3, now = 2, ncl = 2)
}
test_action_error(
  f,
  "Error in ast2ast:::run_checks(ast, r_fct) : \nmatrix(3.0, 2.0, 2.0)\nFound wrong named argument for: matrix\n"
)
f <- function() {
  matrix(daa = 3, now = 2, ncl = 2)
}
test_action_error(
  f,
  "Error in ast2ast:::run_checks(ast, r_fct) : \nmatrix(3.0, 2.0, 2.0)\nFound wrong named argument for: matrix\n"
)
