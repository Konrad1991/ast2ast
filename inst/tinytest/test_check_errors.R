library(tinytest)

# --- helpers ---------------------------------------------------------------
get_checks <- function(f, r_fct = TRUE) {
  env <- new.env(parent = emptyenv())
  env$r_fct <- r_fct
  env$real_type <- "etr::Double"
  ast <- ast2ast:::parse_body(body(f), env, ast2ast:::function_registry_global)
  ast2ast:::run_checks(ast, r_fct, ast2ast:::function_registry_global)
}
test_action_error <- function(f, error_message, r_fct = TRUE) {
  e <- try(get_checks(f), silent = TRUE)
  e <- attributes(e)[["condition"]]$message
  expect_equal(e, error_message)
}
test_no_action_error <- function(f, r_fct = TRUE) {
  e <- try(get_checks(f, r_fct), silent = TRUE)
  expect_false(inherits(e, "try-error"))
}

# --- type declarations -----------------------------------------------------
f <- function() {
  type(1, int)
}
test_action_error(
  f,
  "\ntype(1.0, integer)\nInvalid type declaration for: 1.0"
)
f <- function() {
  type(a, 3.14)
}
test_action_error(
  f,
  "\ntype(a, 3.14)\nFound unsupported base type: 3.14\nInvalid type declaration: 3.14 for variable a"
)
f <- function() {
  type(a, integr)
}
test_action_error(
  f,
  "\ntype(a, integr)\nFound unsupported base type: integr\nInvalid type declaration: integr for variable a"
)
f <- function() {
  type(a, vec(integr))
}
test_action_error(
  f,
  "\ntype(a, vector(integr))\nFound unsupported base type: integr\nInvalid type declaration: integr for variable a"
)
f <- function() {
  type(a, vectr(integr))
}
test_action_error(
  f,
  "\ntype(a, vectr(integr))\nFound unsupported base type: integr\nFound unsupported data structure: vectr\nInvalid type declaration: integr for variable a"
)
f <- function() {
  type(a, vectr(int))
}
test_action_error(
  f,
  "\ntype(a, vectr(integer))\nFound unsupported data structure: vectr\nInvalid type declaration: vectr for variable a"
)

# --- variable names --------------------------------------------------------
f <- function() {
  int <- 2
  a |> type(int) <- 3L
}
test_action_error(
  f,
  "\nint <- 2.0\nInvalid variable name (reserved internally) int found in int"
)
f <- function() {
  vec <- 2
  a |> type(int) <- 3L
}
test_action_error(
  f,
  "\nvec <- 2.0\nInvalid variable name (reserved internally) vec found in vec"
)
f <- function() {
  class <- 3
}
test_action_error(
  f,
  "\nclass <- 3.0\nInvalid variable name: is a C++ keyword --> class"
)

f <- function() {
  a.b <- 2
}
test_action_error(
  f,
  "\na.b <- 2.0\nInvalid variable name: contains forbidden character --> . found in a.b"
)
f <- function() {
  .a <- 2
}
test_action_error(
  f,
  "\n.a <- 2.0\nInvalid variable name: contains forbidden character --> . found in .a"
)
f <- function() {
  aSEXP <- 2
}
test_action_error(
  f,
  "\naSEXP <- 2.0\nInvalid variable name: contains forbidden character --> SEXP found in aSEXP"
)
f <- function() {
  SEXP <- 2
}
test_action_error(
  f,
  "\nSEXP <- 2.0\nInvalid variable name: contains forbidden character --> SEXP found in SEXP"
)
f <- function() {
  agetXPtr <- 2
}
test_action_error(
  f,
  "\nagetXPtr <- 2.0\nInvalid variable name: contains forbidden character --> getXPtr found in agetXPtr"
)
f <- function() {
  a_fct_ptr <- 3
}
test_action_error(
  f,
  "\na_fct_ptr <- 3.0\nInvalid variable name: contains forbidden character --> fct_ptr found in a_fct_ptr"
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
  "\nprint(m) <- 1.0\nFound invalid expression at left side of assignment: <-"
)

# --- only permitted functions ----------------------------------------------
f <- function() {
  a <- bla()
}
test_action_error(
  f,
  "\na <- bla()\nInvalid function bla"
)

f <- function() {
  return(bla())
}
test_action_error(
  f,
  "\nreturn(bla())\nInvalid function bla"
)

f <- function() {
  a %+% b
}
test_action_error(
  f,
  "\n%+%(a, b)\nInvalid function %+%"
)

f <- function() {
  a <- cmr(1, 2, 3, 4)
}
test_action_error(
  f,
  "\na <- cmr(1.0, 2.0, 3.0, 4.0)\nWrong number of arguments for: cmr"
)

# --- arity ----------------------------------------------------------------
f <- function() {
  return(1, 2)
}
test_action_error(
  f,
  "\nreturn(1.0, 2.0)\nWrong number of arguments for: return"
)

f <- function() {
  `+`(1, 2, 3)
}
test_action_error(
  f,
  "\n+(1.0, 2.0, 3.0)\nWrong number of arguments for: +"
)

f <- function() {
  a <- cmr(1, 2, 3, 4)
}
test_action_error(
  f,
  "\na <- cmr(1.0, 2.0, 3.0, 4.0)\nWrong number of arguments for: cmr"
)

# --- named args -----------------------------------------------------------
f <- function() {
  vector(mod = "integer", 2L)
}
test_action_error(
  f,
  "\nvector(\"integer\", 2L)\nFound wrong named argument for: vector"
)
f <- function() {
  vector(mode = "integer", lengt = 2L)
}
test_action_error(
  f,
  "\nvector(\"integer\", 2L)\nFound wrong named argument for: vector"
)
f <- function() {
  vector(mde = "integer", lengt = 2L)
}
test_action_error(
  f,
  "\nvector(\"integer\", 2L)\nFound wrong named argument for: vector"
)

f <- function() {
  matrix(dat = 3, nrow = 2, ncol = 2)
}
test_action_error(
  f,
  "\nmatrix(3.0, 2.0, 2.0)\nFound wrong named argument for: matrix"
)
f <- function() {
  matrix(data = 3, now = 2, ncol = 2)
}
test_action_error(
  f,
  "\nmatrix(3.0, 2.0, 2.0)\nFound wrong named argument for: matrix"
)
f <- function() {
  matrix(dta = 3, nrw = 2, ncol = 2)
}
test_action_error(
  f,
  "\nmatrix(3.0, 2.0, 2.0)\nFound wrong named argument for: matrix"
)
f <- function() {
  matrix(data = 3, nrow = 2, ncl = 2)
}
test_action_error(
  f,
  "\nmatrix(3.0, 2.0, 2.0)\nFound wrong named argument for: matrix"
)
f <- function() {
  matrix(dta = 3, nrow = 2, ncl = 2)
}
test_action_error(
  f,
  "\nmatrix(3.0, 2.0, 2.0)\nFound wrong named argument for: matrix"
)
f <- function() {
  matrix(data = 3, now = 2, ncl = 2)
}
test_action_error(
  f,
  "\nmatrix(3.0, 2.0, 2.0)\nFound wrong named argument for: matrix"
)
f <- function() {
  matrix(daa = 3, now = 2, ncl = 2)
}
test_action_error(
  f,
  "\nmatrix(3.0, 2.0, 2.0)\nFound wrong named argument for: matrix"
)

# --- break/next only inside a loop -----------------------------------------
f <- function() {
  break
}
test_action_error(
  f,
  "\nbreak;\nbreak used outside of a loop"
)
f <- function() {
  if (TRUE) {
    next
  }
}
test_action_error(
  f,
  "\nnext;\nnext used outside of a loop"
)
f <- function() {
  for (i in seq_len(3L)) {
    i <- i + 1L
  }
  break
}
test_action_error(
  f,
  "\nbreak;\nbreak used outside of a loop"
)

f <- function() {
  for (i in seq_len(3L)) {
    if (i > 1L) break
  }
}
test_no_action_error(f)
f <- function() {
  i <- 0L
  while (i < 3L) {
    i <- i + 1L
    if (i == 2L) next
  }
}
test_no_action_error(f)
f <- function() {
  repeat {
    break
  }
}
test_no_action_error(f)
# weird case: nested loops each break independently, and the inner loop's
# scope must not leak into (or be shadowed by) the outer one
f <- function() {
  for (i in seq_len(3L)) {
    for (j in seq_len(3L)) {
      if (j > 1L) break
    }
    if (i > 1L) break
  }
}
test_no_action_error(f)

# --- inner functions cannot be used as a value --------------------------
# these are caught during type inference / return handling, not run_checks,
# so drive them through the full translator
inner_fn <- quote(fn(
  args_f = function() {},
  return_value = type(double),
  block = function() {
    return(3.14)
  }
))

f <- function() {
  g <- eval(inner_fn)
  print(g)
}
body(f)[[2]][[3]] <- inner_fn
e <- try(ast2ast::translate(f), silent = TRUE)
expect_true(inherits(e, "try-error"))
expect_true(grepl(
  "Cannot use an inner function as an argument to 'print'",
  attributes(e)[["condition"]]$message, fixed = TRUE
))

f <- function() {
  g <- eval(inner_fn)
  return(g)
}
body(f)[[2]][[3]] <- inner_fn
e <- try(ast2ast::translate(f), silent = TRUE)
expect_true(inherits(e, "try-error"))
expect_true(grepl(
  "return(g)\nCannot use an inner function as an argument to 'return'",
  attributes(e)[["condition"]]$message, fixed = TRUE
))
