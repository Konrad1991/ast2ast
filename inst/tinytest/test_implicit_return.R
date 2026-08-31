library(tinytest)
library(ast2ast)

ret_type_of <- function(fct, args_fct = NULL, r_fct = FALSE, types_fct = NULL) {
  real_type <- "etr::Double"
  fr <- ast2ast:::function_registry_global$clone()
  known_types <- ast2ast:::make_known_types(types_fct, r_fct, real_type)
  env <- new.env(parent = emptyenv())
  env$r_fct <- r_fct
  env$real_type <- real_type
  env$known_types <- known_types

  AST <- ast2ast:::parse_body(ast2ast:::wrap_in_block(body(f)), env, fr)
  ast2ast:::update_function_registry(AST, fr)
  ast2ast:::run_checks(AST, r_fct, fr, known_types)
  AST <- ast2ast:::sort_args(AST, fr)
  vars <- ast2ast:::infer_types(AST, fct, args_fct, r_fct, real_type, fr, known_types)
  ast2ast:::type_checking(AST, vars, r_fct, real_type, fr, known_types)
  ast2ast:::determine_types_of_returns(AST, vars, r_fct, real_type, fr, known_types)
}
msg_of <- function(expr) {
  e <- try(expr, silent = TRUE)
  as.character(attributes(e)[["condition"]]$message)
}
is_type <- function(t, bt, ds) {
  expect_true(inherits(t, "pre_type_node"))
  expect_equal(t$get_base_type(), bt)
  expect_equal(t$get_data_struct(), ds)
}

# ==========================================================================
# 1. Bare-expression tail becomes the return value (was silently dropped)
# ==========================================================================
f <- function(x) x^2
is_type(ret_type_of(f, function(x) x |> type(double), TRUE), "double", "scalar")

f <- function(x) x
is_type(ret_type_of(f, function(x) x |> type(vec(double)), TRUE), "double", "vector")

f <- function() 5L
is_type(ret_type_of(f, function() {}, TRUE), "integer", "scalar")

f <- function(x) {
  s <- sum(x)
  s / length(x)
}
is_type(ret_type_of(f, function(x) x |> type(vec(double)), TRUE), "double", "scalar")

# end to end
f <- ast2ast::translate(function(x) { argtypes(x |> type(double)); x^2 })
expect_equal(f(3), 9)
f <- ast2ast::translate(function(x) {
  argtypes(x |> type(vec(double)))
  s <- sum(x)
  s / length(x)
})
expect_equal(f(c(1, 2, 3, 4)), 2.5)
f <- ast2ast::translate(function(x) { argtypes(x |> type(vec(double))); x })
expect_equal(
  c(f(c(7, 8, 9))),
  c(7, 8, 9)
)

# ==========================================================================
# 2. Tail if: each branch's last statement becomes a return
# ==========================================================================
f <- ast2ast::translate(function(x) { argtypes(x |> type(double)); if (x > 0) 1 else 2 })
expect_equal(f(3), 1)
expect_equal(f(-3), 2)

# nested tail if
f <- ast2ast::translate(function(x) {
  argtypes(x |> type(double))
  if (x > 0) {
    if (x > 10) 3 else 2
  } else {
    1
  }
})
expect_equal(f(20), 3)
expect_equal(f(5), 2)
expect_equal(f(-1), 1)

# else-if chain with a terminal else
f <- ast2ast::translate(function(x) {
  argtypes(x |> type(double))
  if (x == 1) 10 else if (x == 2) 20 else 30
})
expect_equal(f(1), 10)
expect_equal(f(2), 20)
expect_equal(f(9), 30)

# ==========================================================================
# 3. check_tail_missing_else
# ==========================================================================
tail_err <- "If the last statement is an if-block an else-branch is required!"

# tail if, no else (was: falls off end -> UB / core dump)
expect_error(
  ast2ast::translate(function(x) {
    argtypes(x |> type(double))
    if (x > 0) return(1)
  }),
  pattern = "else-branch is required"
)

# else-if chain, no terminal else
expect_error(
  ast2ast::translate(function(x) {
    argtypes(x |> type(double))
    if (x > 0) return(1) else if (x < 0) return(-1)
  }),
  pattern = "else-branch is required"
)

# guard clause (if NOT the last statement) must NOT be flagged
f <- ast2ast::translate(function(x) {
  argtypes(x |> type(double))
  if (x < 0) return(-1)
  return(x * 2)
})
expect_equal(f(-5), -1)
expect_equal(f(4), 8)

# guard nested inside a tail branch must NOT be flagged
f <- ast2ast::translate(function(x) {
  argtypes(x |> type(double))
  if (x > 0) {
    if (x == 1) return(99)
    return(x * 2)
  } else {
    return(0)
  }
})
expect_equal(f(1), 99)
expect_equal(f(5), 10)
expect_equal(f(-2), 0)

# ==========================================================================
# 4. Valueless tail -> NULL (r_fct) / void, no crash
# ==========================================================================
f <- function() {
  a <- 1
}
expect_equal(ret_type_of(function() {
  a <- 1
}, function() {}, TRUE), "R_NilValue")
expect_equal(ret_type_of(function() {
  a <- 1
}, function() {}, FALSE), "void")
expect_equal(ret_type_of(function() {}, function() {}, TRUE), "R_NilValue")

# trailing loop, no return anywhere (was: fall off end)
f <- ast2ast::translate(function(n) {
  argtypes(n |> type(integer))
  v <- numeric(n)
  for (i in 1:n) v[i] <- i * i
})
expect_null(f(5L))

# trailing print
f <- ast2ast::translate(function(x) {
  argtypes(x |> type(double))
  print(x)
})
expect_null(f(3))

# ==========================================================================
# 5. void + value returns: r_fct allows, XPtr rejects
# ==========================================================================
# r_fct: explicit return() alongside return(obj) is fine now
f <- ast2ast::translate(function(x) {
  argtypes(x |> type(double))
  if (x > 0) return(1) else return()
})
expect_equal(f(2), 1)
expect_null(f(-2))

# r_fct: value return + valueless (loop) tail is fine
f <- ast2ast::translate(function(x) {
  argtypes(x |> type(double))
  if (x > 0) return(x * 2)
  for (i in 1:3) print(i)
})
expect_equal(f(4), 8)
expect_null(f(-1))

# XPtr: the same mix is a translation-time error
expect_error(
  ast2ast::translate(function(x) {
    argtypes(x |> type(double))
    if (x > 0) return(1.0) else return()
  }, output = "XPtr"),
  pattern = "every path"
)
expect_error(
  ast2ast::translate(function(x) {
    argtypes(x |> type(double))
    if (x > 0) return(x * 2)
    for (i in 1:3) print(i)
  }, output = "XPtr"),
  pattern = "every path"
)

# ==========================================================================
# 6. Return-type reconciliation
#    r_fct: SEXP, never reconciled -> multiple value returns give "R_NilValue"
#    XPtr:  folded over EVERY value return (not just the last pair)
# ==========================================================================
f <- function(a) {
  if (a == 1) {
    return(matrix(1.0, 2, 2))
  }
  if (a == 2) {
    return(1.0)
  }
  return(2.0)
}
# r_fct: not reconciled
expect_equal(ret_type_of(f, function(a) a |> type(double), TRUE), "R_NilValue")
# XPtr: folds matrix + scalar + scalar -> matrix (a plain per-pair last fold
# would have collapsed to scalar)
is_type(ret_type_of(f, function(a) a |> type(double), FALSE), "double", "matrix")

# XPtr base type widens across three returns: logical, integer, double -> double
f <- function(a) {
  if (a == 1) {
    return(TRUE)
  }
  if (a == 2) {
    return(1L)
  }
  return(3.14)
}
is_type(ret_type_of(f, function(a) a |> type(double), FALSE), "double", "scalar")

# a single value return IS still reported under r_fct
f <- function(a) {
  if (a == 1) {
    return(2.5)
  }
  return(3.5)
}
expect_equal(ret_type_of(f, function(a) a |> type(double), TRUE), "R_NilValue")
f <- function(a) return(matrix(1.0, 2, 2))
is_type(ret_type_of(f, function(a) a |> type(double), TRUE), "double", "matrix")

# ==========================================================================
# 7. Inner fn (fn()) call as the tail statement
# ==========================================================================
# non-void inner fn -> its value is returned
f <- ast2ast::translate(function(x) {
  argtypes(x |> type(double))
  g <- fn(
    argtypes(),
    return(double),
    return(13)
  )
  if (x == 1) return(1.0) else {}
  g()
})
expect_equal(f(2), 13)
expect_equal(f(1), 1)

# void inner fn -> call kept for side effects, function returns NULL
f <- ast2ast::translate(function(x) {
  argtypes(x |> type(double))
  g <- fn(
    argtypes(),
    return(void),
    print("hi")
  )
  if (x == 1) return(1.0) else {}
  g()
})
expect_null(f(2))
expect_equal(f(1), 1)
