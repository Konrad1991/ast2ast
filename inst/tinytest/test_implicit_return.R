library(tinytest)
library(ast2ast)

# Skipped on CRAN: ~11 compiles. Return-type inference is exercised without
# compiling elsewhere; the compile-and-run checks run at home / CI only.
if (!at_home()) exit_file("slow: implicit-return compiles; runs at home / CI only")

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

# end to end -- one TU, the tail of each branch is a bare expression
#   1  scalar expr   2  reduction / arithmetic tail   3  bare variable
f <- ast2ast::translate(function(test, s, v) {
  argtypes(
    test |> type(int),
    s |> type(double),
    v |> type(vec(double))
  )
  if (test == 1L) {
    s^2
  } else if (test == 2L) {
    total <- sum(v)
    total / length(v)
  } else {
    v
  }
})
expect_equal(f(1L, 3, c(0.0)), 9)
expect_equal(f(2L, 0, c(1, 2, 3, 4)), 2.5)
expect_equal(c(f(3L, 0, c(7, 8, 9))), c(7, 8, 9))

# ==========================================================================
# 2. Tail if: each branch's last statement becomes a return
# One TU dispatched on `test`; the tail statement of each `test` branch is
# itself a bare tail-if, so the implicit-return machinery still has to
# return-ify it (test 2 already covers a tail-if nested in a tail-if branch).
#   1  plain tail if           2  nested tail if           3  else-if chain
# ==========================================================================
f <- ast2ast::translate(function(test, x) {
  argtypes(
    test |> type(int),
    x |> type(double)
  )
  if (test == 1L) {
    if (x > 0) 1 else 2
  } else if (test == 2L) {
    if (x > 0) {
      if (x > 10) 3 else 2
    } else {
      1
    }
  } else if (test == 3L) {
    if (x == 1) 10 else if (x == 2) 20 else 30
  } else {
    0
  }
})
# plain tail if
expect_equal(f(1L, 3), 1)
expect_equal(f(1L, -3), 2)
# nested tail if
expect_equal(f(2L, 20), 3)
expect_equal(f(2L, 5), 2)
expect_equal(f(2L, -1), 1)
# else-if chain with a terminal else
expect_equal(f(3L, 1), 10)
expect_equal(f(3L, 2), 20)
expect_equal(f(3L, 9), 30)

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

# valueless tail -> NULL, no crash
#   1  trailing loop, no return anywhere    2  trailing print
f <- ast2ast::translate(function(test, n, x) {
  argtypes(
    test |> type(int),
    n |> type(integer),
    x |> type(double)
  )
  if (test == 1L) {
    v <- numeric(n)
    for (i in 1:n) v[i] <- i * i
  } else {
    print(x)
  }
})
expect_null(f(1L, 5L, 0))
expect_null(f(2L, 0L, 3))

# ==========================================================================
# 5. void + value returns: r_fct allows, XPtr rejects
# ==========================================================================
# r_fct allows a valueless path alongside a value return
#   1  explicit return() as a tail-if else branch
#   2  value return via guard + valueless (loop) tail
f <- ast2ast::translate(function(test, x) {
  argtypes(
    test |> type(int),
    x |> type(double)
  )
  if (test == 1L) {
    if (x > 0) return(1) else return()
  } else if (test == 2L) {
    if (x > 0) return(x * 2)
    for (i in 1:3) print(i)
  } else {
    return()
  }
})
expect_equal(f(1L, 2), 1)
expect_null(f(1L, -2))
expect_equal(f(2L, 4), 8)
expect_null(f(2L, -1))

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
# inner fn() call as the tail statement
#   1  non-void inner fn -> its value is returned
#   2  void inner fn -> call kept for side effects, function returns NULL
f <- ast2ast::translate(function(test, x) {
  argtypes(
    test |> type(int),
    x |> type(double)
  )
  if (test == 1L) {
    g_val <- fn(
      argtypes(),
      return(double),
      return(13)
    )
    if (x == 1) return(1.0) else {}
    g_val()
  } else if (test == 2L) {
    g_void <- fn(
      argtypes(),
      return(void),
      print("hi")
    )
    if (x == 1) return(1.0) else {}
    g_void()
  } else {
    return()
  }
})
expect_equal(f(1L, 2), 13)
expect_equal(f(1L, 1), 1)
expect_null(f(2L, 2))
expect_equal(f(2L, 1), 1)
