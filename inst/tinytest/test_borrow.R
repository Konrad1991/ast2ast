library(tinytest)

# --- passing a borrowed vector where an owned vector is required -----------

f <- function(v) {
  argtypes(v |> type(borrow_vec(double)))
  g <- fn(
    argtypes(x |> type(vec(double)) |> const()),
    return(vec(double)),
    { return(x) }
  )
  return(g(v))
}
expect_error(
  ast2ast::translate(f),
  pattern = "Found borrow_vector but vector is required"
)

# --- happy path: a borrowed argument materialized into an owned return -----

f <- function(v) {
  argtypes(v |> type(borrow_vec(double)))
  g <- fn(
    argtypes(x |> type(borrow_vec(double)) |> const()),
    return(vec(double)),
    { return(x) }
  )
  return(g(v))
}
fcpp <- ast2ast::translate(f)
v <- c(1.1, 2.2, 3.3)
expect_equal(fcpp(v) |> c(), v)

# --- a return cannot itself be declared borrow_vec --------------------

f <- function(v) {
  argtypes(v |> type(borrow_vec(double)))
  g <- fn(
    argtypes(x |> type(borrow_vec(double)) |> const()),
    return(borrow_vec(double)),
    { return(x) }
  )
  return(g(v))
}
expect_error(
  ast2ast::translate(f),
  pattern = "borrow types only allowed in function inputs"
)

# --- borrow_vec cannot be combined with automatic differentiation ----------

f <- function(a) {
  argtypes(a |> type(borrow_vec(double)))
  a[[1L]] <- a[[1L]] * a[[1L]]
  return(a)
}
# create_vars_types_list() print()s the specific message and only throws the
# generic "Types for arguments are invalid" as the condition itself.
check_borrow_ad_rejected <- function(derivative, info = "") {
  out <- capture.output(
    e <- tryCatch(
      ast2ast::translate(f, derivative = derivative),
      error = function(e) e
    )
  )
  expect_true(inherits(e, "error"), info = info)
  expect_equal(conditionMessage(e), "Types for arguments are invalid", info = info)
  expect_true(
    any(grepl("borrow types cannot be used together with automatic differentiation", out)),
    info = info
  )
}

# 1. forward mode
check_borrow_ad_rejected("forward", "forward")

# 2. reverse mode
check_borrow_ad_rejected("reverse", "reverse")
