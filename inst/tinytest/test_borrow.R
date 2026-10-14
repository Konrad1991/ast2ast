library(tinytest)

# --- passing a borrowed vector where an owned vector is required -----------
f <- function(v) {
  argtypes(
    v |> type(borrow_vec(double))
  )
  g <- fn(
    argtypes(
      x |> type(vec(double)) |> const()
    ),
    return(vec(double)),
    {
      return(x)
    }
  )
  return(g(v))
}
expect_error(
  ast2ast::translate(f),
  pattern = "Found borrow_vector but vector is required"
)

# --- happy path: a borrowed argument materialized into an owned return -----
# TODO: hopefully we can bundle this compilation together with other tests
f <- function(v) {
  argtypes(
    v |> type(borrow_vec(double))
  )
  g <- fn(
    argtypes(
      x |> type(borrow_vec(double)) |> const()
    ),
    return(vec(double)),
    {
      return(x)
    }
  )
  return(g(v))
}
fcpp <- ast2ast::translate(f)
v <- c(1.1, 2.2, 3.3)
expect_equal(fcpp(v) |> c(), v)

# --- a return cannot itself be declared borrow_vec --------------------
f <- function(v) {
  argtypes(
    v |> type(borrow_vec(double))
  )
  g <- fn(
    argtypes(
      x |> type(borrow_vec(double)) |> const()
    ),
    return(borrow_vec(double)),
    {
      return(x)
    }
  )
  return(g(v))
}
expect_error(
  ast2ast::translate(f),
  pattern = "borrow types only allowed in function inputs"
)

# --- borrow_vec cannot be combined with automatic differentiation ----------
f <- function(a) {
  argtypes(
    a |> type(borrow_vec(double))
  )
  a[[1L]] <- a[[1L]] * a[[1L]]
  return(a)
}
# create_vars_types_list() print()s the specific message and only throws the
# generic "Types for arguments are invalid" as the condition itself.
check_borrow_ad_rejected <- function(derivative, output = "R", info = "") {
  e <- capture.output(
    error <- try(
      ast2ast::translate(f, derivative = derivative, output = output, getsource = TRUE),
      silent = TRUE
    )
  )
  expect_true(inherits(error, "try-error"), info = info)
  expect_true(
    any(
      grepl("borrow types cannot be used together with automatic differentiation",
        e)
    ),
    info = info
  )
}

# 1. forward mode
check_borrow_ad_rejected("forward", info = "forward")

# 2. reverse mode
check_borrow_ad_rejected("reverse", info = "reverse")

# 3. reverse mode is also rejected for XPtr
check_borrow_ad_rejected("reverse", "XPtr", info = "reverse XPtr")

# --- forward mode + XPtr: Dual memory can be borrowed ----------------------
code <- ast2ast::translate(f, derivative = "forward", output = "XPtr", getsource = TRUE)
expect_true(grepl("using ETR_REAL = etr::Dual;", code, fixed = TRUE))
expect_true(grepl("etr::Array<ETR_REAL, etr::Borrow<ETR_REAL>>", code, fixed = TRUE))

# inner fn with a borrowed Dual argument
f <- function(a) {
  argtypes(
    a |> type(borrow_vec(double))
  )
  g <- fn(
    argtypes(
      x |> type(borrow_vec(double)) |> const()
    ),
    return(vec(double)),
    {
      return(x * x)
    }
  )
  return(g(a))
}
code <- ast2ast::translate(f, derivative = "forward", output = "XPtr", getsource = TRUE)
expect_true(grepl("etr::Borrow<ETR_REAL>", code, fixed = TRUE))
# Borrow<Dual> has to compile, not only translate
fcpp <- ast2ast::translate(f, derivative = "forward", output = "XPtr")
expect_true(inherits(fcpp, "XPtr"))

# --- NA in borrowed R memory: R's sentinel value, no separate NA flags ------
# is.na() missed every NA and a written NA arrived as NaN
borrow_na <- function(M, v, x) {
  argtypes(
    M |> type(borrow_mat(double)) |> const(),
    v |> type(borrow_vec(int)) |> const(),
    x |> type(borrow_vec(double))
  )
  returntype(int)
  n <- 0L
  for (i in seq_len(length(M))) if (is.na(M[[i]])) n <- n + 1L
  for (i in seq_len(length(v))) if (is.na(v[[i]])) n <- n + 1L
  x[[2L]] <- NA
  return(n)
}
fcpp <- ast2ast::translate(borrow_na)
x <- c(1, 2, 3)
expect_equal(fcpp(matrix(c(1, NA, 3, NA), 2L, 2L), c(1L, NA, 3L), x), 3L)
expect_true(is.na(x[[2L]]) && !is.nan(x[[2L]]))
