# PLAN: Move the project towards a static R
# Add Matrix as own typ: matrix, m, matrix_borrow, mb, matrix_borrow_sexp, mbs
# Matrix can only store double values in R!
# Maybe use directly an R-array instead of a matrix

# NOTE: Type definitions within the function
# variable_name %type% type. Here type is one of the permitted types
# permitted types are: l, i, d, lv, iv and dv and their full forms
# Additionally for the function signature for XPtr: lb, ib, db


# IDEA: Design interface for the user
# f = function
# output = "R" or "XPtr"
# verbose = TRUE or FALSE
# getsource = TRUE or FALSE
# arguments:
# XPtr: name %info% c(constentness, type, how to handle)
# R: name %info% c(type, how to handle)

translate <- function(f, args) {
  body <- NULL
  sym <- substitute(f)

  if (is.language(sym) && !is.symbol(sym)) {
    # Body is directly defined
    body <- as.list(sym)[-1]
    body <- lapply(body, function(x) {
      attributes(x) <- NULL
      x
    })
  } else {
    # Function is passed as argument
    env <- NULL
    if (is.symbol(sym)) {
      env <- parent.frame()
      while (!exists(as.character(sym), envir = env, inherits = FALSE)) {
        env <- parent.env(env)
        if (identical(env, emptyenv())) {
          stop("function not found in any environment")
        }
      }
    }
    if (is.function(f)) {
      env <- environment(f)
      if (!is.null(attr(f, ".Environment"))) {
        env <- attr(f, ".Environment")
      }
      body <- env[[as.character(sym)]] |> body()
      if (body[[1]] == "{") {
        body <- body[-1]
      }
    } else {
      stop("f is not a function")
    }
    body <- as.list(body)
  }
  args <- substitute(args)
  args <- as.list(args)[-1]

  return(
    list(
      args = args,
      body = body
    )
  )
}

fct <- function(a, b, c) {
  a <- c(1, 2, 3)
  a[b] <- c
}

res <- translate(
  f = fct,
  args = c(
    a |> type(double_vector) |> copy(),
    b |> type(logical) |> reference() |> const(),
    c |> type(double_vector) |> borrow()
  )
)
str(res)

res <- translate(
  f = {
    a <- c(1, 2, 3)
    a[b] <- c
  },
  args = c(
    a %info% c(double_vector, copy),
    b %info% c(logical, reference),
    c %info% c(double_vector, borrow)
  )
)
str(res)
