library(tinytest)

types <- function() {
  new_layout(State, double, names = c("prey", "pred"))
}

# --- read and write by name, same as by index ---------------------------------
f <- function(y) {
  argtypes(y |> type(named_vec(State)))
  y[["pred"]] <- y[["prey"]] * 2
  return(y)
}
fcpp <- ast2ast::translate(f, types_f = types)
# the names describe a prefix, longer vectors are fine;
# the returned vector carries the names, entries beyond them get ""
expect_equal(fcpp(c(3, 0, 7)), c(prey = 3, pred = 6, 7))

# single bracket with one name
f <- function(y) {
  argtypes(y |> type(named_vec(State)))
  return(y["pred"] + 1)
}
fcpp <- ast2ast::translate(f, types_f = types)
expect_equal(fcpp(c(3, 4)), 5)

# --- names from an R expression ---------------------------------------------------
state_names <- c("a", "b", "c")
types_expr <- function() {
  new_layout(Abc, double, names = state_names)
}
f <- function(y) {
  argtypes(y |> type(named_vec(Abc)))
  return(y[["c"]] - y[["a"]])
}
fcpp <- ast2ast::translate(f, types_f = types_expr)
expect_equal(fcpp(c(1, 2, 10)), 9)

# --- inner function with a named parameter -----------------------------------------
f <- function(y) {
  argtypes(y |> type(named_vec(State)))
  g <- function(s) {
    argtypes(s |> type(named_vec(State)) |> const())
    returntype(double)
    return(s[["pred"]])
  }
  return(g(y))
}
fcpp <- ast2ast::translate(f, types_f = types)
expect_equal(fcpp(c(1, 8)), 8)

# --- borrowed named vector ---------------------------------------------------------
f <- function(y) {
  argtypes(y |> type(named_borrow_vec(State)))
  return(y[["pred"]] * 3)
}
fcpp <- ast2ast::translate(f, types_f = types)
expect_equal(fcpp(c(1, 2)), 6)

# --- errors ------------------------------------------------------------------------
# typo -> suggestion
f <- function(y) {
  argtypes(y |> type(named_vec(State)))
  return(y[["pred1"]])
}
expect_error(ast2ast::translate(f, types_f = types, getsource = TRUE),
             pattern = "Unknown name 'pred1' for layout State. Did you mean 'pred'?", fixed = TRUE)

# string index on a plain vector
f <- function(y) {
  argtypes(y |> type(vec(double)))
  return(y[["pred"]])
}
expect_error(ast2ast::translate(f, getsource = TRUE),
             pattern = "only allowed on a named_vec / named_borrow_vec")

# plain vector where a named vector is required
f <- function(y) {
  argtypes(y |> type(vec(double)))
  g <- function(s) {
    argtypes(s |> type(named_vec(State)) |> const())
    returntype(double)
    return(s[["pred"]])
  }
  return(g(y))
}
expect_error(ast2ast::translate(f, types_f = types, getsource = TRUE),
             pattern = "has to be named_vec(State) or named_borrow_vec(State)", fixed = TRUE)

# undefined layout
f <- function(y) {
  argtypes(y |> type(named_vec(Nope)))
  return(y)
}
# expect_error fails as the error message is printed and then stop is called with another message
output <- capture.output(try(ast2ast::translate(f, types_f = types, getsource = TRUE), silent = TRUE))
expect_equal(output, "[1] \"named_vec(Nope): Nope is not defined with new_layout(), for variable: y\"")

# invalid definitions
f <- function(y) {
  argtypes(y |> type(vec(double)))
  return(y)
}
expect_error(ast2ast::translate(f, types_f = function() new_layout(D, double, names = c("a", "a")), getsource = TRUE),
             pattern = "duplicated names: a")
expect_error(ast2ast::translate(f, types_f = function() new_layout(D, double, names = c("a", NA)), getsource = TRUE),
             pattern = "NA or empty")
expect_error(ast2ast::translate(f, types_f = function() new_layout(D, double, names = 1:3), getsource = TRUE),
             pattern = "non-empty character vector")
expect_error(ast2ast::translate(f, types_f = function() new_layout(D, double, names = no_such_variable), getsource = TRUE),
             pattern = "evaluating names failed")

# --- R boundary --------------------------------------------------------------------
f <- function(y) {
  argtypes(y |> type(named_vec(State)))
  return(y[["pred"]])
}
fcpp <- ast2ast::translate(f, types_f = types)
expect_equal(fcpp(c(prey = 1, pred = 2)), 2)
expect_equal(fcpp(c(1, 2)), 2)  # unnamed is accepted
expect_error(fcpp(c(pred = 2, prey = 1)), pattern = "position 1 is 'pred', expected 'prey'")
expect_error(fcpp(1), pattern = "expected at least 2, got 1")

types_pair <- function() {
  new_layout(Pair, double, names = c("a", "b"))
}

# a returned borrowed vector gets the names, the caller's vector does not
f <- function(y) {
  argtypes(y |> type(named_borrow_vec(State)))
  return(y)
}
fcpp <- ast2ast::translate(f, types_f = types)
x <- c(1, 2)
expect_equal(fcpp(x), c(prey = 1, pred = 2))
expect_null(names(x))

# --- vector(mode = <layout>) ---------------------------------------------------------------
f <- function(n) {
  argtypes(n |> type(int))
  rate <- vector(mode = "State", n)
  rate[["pred"]] <- 5
  return(rate)
}
fcpp <- ast2ast::translate(f, types_f = types)
expect_equal(fcpp(3L), c(prey = 0, pred = 5, 0))

# without length: number of names
f <- function() {
  p <- vector(mode = "Pair")
  p[["a"]] <- 1
  return(p)
}
fcpp <- ast2ast::translate(f, types_f = types_pair)
expect_equal(fcpp(), c(a = 1, b = 0))

# a declaration allocates the number of names (zeros)
f <- function(p) {
  argtypes(p |> type(named_vec(Pair)))
  returntype(named_vec(Pair))
  res |> type(named_vec(Pair))
  res[["a"]] <- p[["a"]] + p[["b"]]
  res[2] <- p[["a"]] * p[["b"]]
  res
}
fcpp <- ast2ast::translate(f, types_f = types_pair)
expect_equal(fcpp(c(a = 1, b = 2)), c(a = 3, b = 2))

f <- function() {
  res |> type(named_vec(Pair))
  return(res)
}
fcpp <- ast2ast::translate(f, types_f = types_pair)
expect_equal(fcpp(), c(a = 0, b = 0))
