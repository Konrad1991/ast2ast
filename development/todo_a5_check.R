library(ast2ast)

check <- function(label, expr) {
  res <- tryCatch(expr, error = function(e) paste("error:", substr(conditionMessage(e), 1, 300)))
  cat(label, ": ", paste(format(res), collapse = " "), "\n", sep = "")
}

# inner fn g -> struct f_impl::g_t; type named g_t
types_g_t <- function() new_type(g_t, slots(x |> type(double)))
f1 <- function(a, p) {
  argtypes(a |> type(double), p |> type(g_t))
  g <- function(a) { argtypes(a |> type(double)); returntype(double); a }
  g(a) + p$x
}
check("type g_t + fn g", translate(f1, types_f = types_g_t)(1, structure(list(x = 1), class = "g_t")))

# inner fn g -> body in g_namespace; type named g_namespace
types_g_ns <- function() new_type(g_namespace, slots(x |> type(double)))
f2 <- function(a, p) {
  argtypes(a |> type(double), p |> type(g_namespace))
  g <- function(a) { argtypes(a |> type(double)); returntype(double); a }
  g(a) + p$x
}
check("type g_namespace + fn g", translate(f2, types_f = types_g_ns)(1, structure(list(x = 1), class = "g_namespace")))

# outer fn f3 -> namespace f3_impl; type named f3_impl
types_f3_impl <- function() new_type(f3_impl, slots(x |> type(double)))
f3 <- function(a, p) {
  argtypes(a |> type(double), p |> type(f3_impl))
  a + p$x
}
check("type f3_impl + outer f3", translate(f3, types_f = types_f3_impl)(1, structure(list(x = 1), class = "f3_impl")))

# inner fns g and g_namespace
f4 <- function(a) {
  argtypes(a |> type(double))
  g <- function(a) { argtypes(a |> type(double)); returntype(double); a }
  g_namespace <- function(a) { argtypes(a |> type(double)); returntype(double); a }
  g(a) + g_namespace(a)
}
check("fn g + fn g_namespace", translate(f4)(1))

# inner fn named like the outer namespace
f5 <- function(a) {
  argtypes(a |> type(double))
  f5_impl <- function(a) { argtypes(a |> type(double)); returntype(double); a }
  f5_impl(a)
}
check("inner fn f5_impl in f5", translate(f5)(1))
