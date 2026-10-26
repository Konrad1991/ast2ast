library(ast2ast)

check <- function(label, expr) {
  res <- tryCatch(expr, warning = function(w) paste("warning:", conditionMessage(w)),
                  error = function(e) paste("error:", conditionMessage(e)))
  cat(label, ": ", paste(format(res), collapse = " "), "\n", sep = "")
}

# A1: R gives NA + warning
f1 <- function(a) {
  argtypes(a |> type(int))
  a + 2000000000L
}
check("A1 overflow (R: NA + warning)", translate(f1)(2000000000L))

# A2: R gives 2
f2 <- function(x) {
  argtypes(x |> type(vec(double)))
  returntype(int)
  n <- 0L
  for (i in seq_len(length(x))) if (is.na(x[[i]])) n <- n + 1L
  n
}
check("A2 is.na(NaN) (R: 2)", translate(f2)(c(1, NA, NaN)))

# A3: R gives 0 1 1
f3 <- function(x, b) {
  argtypes(x |> type(vec(double)), b |> type(vec(logical)))
  returntype(vec(double))
  r <- numeric(3L)
  r[1L] <- as.numeric(b[[1L]] & FALSE)
  r[2L] <- as.numeric(b[[1L]] | TRUE)
  r[3L] <- x[[1L]]^0
  r
}
check("A3 NA & / | / ^0 (R: 0 1 1)", translate(f3)(c(NA_real_, 1), c(NA, TRUE)))

# A4: message should say no element matches
f4 <- function(n) {
  argtypes(n |> type(int))
  x <- numeric(n)
  x[x > 0]
}
check("A4 empty subset", translate(f4)(3L))

# A5: should be rejected at translation time
f5 <- function(a) {
  argtypes(a |> type(double))
  g <- function(a) { argtypes(a |> type(double)); returntype(double); a }
  g_t <- function(a) { argtypes(a |> type(double)); returntype(double); a }
  g(a) + g_t(a)
}
check("A5 reserved suffix", translate(f5)(1))
