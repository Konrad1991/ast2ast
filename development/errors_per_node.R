trash <- list.files("./R", full.names = TRUE) |> lapply(source)
f <- function() {
  type(1, int)
}
translate(f, derivative = "reverse", getsource = TRUE) |> cat()
.traceback()
# binary node error.
# -------------------------------------------------------
# * works. But instead of bla(a, b)
#   a bla b is shown
f <- function(a, b) a[NA]
f <- function(a, b) a[b]
translate(f, getsource = TRUE) |> cat("\n")

# Nullary node error
# -------------------------------------------------------
# * works.
f <- function() {
  braek
  for (i in 1:10) braek
}
translate(f, getsource = TRUE) |> cat("\n")

# Unary node error.
# -------------------------------------------------------
# * works.
f <- function() invalid()
translate(f, getsource = TRUE) |> cat("\n")

# binary node error.
# -------------------------------------------------------
# * works. But instead of bla(a, b)
#   a bla b is shown
f <- function(a, b) bla(a, b)
translate(f, getsource = TRUE) |> cat("\n")

# function node
# -------------------------------------------------------
# works!
f <- function(a, b, c) bla(a, b, c)
translate(f, getsource = TRUE) |> cat("\n")
# * instead of a[b, c, NA] the line is represented as [(a, b, c, NA)
f <- function(a, b, c) a[b, c, NA]
translate(f, getsource = TRUE) |> cat("\n")

# if node
# -------------------------------------------------------
# works. But two empty lines are below the error message
f <- function(a, b) if (invalid(a)) print(b)
translate(f, getsource = TRUE) |> cat("\n")
# works!
f <- function(a, b) if (a) invalid(b)
translate(f, getsource = TRUE) |> cat("\n")

# for node
# -------------------------------------------------------
# works. But no empty line is below the error message.
f <- function(b) for (i in invalid()) print(i)
translate(f, getsource = TRUE) |> cat("\n")
# works. But no empty line is below the error message.
f <- function(b) for (i in b) print(invalid())
translate(f, getsource = TRUE) |> cat("\n")

# while node
# -------------------------------------------------------
# works. But no empty line is below the error message.
f <- function(b) while(invalid()) print("bla")
translate(f, getsource = TRUE) |> cat("\n")
# works. But no empty line is below the error message.
f <- function(b) while(TRUE) print(invalid())
translate(f, getsource = TRUE) |> cat("\n")

# repeat node
# -------------------------------------------------------
# works. But no empty line is below the error message.
f <- function(b) repeat {print(invalid())}
translate(f, getsource = TRUE) |> cat("\n")
