# declare
# -----------------------------------------
# declare is a noopt in R itself
# quickr is using it as well
f <- function(x = declare(vec(double)), y = declare(int)) {
  # returntype(vec(int))
  x + y
}
formals(f)
# > formals(f)
# $x
# declare(vec(double))
# $y
# declare(int)
f(1, 2)

f <- function(
  x = vec(int) |> ref() |> const(),
  y = int, z = borrow_array(double),
  p = Point, cp = collection(Point)
) {
  declare(returntype(int))
  x + y
}
formals(f)
f(1, 2)
