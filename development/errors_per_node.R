trash <- list.files("./R", full.names = TRUE) |> lapply(source)
f <- function(a, b) {
  if (a) {
    return(b)
  } else if(!b) {
    for (i in 1:10) print(i)
    return(3.24)
  } else {
    if (a) {
      if (b) {
        print(TRUE)
      } else {
        print(FALSE)
      }
    }
    print("blub")
  }
}
translate(f, getsource = TRUE) |> cat("\n")

f <- function(a, b) {
  braek # Nullary
  invalid() # Nullary
  invalid(a) # Unary
  bla(a, b) # Binary
  blub(a,b, TRUE) # Function
  if (invalid(a)) print(b)
  if (one(a)) print(two()) else if (three()) print(four())
  for (i in invalid()) print(i)
  for (i in b) print(invalid())
  while(invalid()) print("bla")
  while(TRUE) print(invalid())
  repeat {print(invalid())}
}
translate(f, getsource = TRUE) |> cat("\n")
