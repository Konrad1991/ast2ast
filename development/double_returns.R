trash <- list.files("./R", full.names = TRUE) |> lapply(source)
f <- function() {
  return()
  return(3.14)
}
translate(f, getsource = TRUE, derivative = "forward") |> cat("\n")
