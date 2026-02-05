trash <- list.files("./R", full.names = TRUE) |> lapply(source)
f <- function() {
  return()
}
translate(f, getsource = TRUE, derivative = "forward") |> cat("\n")
