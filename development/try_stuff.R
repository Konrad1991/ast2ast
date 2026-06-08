files <- list.files("./R", full.names = TRUE)
trash <- lapply(files, source)
f <- function() {
  a <- vector("numeric", 3L)
  b <- numeric(1L)
}
translate(f, getsource = TRUE, derivative = "reverse") |> cat("\n")
