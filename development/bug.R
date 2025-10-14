files <- list.files("./R", full.names = TRUE)
trash <- lapply(files, source)

ode <- function(t, y, ydot, parameter) {
  a <- parameter[[1]]
  b <- parameter[[2]]
  c <- parameter[[3]]
  d <- parameter[[4]]
  predator <- y[[1]]
  prey <- y[[2]]
  ydot[1] <- predator * prey * c - predator * d
  # ydot[2] <- prey * a - prey * predator * b
}
fct_input <- function(t, y, ydot, parameter) {
  t |> type(double) |> ref()
  y |> type(borrow_vec(double)) |> ref()
  ydot |> type(borrow_vec(double)) |> ref()
  parameter |> type(borrow_vec(double)) |> ref()
}
res <- translate(
  ode, fct_input, output = "XPtr",
  verbose = TRUE, getsource = TRUE)
cat(res)
