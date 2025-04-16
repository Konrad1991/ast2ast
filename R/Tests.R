if (!grepl("R", getwd())) {
  setwd("./R/")
}
files <- list.files()
files <- files[files != "Tests.R"]
files <- sapply(files, function(x) paste0(getwd(), "/", x))
trash <- lapply(files, source)

fct <- function() {
  for (i in 1:10) {
    print(i)
    if (i == 1) {
      print("bla")
    }
  }
}
args_f <- function() {}
fcpp <- translate(
  fct,
  args_f,
  output = "R"
)
cat(fcpp, "\n")

fct <- function(a, c) {
  a <- 1
  b |> type(d) <- a + c
  return()
}
args_f <- function() {
  a
  c
}
result <- translate(
  fct,
  args_f,
  output = "XPtr"
)
.traceback()
cat(result, "\n")

fct <- function(a, b, c) {
  a <- 1
  b <- 2
  c <- 3
  d |> type(integer)
  e |> type(dv)
  d <- 1L
  return()
}
args_f <- function() {
  a |> type(dv) |> ref()
  b |> type(d)
  c
}
result <- translate(
  fct,
  args_f,
  output = "R"
)
cat(result)

fct <- function() {
  a <- c(1, 2, 3)
  a[b] <- c
}
args_f <- function() {
  a
}
fcpp <- translate(f = fct, args_f)

