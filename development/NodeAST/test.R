if (!grepl("NodeAST", getwd())) {
  setwd("./development/NodeAST")
}
files <- list.files(path = ".", full.names = TRUE)
files <- files[files != "./test.R"]
trash <- lapply(files, source)

f <- function() {
  a %type% integer <- 1
  for (i in 1:10) {
    print(i)
    next
  }
  a <- 1
  a <- 1 + c
  b %type% double
}
fcpp <- translate(f)
fcpp |> cat()
cat("\n")

f <- function() {
  for (i in bla(10)) {
    print(i)
    g <- 2
    b.b <- 1
  }
}
fcpp <- translate(f)
fcpp |> cat()
cat("\n")

f <- function() {
  print(a) %type% integer <- 1
  # "a" %type% integer <- 1
  # a %type% print(a)
  # a %type% "int"
  # if (TRUE) {
  #   print("test")
  #   b.b <- 1
  # }
}
fcpp <- translate(f)
.traceback()
cat("\n")

f <- function() {
  bb %type% inter <- 1
}
fcpp <- translate(f)
cat("\n")

fct <- function() {
  c.c <- 1
  c.c <- c %/% 2 %bla% 3.14
  if (bla()) {
    a <- 1
  } else {
    b <- 2
  }
  if (TRUE) {
    print("test")
  }
}
fcpp <- translate(fct)
