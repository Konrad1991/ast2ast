files <- list.files("~/Documents/ast2ast/R/", full.names = TRUE)
trash <- lapply(files, source)
f <- function() {
  a <- -b
  a <- bla1() + foo()
  a <- bla2() + 1
  a <- 1
  for (iSEXP in bla3()) {
    print(i)
    print(bla4(4))
  }
  if (a == bnla5()) {
    print(a)
  } else if (aSEXP) {
    a <- 3
    aSEXP <- 112
  } else {
    blub()
  }
  a |> type(doubleass)
}
f <- function() {
  # a <- 1
  # a <- c(1, 2)
  a <- TRUE
  b <- matrix(0, 2, 2)
}
translate(f)
.traceback()
