files <- list.files("~/Documents/ast2ast/R/", full.names = TRUE)
trash <- lapply(files, source)

f1 <- function() {
  a <- 1
  if (a == 1) {
    brak()
  }
}

fcpp <- translate(f1, verbose = TRUE)
fcpp()
.traceback()
