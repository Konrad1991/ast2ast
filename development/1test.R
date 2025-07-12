# Test case 1: simple scalar assignments
f1 <- function() {
  x <- 1L
  y <- 2.0
  z <- x + y
}
# Test case 2: reassignment (SSA)
f2 <- function() {
  a <- 1L
  a <- 2.5
  b <- a + 1
}
# Test case 3: function argument with operation
f3 <- function(x) {
  y <- x + 1
}
# Test case 4: undefined variable (error)
f4 <- function() {
  z <- x + 1
}
# Test case 5: if-branch merging
f5 <- function(a) {
  x <- 1
  if (a > 0) {
    x <- 2L
  }
  y <- x + 1
}
# Test case 6: argument not used
f6 <- function(arg) {
  a <- 1
}
# Test case 7: branching without overwrite
f7 <- function(a) {
  x <- 1L
  if (a > 0) {
    y <- x + 1L
  }
  z <- x + 2L
}
# Test case 8: if in if
f8 <- function(a, b) {
  x <- 1L
  if (a > 0) {
    if (b < 1) {
      y <- x + 1L
    } else if (b == 1) {
      y <- x
    } else {
      y <- x + 2L
    }
  }
  z <- x + 2L
}

files <- list.files("~/Documents/ast2ast/R/", full.names = TRUE)
trash <- lapply(files, source)
library(rlang) # for %||%
ast <- create_ast_f(f7)
graph_creation(ast)

