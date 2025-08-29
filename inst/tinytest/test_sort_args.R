library(tinytest)
library(ast2ast)

get_sorted_ast <- function(f, r_fct = TRUE) {
  ast <- ast2ast:::parse_body(body(f), r_fct)
  ast2ast:::sort_args(ast)
}

# --- vector sorting ----------------------------------------------------
f <- function() {
  v1 <- vector(mode = "logical", length = 2)
  v2 <- vector("integer", 3)
  v3 <- vector("numeric", length = 4)
  v4 <- vector(mode = "numeric", 5)
  v5 <- vector(length = 4, mode = "logical")
}
ast <- get_sorted_ast(f)
res <- lapply(ast$block, function(node) {
  node$right_node$stringify("")
})
expect_equal(res[[1]], "vector(\"logical\", 2)")
expect_equal(res[[2]], "vector(\"integer\", 3)")
expect_equal(res[[3]], "vector(\"numeric\", 4)")
expect_equal(res[[4]], "vector(\"numeric\", 5)")
expect_equal(res[[5]], "vector(\"logical\", 4)")


# --- matrix sorting ----------------------------------------------------
f <- function() {
  m1 <- matrix(1, 1, 1)
  m2 <- matrix(data = 2, 2, 2)
  m3 <- matrix(3, nrow = 3, 3)
  m4 <- matrix(data = 4, nrow = 4, 4)
  m5 <- matrix(5, 5, ncol = 5)
  m6 <- matrix(data = 6, 6, ncol = 6)
  m7 <- matrix(7, nrow = 7, ncol = 7)
  m8 <- matrix(data = 8, nrow = 8, ncol = 8)
}
ast <- get_sorted_ast(f)
res <- lapply(ast$block, function(node) {
  node$right_node$stringify("")
})
checks <- logical(8)
for (i in 1:8) {
  idx <- paste0(i, ".0")
  expected <- sprintf("matrix(%s, %s, %s)", idx, idx, idx)
  checks[i] <- expected == res[[i]]
}
expect_true(all(checks))
