files <- list.files("~/Documents/ast2ast/R/", full.names = TRUE)
trash <- lapply(files, source)
library(tinytest)

# --- Checks for translate_internally ---------------------------------------
f <- function() print("Hello world")
expect_error(
  translate_internally(f, NULL, "f", TRUE)
)
f <- function() {}
expect_error(
  translate_internally(f, NULL, "f", TRUE)
)

# --- helpers ---------------------------------------------------------------
ast_of <- function(f, r_fct = TRUE) {
  parse_body(body(f), r_fct)
}

expect_node <- function(node, cls, operator = NULL, context = NULL) {
  checks <- logical(3)
  checks[1] <- inherits(node, cls)
  if (!is.null(operator) && !is.null(node$operator)) {
    checks[2] <- node$operator == operator
  } else {
    checks[2] <- TRUE
  }
  if (!is.null(context) && !is.null(node$context)) {
    checks[3] <- node$context == context
  } else {
    checks[3] <- TRUE
  }
  expect_true(all(checks))
}

expect_block_len <- function(block_node, n) {
  expect_true(inherits(block_node, "block_node"))
  expect_equal(length(block_node$block), n)
}

gather <- function(AST) {
  env <- new.env(parent = emptyenv())
  env$functions <- c()
  env$variables <- c()
  env$literals <- c()
  gatherer <- function(node, env) {
    if (!is.null(node$operator)) {
      env$functions <- c(env$functions, node$operator)
    }
    if (inherits(node, "literal_node")) {
      env$literals <- c(env$literals, node$name)
    }
    if (inherits(node, "variable_node")) {
      env$variables <- c(env$variables, node$name)
    }
  }
  traverse_ast(AST, gatherer, env)
  return(list(env$functions, env$variables, env$literals))
}
expected_to_find <- function(AST, fcts_expected, literals_expected, vars_expected) {
  res <- gather(AST)
  fcts <- res[[1]]
  vars <- res[[2]]
  literals <- res[[3]]
  checks <- logical(6)
  checks[1] <- length(fcts_expected) == length(fcts)
  checks[2] <- length(literals_expected) == length(literals)
  checks[3] <- length(vars_expected) == length(vars)
  checks[4] <- all(fcts_expected %in% fcts)
  checks[5] <- all(literals_expected %in% literals)
  checks[6] <- all(vars_expected %in% vars)
  expect_true(all(checks))
}

# --- smoke: empty & simple bodies -----------------------------------------
f <- function() { a <- 1 }
AST <- ast_of(f)
expect_node(AST, "block_node", context = "Start")
expect_block_len(AST, 1)
expected_to_find(AST, c("<-"), 1, "a")

f <- function() { a <- 1L }
AST <- ast_of(f)
stmt <- AST$block[[1]]
expect_node(stmt, "binary_node", operator = "<-", context = "{")
expect_true(inherits(stmt$left_node, "variable_node"))
expect_true(inherits(stmt$right_node, "literal_node"))
expected_to_find(AST, "<-", "1L", "a")

# --- unary / binary / function nodes --------------------------------------
f <- function() { b <- -a }
AST <- ast_of(f)
assign <- AST$block[[1]]
expect_node(assign, "binary_node", operator = "<-")
expect_true(inherits(assign$right_node, "unary_node"))
expect_equal(assign$right_node$operator, "-")
expected_to_find(AST, c("<-", "-"), character(), c("a", "b"))

f <- function() { z <- x + y * 2 }
AST <- ast_of(f)
assign <- AST$block[[1]]
rhs <- assign$right_node
expect_true(inherits(rhs, "binary_node"))
expect_true(rhs$operator %in% c("+","*"))
expected_to_find(AST, c("<-", "+", "*"), 2, c("x", "y", "z"))

f <- function() { v <- c(1, 2, 3) }
AST <- ast_of(f)
rhs <- AST$block[[1]]$right_node
expect_node(rhs, "function_node", operator = "c")
expect_length(rhs$args, 3)
expected_to_find(AST, c("<-", "c"), c(1, 2, 3), c("v"))

# --- subsetting: vector vs matrix forms -----------------------------------
f1 <- function() { y <- x[1] }
AST <- ast_of(f1)
expect_true(inherits(AST$block[[1]]$right_node, "binary_node"))
expect_equal(AST$block[[1]]$right_node$operator, "[")
expected_to_find(AST, c("<-", "["), c(1), c("x", "y"))

f2 <- function() { m <- M[1, 2] }
AST <- ast_of(f2)
expect_true(inherits(AST$block[[1]]$right_node, "function_node"))
expect_equal(AST$block[[1]]$right_node$operator, "[")
expect_length(AST$block[[1]]$right_node$args, 3) # target + two indices
expected_to_find(AST, c("<-", "["), c(1, 2), c("m", "M"))

f <- function() { s <- x[[1]]; t <- at(x, 1) }
AST <- ast_of(f)
s_rhs <- AST$block[[1]]$right_node
t_rhs <- AST$block[[2]]$right_node
expect_true(inherits(s_rhs, "binary_node"))
expect_equal(s_rhs$operator, "[[")
expect_true(inherits(t_rhs, "binary_node"))
expect_equal(t_rhs$operator, "at")
expected_to_find(AST, c("<-", "[[", "at", "<-"), c(1, 1), c("s", "x", "t", "x"))

# --- control flow nodes ----------------------------------------------------
f <- function() {
  if (a > 0) { x <- 1
  } else if (a == 0) { x <- 0
  } else { x <- -1 }
}
AST <- ast_of(f)
stmt <- AST$block[[1]]
expect_true(inherits(stmt, "if_node"))
expect_true(inherits(stmt$condition, "binary_node"))
expect_true(inherits(stmt$true_node, "block_node"))
expect_true(inherits(stmt$false_node, "block_node"))
expect_true(length(stmt$else_if_nodes) >= 1)
expected_to_find(AST, c(">", "<-", "==", "<-", "<-", "-"),
  c(0, 1, 0, 0, 1), c("a", "x", "a", "x", "x"))

f <- function() { for (i in 1:3) { a <- i } }
AST <- ast_of(f)
stmt <- AST$block[[1]]
expect_true(inherits(stmt, "for_node"))
expect_true(inherits(stmt$i, "variable_node"))
expect_true(inherits(stmt$seq, "binary_node"))
expect_true(inherits(stmt$block, "block_node"))
expected_to_find(AST, c(":", "<-"), c(1, 3), c("a", "i", "i"))

f <- function() {
  while (cond) { next }
  repeat { break }
}
AST <- ast_of(f)
expect_true(inherits(AST$block[[1]], "while_node"))
expect_true(inherits(AST$block[[2]], "repeat_node"))
expected_to_find(AST, c("next", "break"), character(), "cond")

# --- type declaration surface ---------------------------------------------
f <- function() { type(a, vec(double)) }
AST <- ast_of(f)
stmt <- AST$block[[1]]
expect_true(inherits(stmt, "binary_node"))
expect_equal(stmt$operator, "type")
expect_true(inherits(stmt$left_node, "variable_node"))
expect_true(inherits(stmt$right_node, "type_node"))
expected_to_find(AST, c("type"), character(), c("a"))

# --- logical & comparisons -------------------------------------------------
f <- function() { ok <- (x > 0) && (y <= 1) || z }
AST <- ast_of(f)
rhs <- AST$block[[1]]$right_node
expect_true(inherits(rhs, "binary_node"))
expected_to_find(AST, c("<-", "||", "&&", "(", ">", "(", "<="), c(0, 1), c("ok", "x", "y", "z"))

# --- stringify sanity (no errors, basic invariants) -----------------------
f <- function() { a <- 1; if (a > 0) { a <- a + 1 } }
AST <- ast_of(f)
expect_true(inherits(AST, "block_node"))
expected_to_find(AST, c("<-", ">", "<-", "+"), c(1, 0, 1), c("a", "a", "a", "a"))

# --- error-line stringification is empty right after parsing --------------
f <- function() { a <- 1 }
AST <- ast_of(f)
expect_equal(AST$stringify_error_line(), "")
