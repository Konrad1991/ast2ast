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

files <- list.files("./R/", full.names = TRUE)
trash <- lapply(files, source)

create_ast_f <- function(f) {
  type_infer <- type_inference$new(f)
  ast <- process(body(f), "Start", TRUE, type_infer)
  structure(
    list(fct = f, AST = ast, type_infer = type_infer),
    class = "AST"
  )
}
graph_creation <- function(ast) {
  stopifnot(inherits(ast, "AST"))
  edges <- extract_edges(ast$type_infer$all_nodes)
  sorted_ids <- topo_sort(edges)
  structure(
    list(
      fct = ast$f, AST = ast$AST, edges = edges,
      sorted_ids = sorted_ids, all_nodes = ast$type_infer$all_nodes,
      sorted_nodes = ast$type_infer$all_nodes[sorted_ids]
    ),
    class = "graph"
  )
}
infer <- function(expression) {}
type_infer <- function(graph) {
  stopifnot(inherits(graph, "graph"))
  names <- c(all.vars(body(graph$fct)), formals(graph$fct))
  types <- setNames(lapply(names, \(x) {
    list()
  }), names)
  for (i in seq_along(graph$sorted_nodes)) {
    # call infer and append to types
  }
  types
}
ast <- create_ast_f(f8)
g <- graph_creation(ast)
g
type_infer(g)

# Idea of type inference from top to bottom in sorted graph
names <- names(type_infer$last_assignment) # a, b, and c
types <- setNames(lapply(names, \(x) {
  list()
}), names)
types[["b"]] <- c(types[["b"]], "Integer")
types[["b"]] <- c(types[["b"]], "Double")
types[["a"]] <- c(types[["a"]], "VecDouble")
types
