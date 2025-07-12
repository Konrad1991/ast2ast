extract_edges <- function(all_nodes) {
  edges <- list()
  for (node in all_nodes) {
    if (inherits(node, "binary_node") && node$operator %in% c("<-", "=")) {
      env_rhs <- new.env(parent = emptyenv())
      env_rhs$vars <- list()
      traverse_ast(node$right_node, get_variables_action, env_rhs)
      rhs_vars <- env_rhs$vars
      if (is.null(rhs_vars) || identical(rhs_vars, list())) {
        edges[[length(edges) + 1]] <- c(2, node$id)
      }
      for (rhs in rhs_vars) {
        from <- rhs$last_assignment
        to <- node$id
        edges[[length(edges) + 1]] <- c(from, to)

        from <- rhs$id
        to <- node$id
        edges[[length(edges) + 1]] <- c(from, to)
      }
    } else if (inherits(node, "variable_node")) {
      from <- node$last_assignment
      to <- node$id
      edges[[length(edges) + 1]] <- c(from, to)
    } else if (inherits(node, "merge_node")) {
      for (i in node$last_assignment_before_if) {
        from <- i
        to <- node$id
        edges[[length(edges) + 1]] <- c(from, to)
      }
    }
  }
  if (length(edges) == 0) {
    return(matrix(nrow = 0, ncol = 2))
  }
  edges_mat <- do.call(rbind, edges)
  colnames(edges_mat) <- c("from", "to")
  return(edges_mat)
}

topo_sort <- function(edges) {
  nodes <- unique(c(edges[, "from"], edges[, "to"]))
  incoming <- setNames(rep(0, length(nodes)), nodes)
  # Count incoming edges for each node
  for (to in edges[, "to"]) {
    incoming[as.character(to)] <- incoming[as.character(to)] + 1
  }
  # Start with nodes with no incoming edges
  queue <- as.integer(names(incoming)[incoming == 0])
  result <- integer(0)
  while (length(queue) > 0) {
    n <- queue[1]
    queue <- queue[-1]
    result <- c(result, n)
    # Remove outgoing edges
    for (i in which(edges[, "from"] == n)) {
      m <- edges[i, "to"]
      incoming[as.character(m)] <- incoming[as.character(m)] - 1
      if (incoming[as.character(m)] == 0) {
        queue <- c(queue, m)
      }
    }
  }
  if (length(result) != length(nodes)) {
    stop("Cycle detected in graph")
  }
  return(result)
}

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


# g <- graph_creation(ast)
# g
# type_infer(g)
#
# # Idea of type inference from top to bottom in sorted graph
# names <- names(type_infer$last_assignment) # a, b, and c
# types <- setNames(lapply(names, \(x) {
#   list()
# }), names)
# types[["b"]] <- c(types[["b"]], "Integer")
# types[["b"]] <- c(types[["b"]], "Double")
# types[["a"]] <- c(types[["a"]], "VecDouble")
# types

