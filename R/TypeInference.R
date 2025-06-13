# Gather assignments & merge points
# env$last_assignment contains all variables!
# Thus, if an if_node is encountered one can loop over all names of the list
# 1. Check whether the current variable is found in:
#   - true-node, false node or if else nodes
# 2. If it is found create a merge node which contains:
#   - the if_node and the last assignment before the if_node
# ========================================================================
create_last_assignment_list <- function(f) {
  arguments <- formals(f) |> names() |> as.character()
  args_list <- setNames(rep(1, length(arguments)), arguments)
  vars <- all.vars(body(f))
  vars <- setdiff(vars, arguments)
  vars_list <- setNames(rep(2, length(vars)), vars)
  c(args_list, vars_list)
}

merge_node <- R6::R6Class(
  "merge_node",
  public = list(
    id = NULL,
    last_assignment_before_if = NULL,
    if_node = NULL,
    phi = function() {}
  )
)

assigned_in_if <- function(node, env_in_if) {
  if (is_assign(node)  && inherits(node$left_node, "variable_node")) {
    varname <- node$left_node$name
    env_in_if$if_assignments[[varname]] <- node$id
  } else if (is_assign(node) && inherits(node$left_node, "binary_node")) {
    if (node$left_node$operator == "type") {
      varname <- node$left_node$left_node$name
      env_in_if$if_assignments[[varname]] <- node$id
    }
  }
}

is_assign <- function(node) {
  inherits(node, "binary_node") && (node$operator == "=" || node$operator == "<-")
}

gather_assignments <- function(node, env) {
  if (is_assign(node)  && inherits(node$left_node, "variable_node")) {
    varname <- node$left_node$name
    env$last_assignment[[varname]] <- node$id
  } else if (is_assign(node) && inherits(node$left_node, "binary_node")) {
    if (node$left_node$operator == "type") {
      varname <- node$left_node$left_node$name
      env$last_assignment[[varname]] <- node$id
    }
  } else if (inherits(node, "if_node")) {
    env_in_if <- new.env()
    vars <- names(env$last_assignment)
    env_in_if$if_assignments<- setNames(rep(-1, length(vars)), vars)
    traverse_ast(node, assigned_in_if, env_in_if) # TODO: should this be gather_assignments?

    for (i in seq_along(env_in_if$if_assignments)) {
      temp <- env_in_if$if_assignments[i]
      if (temp != -1) {
        varname <- names(temp)
        env$id <- env$id + 1
        mn <- merge_node$new()
        mn$id <- env$id
        mn$last_assignment_before_if <- env$last_assignment[[varname]]
        mn$if_node <- node$id
        env$all_nodes[[mn$id]] <- mn # NOTE: keep the merge nodes alive
        env$last_assignment[[varname]] <- mn$id
      }
    }
  }
}

# Extract edges from graph
# ========================================================================
extract_edges <- function(all_nodes) {
  edges <- list()
  for (node in all_nodes) {
    if (inherits(node, "variable_node")) {
      from <- node$last_assignment
      to <- node$id
      edges[[length(edges) + 1]] <- c(from, to)
    } else if (inherits(node, "merge_node")) {
      edges[[length(edges) + 1]] <- c(node$last_assignment_before_if, node$id)
    }
  }
  if (length(edges) == 0) return(matrix(nrow = 0, ncol = 2))
  edges_mat <- do.call(rbind, edges)
  colnames(edges_mat) <- c("from", "to")
  return(edges_mat)
}

# Topoligical sorting of assignments
# ========================================================================
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
