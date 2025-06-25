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

root_node <- R6::R6Class(
  "root_node",
  public = list(
    id = 2,
    last_assignment = NULL,
    print = function() {
      cat("Id:", self$id, "Root")
    }
  )
)
fct_arg_node <- R6::R6Class(
  "fct_arg_node",
  public = list(
    id = 1,
    last_assignment = NULL,
    print = function() {
      cat("Id:", self$id, "Function argument")
    }
  )
)
lhs_node <- R6::R6Class(
  "lhs_node",
  public = list(
    lhs = NULL,
    initialize = function(node) {
      self$lhs <- node
    },
    print = function() {
      cat(
        "Id:", self$lhs$id, "LHS Variable:", deparse(self$lhs$name),
        "Last assignment: ", self$lhs$last_assignment
      )
    }
  )
)
type_inference <- R6::R6Class(
  "type_inference",
  public = list(
    last_assignment = list(),
    all_nodes = NULL,
    in_if_node = FALSE,
    in_lhs_node = FALSE,
    counter = 3,
    initialize = function(f) {
      if (is.null(f)) { # for if nodes
        self$all_nodes <- list()
        return()
      }
      self$last_assignment <- create_last_assignment_list(f)
      self$all_nodes <- list(fct_arg_node$new(), root_node$new())
    },
    assign = function(node) {
      if (self$in_if_node) {
        node$id <- self$counter
        if (inherits(node, "merge_node")) {
          self$all_nodes[[self$counter]] <- node
        }
        self$counter <- self$counter + 1
        return()
      }
      if (self$in_lhs_node) {
        node$id <- self$counter
        var_lhs <- lhs_node$new(node)
        self$all_nodes[[self$counter]] <- var_lhs
        self$counter <- self$counter + 1
        return()
      }
      self$all_nodes[[self$counter]] <- node
      node$id <- self$counter
      self$counter <- self$counter + 1
    }
  )
)
merge_node <- R6::R6Class(
  "merge_node",
  public = list(
    id = NULL,
    last_assignment_before_if = NULL,
    if_node = NULL,
    type_infer_in_if = NULL,
    phi = function() {},
    print = function() {
      ids <- sapply(self$type_infer_in_if$all_nodes, function(x) x$id)
      ids <- Filter(Negate(is.null), ids)
      ids <- paste(ids, collapse = ", ")
      cat("Id:", self$id, "Merge node;",
        "last assign before if id", self$last_assignment_before_if,
        ";if node id", self$if_node,
        ";type infer all nodes ids:", ids
      )
    }
  )
)

is_assign <- function(node) {
  inherits(node, "binary_node") && (node$operator == "=" || node$operator == "<-")
}

gather_in_if <- function(node, type_infer_in_if, before_if_last_assignment) {
  if (is_assign(node)  && inherits(node$left_node, "variable_node")) {
    varname <- node$left_node$name
    type_infer_in_if$last_assignment[[varname]] <- node$id
  } else if (is_assign(node) && inherits(node$left_node, "binary_node")) {
    if (node$left_node$operator == "type") {
      varname <- node$left_node$left_node$name
      type_infer_in_if$last_assignment[[varname]] <- node$id
    }
  } else if (inherits(node, "if_node")) {
    print("test")
    str(node$true_node$block[[1]]$stringify())
    temp <- node$false_node$block[[1]]
    if(!is.null(temp)) str(temp$stringify())
    gather_assignments(node, type_infer_in_if, type_infer_in_if$before_if_last_assignment)
  }
  type_infer_in_if$assign(node)
}

gather_assignments <- function(node, type_infer, before_if_last_assignment) {
  if (is_assign(node)  && inherits(node$left_node, "variable_node")) {
    varname <- node$left_node$name
    type_infer$last_assignment[[varname]] <- node$id
  } else if (is_assign(node) && inherits(node$left_node, "binary_node")) {
    if (node$left_node$operator == "type") {
      varname <- node$left_node$left_node$name
      type_infer$last_assignment[[varname]] <- node$id
    }
  } else if (inherits(node, "if_node")) {
    # build a list for each block (if, else ifs, and else) and gather all assignments for each block.
    # Than determine a common type for each block.
    # Next, find a common type of the common types.
    # Subsequently, find the common type of the common type and the type for the variable before the if node
    # TODO: replace the type_infer_in_if with the flatted assignment list
    # Or create a type infer for each block and later recursivly call type inference --> Deep recursion...
    type_infer_in_if <- type_inference$new(NULL)
    type_infer_in_if$last_assignment <- before_if_last_assignment
    type_infer_in_if$counter <- type_infer$counter
    traverse_ast(node$true_node, function(x, tif, bila) {
      gather_in_if(x, tif, bila)
    }, type_infer_in_if, type_infer_in_if$last_assignment)
    if (!is.null(node$else_if_nodes)) {
      # Else if nodes are a list of if nodes containing only a true node
      lapply(node$else_if_nodes, function(n) {
        traverse_ast(n$true_node, function(x, tif, bila) {
          gather_in_if(x, tif, bila)
        }, type_infer_in_if, type_infer_in_if$last_assignment)
      })
    }
    if (!is.null(node$false_node)) {
      traverse_ast(node$false_node, function(x, tif, bila) {
        gather_in_if(x, tif, bila)
      }, type_infer_in_if, type_infer_in_if$last_assignment)
    }
    vars <- names(type_infer$last_assignment)
    vars_in_if <- vars[which(before_if_last_assignment != type_infer_in_if$last_assignment)]
    for (i in seq_along(vars_in_if)) {
      mn <- merge_node$new()
      mn$last_assignment_before_if <- before_if_last_assignment[[vars_in_if[i]]]
      mn$if_node <- node$id
      mn$type_infer_in_if <- type_infer_in_if
      type_infer$assign(mn)
      type_infer$last_assignment[[vars_in_if[[i]]]] <- mn$id
    }
  }
}

# Extract edges from graph
# ========================================================================
get_variables_action <- function(node, env) {
  if (inherits(node, "variable_node")) env$vars <- c(env$vars, node)
}
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
