# The return-type pass runs after infer_types has already reset every
# iterator's in_scope flag. A return() lexically inside a for-loop is legal,
# so re-assert in_scope for that iterator while descending this loop's body,
# then restore it (save/restore, not force-FALSE, so nested loops and the
# use-after-loop rejection still work). Returns TRUE so traverse_ast() skips
# its own block descent -- same contract as type_infer_for_node().
type_infer_return_for_node <- function(node, info_env) {
  if (!inherits(node, "for_node")) return(NULL)
  variable <- deparse(node$i$name)
  it <- info_env$vars_list[[variable]]
  is_iter <- inherits(it, c("pre_type_node", "new_type_node")) && isTRUE(it$get_iterator())
  prev <- if (is_iter) it$get_in_scope() else NULL
  if (is_iter) it$set_in_scope(TRUE)
  traverse_ast(node$block, type_infer_return_action, info_env)
  if (is_iter) it$set_in_scope(prev)
  TRUE
}

type_infer_return_action <- function(node, info_env) {
  if (isTRUE(type_infer_return_for_node(node, info_env))) return(TRUE)
  if (inherits(node, "unary_node") && node$operator == "return") {
    type <- infer(node$obj, info_env$vars_list, info_env, info_env$function_registry)
    if (is.character(type)) {
      node$error <- type
    } else if (inherits(type, "fn_node")) {
      node$error <- "Cannot return an inner function"
    } else {
      info_env$return_list[[length(info_env$return_list) + 1]] <- type
    }
    info_env$found_non_void_return <- TRUE
  } else if (inherits(node, "nullary_node") && node$operator == "return") {
    info_env$return_list[[length(info_env$return_list) + 1]] <- if (info_env$r_fct) type <- "R_NilValue" else "void"
    info_env$found_void_return <- TRUE
  }
}

check_tail_missing_else <- function(node) {
  error_message <- "If the last statement is an if-block an else-branch is required!"
  if (inherits(node, "block_node")) {
    n <- length(node$block)
    if (n > 0) check_tail_missing_else(node$block[[n]])
  } else if (inherits(node, "if_node")) {
    if (is.null(node$false_node)) {
      if (!is.null(node$else_if_nodes)) {
        node$else_if_nodes[[length(node$else_if_nodes)]]$error <- error_message
      } else {
        node$error <- error_message
      }
      return(invisible())
    }
    check_tail_missing_else(node$true_node)
    if (!is.null(node$else_if_nodes)) {
      lapply(node$else_if_nodes, function(ei) check_tail_missing_else(ei$true_node))
    }
    check_tail_missing_else(node$false_node)
  }
  invisible()
}

wrap_in_return <- function(node) {
  ret <- unary_node$new()
  ret$operator <- "return"
  ret$obj <- node
  ret$context <- if (is.null(node$context)) "{" else node$context
  ret
}

create_void_return <- function() {
  ret <- nullary_node$new()
  ret$operator <- "return"
  ret$context <- "{"
  ret
}

# TRUE if node is a call to an inner (fn()) function whose declared return type
# is void. Only inner functions are probed -- their infer_fct is the trivial one
# added by action_update_function_registry, so calling it here is safe.
tail_call_is_void <- function(node, function_registry) {
  op <- node$operator
  if (is.null(op)) {
    return(FALSE)
  }
  is_inner <- (op %in% function_registry$permitted_fcts()) &&
    !(op %in% function_registry_global$permitted_fcts())
  if (!is_inner) {
    return(FALSE)
  }
  t <- tryCatch(
    function_registry$infer_fct(op)(node, list(), emptyenv(), function_registry),
    error = function(e) NULL
  )
  inherits(t, "pre_type_node") && identical(t$get_base_type(), "void")
}

# Reduce the value returns to one type.
#   r_fct always returns SEXP -> do NOT reconcile or widen; common_type() also
#     cannot combine new_type / collection returns.
#   XPtr needs one concrete type: widen the plain base/structure lattice via
#     common_type; custom types must be identical.
reconcile_return_types <- function(value_types, r_fct) {
  if (length(value_types) == 0) {
    return(if (r_fct) "R_NilValue" else "void")
  }
  if (r_fct) {
    if (length(value_types) == 1) {
      return(value_types[[1]])
    }
    return("R_NilValue")
  }
  # A string return works for output "R" (Cast(const char*) -> mkString) but
  # has no concrete C++ type for the XPtr interface. A lone string return is
  # left for translate_internally to reject; a string mixed with other returns
  # is caught here (common_type cannot combine it).
  Reduce(function(acc, t) {
    if (inherits(acc, "pre_type_node") && inherits(t, "pre_type_node")) {
      if (identical(acc$get_base_type(), "character") || identical(t$get_base_type(), "character")) {
        stop("An XPtr function cannot return a string")
      }
      return(common_type(acc, t) |> flatten_type())
    }
    same_custom <- identical(class(acc)[[1]], class(t)[[1]]) &&
      !is.null(acc$get_name) && !is.null(t$get_name) &&
      identical(acc$get_name(), t$get_name())
    if (same_custom) {
      return(acc)
    }
    stop("An XPtr function must return the same type on every path")
  }, value_types)
}

create_return_statement <- function(node, function_registry) {
  if (inherits(node, "block_node")) {
    n <- length(node$block)
    if (n == 0) {
      node$block[[1]] <- create_void_return()
      return(node)
    }
    repl <- create_return_statement(node$block[[n]], function_registry)
    if (inherits(repl, "list")) {
      # void tail call: keep the call for its side effects, then return()
      node$block[[n]] <- repl[[1]]
      node$block[[n + 1]] <- repl[[2]]
    } else {
      node$block[[n]] <- repl
    }
    return(node)
  } else if (inherits(node, "if_node")) {
    node$true_node <- create_return_statement(node$true_node, function_registry)
    if (!is.null(node$else_if_nodes)) {
      node$else_if_nodes <- lapply(node$else_if_nodes, function(ei) {
        ei$true_node <- create_return_statement(ei$true_node, function_registry)
        ei
      })
    }
    if (!is.null(node$false_node)) {
      node$false_node <- create_return_statement(node$false_node, function_registry)
    }
    return(node)
  } else if (inherits(node, c("nullary_node", "unary_node")) && node$operator == "return") {
    return(node)
  } else if (inherits(node, "nullary_node") && node$operator %in% c("break", "next", "continue")) {
    return(node)
  } else if (inherits(node, "binary_node") && (node$operator %in% c("<-", "="))) {
    return(list(node, create_void_return())) # keep the assignment, then return()
  } else if (!is.null(node$operator) && void_only_operator(node$operator)) {
    return(list(node, create_void_return())) # print / stop / seed / unseed: no value
  } else if (tail_call_is_void(node, function_registry)) {
    return(list(node, create_void_return())) # inner fn returning void
  } else if (inherits(node, c("nullary_node", "unary_node", "binary_node",
                              "function_node", "literal_node", "variable_node"))) {
    return(wrap_in_return(node)) # bare expr, incl. a 0-arg inner-function call
  } else {
    return(list(node, create_void_return())) # loop etc.: keep it, then return()
  }
}
