# =====================================================================
# Create tree
# =====================================================================
create_type_tree <- function(code, env) {
  code <- as.list(code)
  operator <- deparse(code[[1L]])
  arity_check(code, env)
  if (!is.null(env$error)) return()

  if (operator == "type") {

    t <- pre_type_node$new(iterator = FALSE, type_decl = TRUE, fct_input = env$fct_input, error = NULL)
    t$name <- deparse(code[[2L]])
    t$r_fct <- env$r_fct
    t$real_type <- env$real_type
    if (is.symbol(code[[3L]])) {
      t$data_struct <- scalar$new()
      t$data_struct$real_type <- env$real_type
      t$data_struct$r_fct <- env$r_fct
      t$data_struct$base_type <- resolve_type_collection(code[[3L]], env)
    } else {
      t$data_struct <- parse_type_tree(code[[3L]], env)
    }
    return(t)
  }

  else if (operator %in% c("vec", "vector")) {
    v <- vec$new()
    v$r_fct <- env$r_fct
    v$real_type <- env$real_type
    v$base_type <- resolve_base_type(code[[2L]], env)
    return(v)
  }
  else if (operator %in% c("mat", "matrix")) {
    m <- mat$new()
    m$r_fct <- env$r_fct
    m$real_type <- env$real_type
    m$base_type <- resolve_base_type(code[[2L]], env)
    return(m)
  }
  else if (operator == "array") {
    a <- arr$new()
    a$r_fct <- env$r_fct
    a$real_type <- env$real_type
    a$base_type <- resolve_base_type(code[[2L]], env)
    return(a)
  }
  else if (operator == "collection") {
    c <- collection$new()
    c$type <- resolve_type_collection(code[[2L]], env)
    if (is_base_type(c$type)) {
      env$error <- "collections cannot contain basic types but only class types"
    }
    return(c)
  }

  else if (operator %in% c("borrow_vec", "borrow_vector") && env$fct_input) {
    v <- borrow_vec$new()
    v$base_type <- resolve_base_type(code[[2L]], env)
    v$r_fct <- env$r_fct
    v$real_type <- env$real_type
    return(v)
  }
  else if (operator %in% c("borrow_mat", "borrow_matrix") && env$fct_input) {
    m <- borrow_mat$new()
    m$base_type <- resolve_base_type(code[[2L]], env)
    m$r_fct <- env$r_fct
    m$real_type <- env$real_type
    return(m)
  }
  else if (operator == "borrow_array" && env$fct_input) {
    a <- borrow_arr$new()
    a$base_type <- resolve_base_type(code[[2L]], env)
    a$r_fct <- env$r_fct
    a$real_type <- env$real_type
    return(a)
  }

  else if (operator %in% c("borrow_vec", "borrow_vector", "borrow_mat", "borrow_matrix", "borrow_array")) {
    env$error <- sprintf("borrow types only allowed in function inputs, found %s", operator)
  }

  else if (operator == "const" && env$fct_input && env$r_fct) {
    env$error <- "const has no effect when the output is an R function, as arguments are passed as SEXP"
  }
  else if (operator == "const" && env$fct_input) {
    t <- parse_type_tree(code[[2L]], env)
    t$const_or_mut <- "const"
    return(t)
  }
  else if (operator == "ref" && env$fct_input && env$r_fct) {
    env$error <- "ref has no effect when the output is an R function, as arguments are passed as SEXP"
  }
  else if (operator == "ref" && env$fct_input) {
    t <- parse_type_tree(code[[2L]], env)
    t$copy_or_ref <- "ref"
    return(t)
  }

  else  {
    env$error <- sprintf("Found unsupported function %s", operator)
  }

}

# =====================================================================
# Parser
# =====================================================================
parse_type_tree <- function(code, env) {
  if (!is.symbol(code) && is.call(code)) {
    create_type_tree(code, env)
  }
}

parse_new_type <- function(code, env) {
  code <- as.list(code)
  operator <- deparse(code[[1L]])
  if (operator == "new_type") {
    env$new_type <- new_type_node$new(code[[2L]])
    parse_new_type(code[[3]], env)
  }
  if (operator == "slots") {
    code <- code[-1]
    env$new_type$slots <- list()
    for (i in seq_len(length(code))) {
      if (is.symbol(code[[i]])) {
        env$new_type$slots[[i]] <- env$list[[deparse(code[[i]])]]
      } else {
        operator <- deparse(code[[i]][[1L]])
        if (operator == "type") {
          env$new_type$slots[[i]] <- parse_type_tree(code[[i]], env)
        } else if (operator == "new_type") {
          env$new_type$slots[[i]] <- parse_type_tree(code[[i]], env)
        } else {
          env$error <- sprintf("Found unsupported function %s", operator)
        }
      }
    }
  }
}

parse_types <- function(block, fct_input, r_fct, real_type) {
  env <- new.env(parent = emptyenv())
  env$list <- list()
  env$fct_input <- fct_input
  env$r_fct <- r_fct
  env$real_type <- real_type
  for (i in seq_len(length(block))) {
    row <- block[[i]]
    operator <- try(deparse(row[[1L]]), silent = TRUE)
    if (inherits(operator, "try-error")) {
      stop(sprintf("Cannot resolve type of: %s", deparse(row)))
    }
    if (operator %in% c("type", "ref", "const")) {
      temp <- parse_type_tree(row, env)
      if (!is.null(env$error)) {
        stop(env$error)
      }
      env$list[[temp$name]] <- temp
    } else if (operator == "new_type") {
      parse_new_type(row, env)
      env$list[[env$new_type$name]] <- env$new_type
    } else {
      env$error <- sprintf("Found unsupported function %s in %s", operator, deparse(row))
    }
  }
  if (!is.null(env$error)) {
    stop(env$error)
  }
  env$list
}
