# =====================================================================
# Create tree
# =====================================================================
create_type_tree <- function(code, env) {
  code <- as.list(code)
  operator <- deparse(code[[1L]])
  n_errors_before <- length(env$errors)
  arity_check(code, env)
  if (length(env$errors) > n_errors_before) {
    stop(env$errors[[length(env$errors)]])
  }

  if (operator == "type") {

    if (is.symbol(code[[3L]])) {
      elem <- deparse(code[[3L]])
      custom_type <- env$list[[elem]]
      if (!is.null(custom_type) && inherits(custom_type, "new_type_node")) {
        t <- custom_type$clone()
        t$var_name <- deparse(code[[2L]])
        t$type_decl <- TRUE
        t$fct_input <- env$fct_input
        t$iterator <- FALSE
        t$error <- NULL
        t$r_fct <- env$r_fct
        t$real_type <- env$real_type
        return(t)
      }
    }

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

  else if (operator == "scalar") {
    s <- scalar$new()
    s$r_fct <- env$r_fct
    s$real_type <- env$real_type
    s$base_type <- resolve_base_type(code[[2L]], env)
    return(s)
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
    c$r_fct <- env$r_fct
    c$real_type <- env$real_type
    c$type <- resolve_type_collection(code[[2L]], env)
    if (is_base_type(c$type)) {
      add_error(env, "collections cannot contain basic types but only class types")
    } else {
      c$element_type <- env$list[[c$type]]
    }
    return(c)
  }

  else if (operator %in% c("borrow_vec", "borrow_vector") && env$fct_input) {
    if (env$real_type %in% c("etr::Dual", "etr::ReverseDouble")) {
      add_error(env, "borrow types cannot be used together with automatic differentiation (forward or reverse)")
    }
    v <- borrow_vec$new()
    v$base_type <- resolve_base_type(code[[2L]], env)
    v$r_fct <- env$r_fct
    v$real_type <- env$real_type
    return(v)
  }
  else if (operator %in% c("borrow_mat", "borrow_matrix") && env$fct_input) {
    if (env$real_type %in% c("etr::Dual", "etr::ReverseDouble")) {
      add_error(env, "borrow types cannot be used together with automatic differentiation (forward or reverse)")
    }
    m <- borrow_mat$new()
    m$base_type <- resolve_base_type(code[[2L]], env)
    m$r_fct <- env$r_fct
    m$real_type <- env$real_type
    return(m)
  }
  else if (operator == "borrow_array" && env$fct_input) {
    if (env$real_type %in% c("etr::Dual", "etr::ReverseDouble")) {
      add_error(env, "borrow types cannot be used together with automatic differentiation (forward or reverse)")
    }
    a <- borrow_arr$new()
    a$base_type <- resolve_base_type(code[[2L]], env)
    a$r_fct <- env$r_fct
    a$real_type <- env$real_type
    return(a)
  }

  else if (operator %in% c("borrow_vec", "borrow_vector", "borrow_mat", "borrow_matrix", "borrow_array")) {
    add_error(env, sprintf("borrow types only allowed in function inputs, found %s", operator))
    u <- unresolved_node$new()
    u$name <- operator
    u$base_type <- resolve_base_type(code[[2L]], env)
    return(u)
  }

  else if (operator == "const" && env$fct_input) {
    t <- parse_type_tree(code[[2L]], env)
    t$const_or_mut <- "const"
    return(t)
  }
  else if (operator == "ref" && env$fct_input && env$r_fct) {
    warning("ref has no effect when the output is an R function, as arguments are passed as SEXP")
    t <- parse_type_tree(code[[2L]], env)
    t$copy_or_ref <- "ref"
    return(t)
  }
  else if (operator == "ref" && env$fct_input) {
    t <- parse_type_tree(code[[2L]], env)
    t$copy_or_ref <- "ref"
    return(t)
  }

  else  {
    bt <- NA_character_
    if (length(code) >= 2L) {
      bt <- resolve_base_type(code[[2L]], env)
    }
    add_error(env, sprintf("Found unsupported data structure: %s", operator))
    u <- unresolved_node$new()
    u$name <- operator
    u$base_type <- bt
    return(u)
  }

}

# =====================================================================
# Parser
# =====================================================================
parse_type_tree <- function(code, env) {
  if (!is.symbol(code) && is.call(code)) {
    return(create_type_tree(code, env))
  }
  add_error(env, sprintf("Found unsupported base type: %s", deparse(code)))
  s <- scalar$new()
  s$base_type <- deparse(code)
  s
}

parse_new_type <- function(code, env) {
  code <- as.list(code)
  operator <- deparse(code[[1L]])
  if (operator == "new_type") {
    name <- deparse(code[[2L]])
    unallowed <- unallowed_signs(name)
    if (!is.null(unallowed)) {
      add_error(env, paste0(unallowed, " found in ", name))
    } else if (not_cpp_keyword(name)) {
      add_error(env, paste0("Invalid type name: is a C++ keyword --> ", name))
    }
    if (!is.null(env$list[[name]])) {
      add_error(env, paste0("Duplicated type name: ", name))
    }
    env$new_type <- new_type_node$new(name)
    parse_new_type(code[[3]], env)
  }
  if (operator == "slots") {
    code <- code[-1]
    if (length(code) == 0) {
      add_error(env, "A custom type must have at least one slot")
    }
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
          add_error(env, sprintf("Found unsupported function %s", operator))
        }
        if (operator %in% c("type", "new_type")) {
          slot_name <- env$new_type$slots[[i]]$get_name()
          unallowed <- unallowed_signs(slot_name)
          if (!is.null(unallowed)) {
            add_error(env, paste0(unallowed, " found in ", slot_name))
          } else if (not_cpp_keyword(slot_name)) {
            add_error(env, paste0("Invalid slot name: is a C++ keyword --> ", slot_name))
          } else if (identical(slot_name, env$new_type$name)) {
            add_error(env, paste0("Invalid slot name: cannot be the same as its own type name --> ", slot_name))
          }
        }
      }
    }
    slot_names <- vapply(env$new_type$slots, function(s) s$get_name(), character(1L))
    for (d in unique(slot_names[duplicated(slot_names)])) {
      add_error(env, paste0("Duplicated slot name: ", d))
    }
  }
}

parse_types <- function(block, fct_input, r_fct, real_type, known_types = list()) {
  env <- new.env(parent = emptyenv())
  env$list <- known_types
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
      env$errors <- NULL
      temp <- parse_type_tree(row, env)
      if (length(env$errors) > 0) {
        temp$error <- combine_strings(env$errors, "\n")
      }
      env$list[[temp$get_name()]] <- temp
    } else if (operator == "new_type") {
      env$errors <- NULL
      parse_new_type(row, env)
      if (length(env$errors) > 0) {
        stop(combine_strings(env$errors, "\n"))
      }
      env$new_type$real_type <- real_type
      env$new_type$r_fct <- r_fct
      env$list[[env$new_type$name]] <- env$new_type
    } else {
      stop(sprintf("Found unsupported function %s in %s", operator, deparse(row)))
    }
  }
  env$list[setdiff(names(env$list), names(known_types))]
}

# =====================================================================
# Build a type ad hoc (no source expression to parse, e.g. inferred
# types of expressions such as subsetting, arithmetic, ...)
# =====================================================================
make_inferred_type <- function(data_struct, base_type, r_fct, real_type) {
  ds <- switch(data_struct,
    scalar = scalar$new(),
    vector = vec$new(),
    matrix = mat$new(),
    array = arr$new()
  )
  ds$base_type <- base_type
  ds$r_fct <- r_fct
  ds$real_type <- real_type
  t <- pre_type_node$new(iterator = FALSE, type_decl = FALSE, fct_input = FALSE, error = NULL)
  t$data_struct <- ds
  t$r_fct <- r_fct
  t$real_type <- real_type
  t
}

predefined_known_types <- function() {
  new_type(
    uniroot_result,
    slots(
      root |> type(double),
      f_root |> type(double),
      iter |> type(int),
      estim_prec |> type(double)
    )
  )
}

make_known_types <- function(types_fct, r_fct, real_type) {
  types_block <- as.list(wrap_in_block(body(predefined_known_types)))[-1]
  known_types <- parse_types(types_block, fct_input = FALSE, r_fct, real_type)
  for (nm in names(known_types)) {
    if (inherits(known_types[[nm]], "new_type_node")) {
      known_types[[nm]]$part_of_etr <- TRUE
    }
  }
  if (!is.null(types_fct)) {
    types_block <- as.list(wrap_in_block(body(types_fct)))[-1]
    known_types <- c(known_types, parse_types(types_block, fct_input = FALSE, r_fct, real_type))
  }
  known_types
}

