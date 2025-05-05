# Handle the case when f_args is NULL
# ========================================================================
parse_null_args <- function(f, r_fct) {
  args <- formals(f) |> as.list() |> names()
  lapply(args, function(obj) {
    t <- type_node$new(NULL, TRUE, r_fct)
    t$base_type <- "double"
    t$data_struct <- "vector"
    t$copy_or_ref <- "copy"
    t$name <- obj
  })
}

# Parses the langauge object and extracts:
# - how to handle the argument (copy, reference, ref)
# - the type of the argument (actual type, const)
# ========================================================================
# Handle f_args == NULL --> default types and handling
standard_args_f <- function(f, r_fct) {
  args <- names(formals(f))
  exprs <- lapply(args, as.name)
  body_expr <- as.call(c(quote(`{`), exprs))
  args_f <- function() {}
  body(args_f) <- body_expr
  args_f
}

check_input_args <- function(variables) {
  for (i in seq_along(variables)) {
    if (!is.null(variables[[i]]$type$error)) {
      lapply(variables[[i]]$type$error, print)
      stop()
    }
  }
}

parse_input_args <- function(f, f_args, r_fct) {
  if (is.null(f_args)) {
    f_args <- standard_args_f(f, r_fct)
  }
  args <- body(f_args) |> as.list()
  if (deparse(args[[1]]) == "{") {
    args <- args[-1]
  }
  args <- lapply(args, function(x) {
    attributes(x) <- NULL
    x
  })

  if (length(args) != length(formals(f))) {
    stop("The number of arguments to f does not match the number of expressions in f_args")
  }

  l <- lapply(args, function(obj) {
    t <- type_node$new(obj, TRUE, r_fct)
    t$init()
    t$check()
    t
  })
  variables <- lapply(seq_along(l), function(i) {
    v <- variable_node$new(l[[i]]$name)
    v$type <- l[[i]]
    v$declared <- TRUE
    v$initialized <- TRUE
    v
  })
  check_input_args(variables)
  variables
}

# Gather relevant nodes in each ast entry
# ========================================================================
gather_vars <- function(node, variables) {
  if (inherits(node, "variable_node")) {
    name <- str2lang(node$name) # TODO: Check why str2lang is required
    variables$variable_list[[name]] <- c(variables$variable_list[[name]], node)
    variables$names <- c(variables$names, node$name)
  } else if (inherits(node, "binary_node") && inherits(node$left_node, "variable_node")) {
    name <- str2lang(node$left_node$name)
    variables$variable_list[[name]] <- c(variables$variable_list[[name]], node)
    variables$names <- c(variables$names, node$left_node$name)
  } else if (inherits(node, "binary_node") && inherits(node$left_node, "binary_node")) {
    if (node$left_node$operator == "type") {
      name <- str2lang(node$left_node$left_node$name)
      node$left_node$left_node$type <- node$left_node$right_node
      variables$variable_list[[name]] <- c(variables$variable_list[[name]], node)
      variables$names <- c(variables$names, node$left_node$left_node$name)
    } else if (node$left_node$operator %in% c("[", "[[")) {
      name <- str2lang(node$left_node$left_node$name)
      variables$variable_list[[name]] <- c(variables$variable_list[[name]], node)
      variables$names <- c(variables$names, node$left_node$left_node$name)
    }
  } else if (inherits(node, "if_node")) {
    gather_vars(node$true_node, variables)
    if (!is.null(node$else_if_nodes)) {
      lapply(node$else_if_nodes, function(branch) {
        gather_vars(branch, variables)
      })
    }
    if (!is.null(node$false_node)) {
      gather_vars(node$false_node, variables)
    }
  } else if (inherits(node, "block_node")) {
    lapply(node$block, function(stmt) gather_vars(stmt, variables))
  } else if (inherits(node, "for_node")) {
    name <- node$i$name # TODO: Assuming it is a variable node. Maybe requires a check
    variables$variable_list[[name]] <- c(variables$variable_list[[name]], node)
    variables$names <- c(variables$names, node$i$name)
    lapply(node$block$block, function(stmt) gather_vars(stmt, variables))
  }
}

# Check types
# ========================================================================
# - Check that used type is valid; --> Done during action_error
# - Check that for each variable only one unique type is declared
# - Check that for each variable only once a type is declared
# - Check that the type declaration occures at the first occurance of the variable
# - Check that the iterator variables are not used elsewhere

determine_type_of_assignment <- function(node) {
  if (inherits(node, "variable_node") && node$type$fct_input) {
    return("function_argument")
  } else if (inherits(node, "binary_node")) {
    if (node$operator %in% c("<-", "=") && inherits(node$left_node, "variable_node")) {
      return("assign_to_variable")
    }
    if (node$operator %in% c("<-", "=") && inherits(node$left_node, "binary_node")) {
      if (node$left_node$operator == "type") {
        return("assign_to_declaration")
      }
      if (node$left_node$operator %in% c("[", "[[", "at")) {
        return("assign_to_subset")
      }
    }
    if (node$operator == "type") {
      return("declaration")
    }
  } else if (inherits(node, "for_node")) {
    return("iteration")
  }
  return("Unknown node at lhs")
}

variables <- R6::R6Class(
  "variables",
  public = list(
    r_fct = NULL,
    variable_list = list(),
    names = c(),
    errors = NULL,

    variable_type_list = list(),

    initialize = function(f, f_args, r_fct) {
      parsed_args <- parse_input_args(f, f_args, r_fct)
      self$r_fct <- r_fct
      all_vars <- all.vars(body(f))
      all_vars <- setdiff(all_vars, permitted_base_types()) # TODO: add check that permitted_base_types are not used as variables
      self$variable_list <- rep(list(NULL), length(all_vars))
      names(self$variable_list) <- all_vars

      self$variable_type_list <- self$variable_list

      # TODO: str2lang(x$name) does not work when function accepts 0 arguments
      self$variable_list[sapply(parsed_args, \(x) str2lang(x$name))] <- parsed_args
      # names is incrementaly updated.
      self$names <- sapply(parsed_args, function(x) str2lang(x$name))
    },

    # Determine types
    # ========================================================================
    determine_types_rhs = function(node) {
      if (inherits(node, "literal_node")) {
        type <- determine_literal_type(node$name)
        if (type %in% c("scientific", "numeric")) {
          type <- "double"
        }
        t <- type_node$new(str2lang(node$name), FALSE, self$r_fct)
        t$base_type <- type
        t$data_struct <- "scalar"
        return(t)
      } else if (inherits(node, "variable_node")) {
        return(self$variable_type_list[[node$name]]$type) # TODO: is a check needed that the variable possesses a type?
      } else if (inherits(node, "unary_node")) {
        if (node$operator == "print") {
          self$errors <- c(self$errors, sprintf("Found print within an expression: %s", node$stringify()))
        } else if (node$operator == "return") {
          if (node$context == "Start") {
            return(self$determine_types_rhs(node$obj))
          } else {
            self$errors <- c(self$errors, sprintf("Found return within an expressio: %s", node$stringify()))
          }
        } else if (node$operator %in% c("sin", "asin", "sinh", "cos", "acos", "cosh", "tan", "atan", "tanh", "log", "sqrt", "exp")) {
          inner_type <- self$determine_types_rhs(node$obj)
          t <- type_node$new(NA, FALSE, self$r_fct)
          t$base_type <- "double"
          t$data_struct <- inner_type$data_struct
          return(t)
        } else if (node$operator == "-") {
          inner_type <- self$determine_types_rhs(node$obj)
          base_type <- inner_type$base_type
          if (base_type == "logical") base_type <- "int" # TODO: inform user about type promotion
          t <- type_node$new(NA, FALSE, self$r_fct)
          t$base_type <- base_type
          t$data_struct <- inner_type$data_struct
          return(t)
        } else if (node$operator == "(") {
          inner_type <- self$determine_types_rhs(node$obj)
          return(inner_type)
        } else if (node$operator %in% c("length", "dim")) {
          t <- type_node$new(NA, FALSE, self$r_fct)
          t$base_type <- "integer"
          printt$data_struct <- "vector"
          return(t)
        } else if (node$operator %in% c("is.na", "is.finite", "is.infinite")) {
          inner_type <- self$determine_types_rhs(node$obj)
          t <- type_node$new(NA, FALSE, self$r_fct)
          t$base_type <- "logical"
          t$data_struct <- inner_type$data_struct
          return(t)
        }
      } else if (inherits(node, "binary_node")) {
        if (node$operator %in% c("=", "<-")) {
          self$errors <- c(self$errors, sprintf("Found assignment within an expression: %s", node$stringify()))
        } else if (node$operator == ":") {
          left_type <- self$determine_types_rhs(node$left_node)
          right_node <- self$determine_types_rhs(node$right_node)
          left_base_type <- left_type$base_type
          right_base_type <- right_type$base_type
          if (left_base_type == "logical") left_base_type <- "integer"
          if (right_base_type == "logical") right_base_type <- "integer"
          common_type <- "integer"
          if (any(c(left_base_type, right_base_type) %in% c("double"))) {
            common_type <- "double"
          }
          t <- type_node$new(NA, FALSE, self$r_fct)
          t$base_type <- common_type
          t$data_struct <- "vector"
          return(t)
        } else if (node$operator %in% c("^", "+", "*", "/", "-", "power")) {
          left_type <- self$determine_types_rhs(node$left_node)
          right_node <- self$determine_types_rhs(node$right_node)
          left_base_type <- left_type$base_type
          right_base_type <- right_type$base_type
          if (left_base_type == "logical") left_base_type <- "integer"
          if (right_base_type == "logical") right_base_type <- "integer"
          common_type <- "integer"
          if (any(c(left_base_type, right_base_type) %in% c("double"))) {
            common_type <- "double"
          }
          common_data_struct <- "vector"
          if (any(c(left_type$data_struct, right_type$data_struct) %in% "matrix")) {
            common_data_struct <- "matrix"
          }
          t <- type_node$new(NA, FALSE, self$r_fct)
          t$base_type <- common_type
          t$data_struct <- common_data_struct
          return(t)
        } else if (node$operator %in% c("==", "!=", ">", ">=", "<", "<=", "&&", "||")) {
          left_type <- self$determine_types_rhs(node$left_node)
          right_node <- self$determine_types_rhs(node$right_node)
          common_type <- "logical"
          common_data_struct <- "scalar"
          if (any(c(left_type$data_struct, right_type$data_struct) %in% "vector")) {
            common_data_struct <- "vector"
          }
          if (any(c(left_type$data_struct, right_type$data_struct) %in% "matrix")) {
            common_data_struct <- "matrix"
          }
          t <- type_node$new(NA, FALSE, self$r_fct)
          t$base_type <- common_type
          t$data_struct <- common_data_struct
          return(t)
        } else if (node$operator %in% c("[", "[[", "at")) {
          # This is due to number of args vector subsetting
          left_type_node <- self$determine_types_rhs(node$left_node)
          t <- type_node$new(NA, FALSE, self$r_fct)
          t$base_type <- left_type_node
          t$data_struct <- "vector"
          return(t)
        }
      } else if (inherits(node, "function_node")) {
        if (node$operator == "cmr") {
          t <- type_node$new(NA, FALSE, self$r_fct)
          t$base_type <- "double"
          t$data_struct <- "scalar"
          return(t)
        } else if (node$operator == "vector") {
          mode <- node$args[[1]]
          t <- type_node$new(NA, FALSE, self$r_fct)
          t$base_type <- mode
          t$data_struct <- "vector"
          return(t)
        } else if (node$operator == "matrix") {
          first_arg <- node$args[[1]]
          type_first_arg <- self$determine_types_rhs(first_arg)
          t <- type_node$new(NA, FALSE, self$r_fct)
          t$base_type <- type_first_arg
          t$data_struct <- "matrix"
          return(t)
        } else if (node$operator == "c") {
          types_of_args <- lapply(node$args, self$determine_types_rhs)
          types_of_args <- sapply(types_of_args, \(x) x$base_type)
          common_type <- "logical"
          if (any(types_of_args %in% c("int", "integer"))) {
            common_type <- "integer"
          } else if (any(types_of_args == "double")) {
            common_type <- "double"
          }
          t <- type_node$new(NA, FALSE, self$r_fct)
          t$base_type <- common_type
          t$data_struct <- "vector"
          return(t)
        } else if (node$operator %in% c("[", "[[", "at")) {
          # This is due to number of args matrix subsetting
          first_arg <- node$args[[1]]
          type_first_arg <- self$determine_types_rhs(first_arg)
          t <- type_node$new(NA, FALSE, self$r_fct)
          t$base_type <- type_first_arg
          t$data_struct <- "matrix"
          return(t)
        }
      } else {
        self$errors <- c(self$errors, sprintf("Cannot determine the type for: %s", node$stringify()))
      }
    },

    check = function() {
      names <- unique(self$names)

      for (name in names) {
        nodes <- self$variable_list[[name]]

        if (is.list(nodes)) {
          temp_l <- lapply(nodes, function(x) {
            if (inherits(x, "binary_node") && x$operator %in% c("<-", "=")) {
              x <- x$right_node
            }
            temp <- self$determine_types_rhs(x)
          })
          lapply(temp_l, function(x) {
            cat("Base type: ", x$base_type, "Data struct: ", x$data_struct, "\n")
          })
        } else {
          node_rhs <- nodes
          if (inherits(nodes, "binary_node") && nodes$operator %in% c("<-", "=")) {
            node_rhs <- nodes$right_node
          }
          self$variable_type_list[[name]] <- traverse_ast(node_rhs, self$determine_types_rhs)
        }

        if (is.null(nodes)) next
        assignment_types <- lapply(nodes, determine_type_of_assignment)

        # Check: how many type declarations?
        num_declarations <- sum(assignment_types %in% c("declaration", "assign_to_declaration"))
        declaration_indices <- which(assignment_types %in% c("declaration", "assign_to_declaration"))

        # (1) At most one type declaration
        if (num_declarations > 1) {
          self$errors <- c(self$errors,
            sprintf("Variable '%s' has multiple type declarations\n", name))
        }

        # (2) If declared, must be the first appearance
        if (num_declarations == 1 && declaration_indices[1] != 1) {
          self$errors <- c(self$errors,
            sprintf("Variable '%s' has type declaration but it is not the first one. Error in: \n %s",
              name, nodes[[declaration_indices[1]]]$stringify()))
        }

        # (3) Iterator variables must not be used elsewhere
        if (any(assignment_types == "iteration") && length(nodes) != 1) {
          if (length(nodes) > 1) {
            self$errors <- c(self$errors,
              sprintf("Iterator variable '%s' is used at left hand site of assignment. First misuse here: %s \n",
                name, nodes[[2]]$stringify()
              ))
          }
        }
      }

    }
    # Infer type for variables with no type declaration; Find common type across all assignments and store it in self$variable_type_list
    # For each assignment check that type is suitable for rhs
    # Use the type_node to declare the variable declarations
    # Use the type nodes to define the function signature. --> append SEXP to name in case r_fct
    # Combine it to cpp code

  )
)
