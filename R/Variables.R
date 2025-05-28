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

# Gather relevant nodes in each ast entry & return nodes
# ========================================================================
gather_vars_and_returns <- function(node, variables) {
  if (inherits(node, "variable_node")) {
    name <- node$name
    variables$variable_list[[name]] <- c(variables$variable_list[[name]], node)
    variables$names <- c(variables$names, node$name)
  } else if ( (inherits(node, "unary_node") || inherits(node, "nullary_node")) && node$operator == "return") {
    variables$return_list <- c(variables$return_list, node)
  } else if (inherits(node, "binary_node") && inherits(node$left_node, "variable_node")) {
    name <- node$left_node$name
    variables$variable_list[[name]] <- c(variables$variable_list[[name]], node)
    variables$names <- c(variables$names, node$left_node$name)
  } else if (inherits(node, "binary_node") && inherits(node$left_node, "binary_node")) {
    if (node$left_node$operator == "type") {
      name <- node$left_node$left_node$name
      node$left_node$left_node$type <- node$left_node$right_node
      variables$variable_list[[name]] <- c(variables$variable_list[[name]], node)
      variables$names <- c(variables$names, node$left_node$left_node$name)
    } else if (node$left_node$operator %in% c("[", "[[")) {
      name <- node$left_node$left_node$name
      variables$variable_list[[name]] <- c(variables$variable_list[[name]], node)
      variables$names <- c(variables$names, node$left_node$left_node$name)
    }
  } else if (inherits(node, "if_node")) {
    gather_vars_and_returns(node$true_node, variables)
    if (!is.null(node$else_if_nodes)) {
      lapply(node$else_if_nodes, function(branch) {
        gather_vars_and_returns(branch, variables)
      })
    }
    if (!is.null(node$false_node)) {
      gather_vars_and_returns(node$false_node, variables)
    }
  } else if (inherits(node, "block_node")) {
    lapply(node$block, function(stmt) gather_vars_and_returns(stmt, variables))
  } else if (inherits(node, "for_node")) {
    name <- node$i$name # TODO: Assuming it is a variable node. Maybe requires a check
    variables$variable_list[[name]] <- c(variables$variable_list[[name]], node)
    variables$names <- c(variables$names, node$i$name)
    lapply(node$block$block, function(stmt) gather_vars_and_returns(stmt, variables))
  }
}

is_iterator <- function(all_types) {
  all_data_structs <- sapply(all_types, \(x) x$data_struct)
  any(all_data_structs == "iterator")
}

infer_common_type <- function(all_types, r_fct, name) {
  all_base_types <- sapply(all_types, \(x) x$base_type)
  all_data_structs <- sapply(all_types, \(x) x$data_struct)
  null_type <- "void"
  if (r_fct) null_type <- "R_NilValue"

  base_type <- null_type
  if (any(all_base_types == "logical")) {
    base_type <- "logical"
  }
  if (any(all_base_types %in% c("int", "integer"))) {
    base_type <- "integer"
  }
  if (any(all_base_types == "double")) {
    base_type <- "double"
  }
  data_struct <- "scalar"
  if (any(all_data_structs %in% c("vec", "vector"))) {
    data_struct <- "vector"
  }
  if (any(all_data_structs %in% c("mat", "matrix"))) {
    data_struct <- "matrix"
  }
  t <- type_node$new(name, FALSE, r_fct)
  t$base_type <- base_type
  t$data_struct <-  data_struct
  t
}

# Variable class
# ========================================================================
# - gathers variables in variable_list
# - run checks for types
# - Infer missing types
# - gather also the return nodes
variables <- R6::R6Class(
  "variables",
  public = list(
    r_fct = NULL,
    variable_list = list(),
    names = c(),
    variable_type_list = list(),

    return_list = list(),

    errors = NULL,

    initialize = function(f, f_args, r_fct) {
      parsed_args <- parse_input_args(f, f_args, r_fct)
      self$r_fct <- r_fct
      arguments <- formals(f) |> names() |> as.character()
      all_vars <- c(arguments, all.vars(body(f)))
      all_vars <- setdiff(all_vars, permitted_base_types()) # TODO: add check that permitted_base_types are not used as variables
      self$variable_list <- rep(list(NULL), length(all_vars))
      names(self$variable_list) <- all_vars

      self$variable_type_list <- self$variable_list

      names <- sapply(parsed_args, \(x) {
        str2lang(x$name)})
      names(parsed_args) <- arguments
      lapply(arguments, function(name) {
        self$variable_list[[name]] <- c(parsed_args[[name]]) # NOTE: the wrap in c is necessary so that all entries are lists!
        self$variable_type_list[[name]] <- parsed_args[[name]]
      })
      # names is incrementaly updated.
      self$names <- sapply(parsed_args, function(x) str2lang(x$name))
    },

    # Determine types
    # ========================================================================
    # TODO: handle for node; if node and block node
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
        name <- node$name
        if (is.symbol(node$name)) {
          name <- deparse(node$name)
        }
        return(self$variable_type_list[[str2lang(name)]]$type) # TODO: is a check needed that the variable possesses a type?
      } else if (inherits(node, "unary_node")) {
        if (node$operator == "print") {
          self$errors <- c(self$errors, sprintf("Found print within an expression: %s", node$stringify()))
        } else if (node$operator == "return") {
          if (node$context == "Start") {
            return(self$flatten_type(self$determine_types_rhs(node$obj)))
          } else {
            self$errors <- c(self$errors, sprintf("Found return within an expressio: %s", node$stringify()))
          }
        } else if (node$operator %in% c("sin", "asin", "sinh", "cos", "acos", "cosh", "tan", "atan", "tanh", "log", "sqrt", "exp")) {
          inner_type <- self$determine_types_rhs(node$obj)
          inner_type <- self$flatten_type(inner_type)
          t <- type_node$new(NA, FALSE, self$r_fct)
          t$base_type <- "double"
          t$data_struct <- inner_type$data_struct
          return(t)
        } else if (node$operator == "-") {
          inner_type <- self$determine_types_rhs(node$obj)
          inner_type <- self$flatten_type(inner_type)
          base_type <- inner_type$base_type
          if (base_type == "logical") base_type <- "int" # TODO: inform user about type promotion
          t <- type_node$new(NA, FALSE, self$r_fct)
          t$base_type <- base_type
          t$data_struct <- inner_type$data_struct
          return(t)
        } else if (node$operator == "(") {
          inner_type <- self$determine_types_rhs(node$obj)
          inner_type <- self$flatten_type(inner_type)
          return(inner_type)
        } else if (node$operator %in% c("length", "dim")) {
          t <- type_node$new(NA, FALSE, self$r_fct)
          t$base_type <- "integer"
          t$data_struct <- "vector"
          return(t)
        } else if (node$operator %in% c("is.na", "is.finite", "is.infinite")) {
          inner_type <- self$determine_types_rhs(node$obj)
          inner_type <- self$flatten_type(inner_type)
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
          right_type <- self$determine_types_rhs(node$right_node)
          left_type <- self$flatten_type(left_type)
          right_type <- self$flatten_type(right_type)
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
          right_type <- self$determine_types_rhs(node$right_node)
          left_type <- self$flatten_type(left_type)
          right_type <- self$flatten_type(right_type)
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
          right_type <- self$determine_types_rhs(node$right_node)
          left_type <- self$flatten_type(left_type)
          right_type <- self$flatten_type(right_type)
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
          left_type_node <- self$flatten_type(left_type_node)
          t <- type_node$new(NA, FALSE, self$r_fct)
          t$base_type <- left_type_node
          t$data_struct <- "vector"
          return(self$flatten_type(t)) # TODO: add this flattening step everywhere
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
          type_first_arg <- self$flatten_type(type_first_arg)
          t <- type_node$new(NA, FALSE, self$r_fct)
          t$base_type <- type_first_arg
          t$data_struct <- "matrix"
          return(t)
        } else if (node$operator == "c") {
          types_of_args <- lapply(node$args, function(x) {
            temp <- self$determine_types_rhs(x)
            self$flatten_type(temp)
          })
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
          type_first_arg <- self$flatten_type(type_first_arg)
          t <- type_node$new(NA, FALSE, self$r_fct)
          t$base_type <- type_first_arg
          t$data_struct <- "matrix"
          return(t)
        }
      } else if (inherits(node, "for_node")) {
        type_seq <- self$determine_types_rhs(node$seq)
        type_seq <- self$flatten_type(type_seq)
        t <- type_node$new(NA, FALSE, self$r_fct)
        t$base_type <- type_seq$base_type
        t$data_struct <- "iterator"
        return(t)
      } else {
        self$errors <- c(self$errors, sprintf("Cannot determine the type for: %s", node$stringify()))
        stop()
      }
    },

    flatten_type = function(type) {
      if (inherits(type$base_type, "type_node")) {
        type$base_type <- type$base_type$base_type
        type <- self$flatten_type(type)
      }
      if (inherits(type$data_struct, "type_node")) {
        type$data_struct <- type$data_struct$data_struct
        type <- self$flatten_type(type)
      }
      return(type)
    },

    # Infer types
    # ========================================================================
    determine_type_of_assignment = function(node) {
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
    },
    infer_missing_type = function(nodes, name) {
      temp_l <- lapply(nodes, function(x) {
        if (inherits(x, "binary_node") && x$operator %in% c("<-", "=")) {
          x <- x$right_node
        }
        self$determine_types_rhs(x)
      })
      t <- infer_common_type(temp_l, self$r_fct, name)
      t$init()
      if (is_iterator(temp_l)) t$iterator <- TRUE
      v <- variable_node$new(name)
      v$type <- t
      self$variable_type_list[[name]] <- v
    },
    infer_types = function() {
      names <- unique(self$names)
      for (name in names) {
        nodes <- self$variable_list[[name]]
        assignment_types <- lapply(nodes, self$determine_type_of_assignment)
        num_declarations <- sum(assignment_types %in% c("function_argument", "declaration", "assign_to_declaration"))
        if (num_declarations == 0) {
          self$infer_missing_type(nodes, name)
        }
      }
    },
    # Check types
    # ========================================================================
    # - Check that used type is valid; --> Done during action_error
    # - Check that for each variable only one unique type is declared
    # - Check that for each variable only once a type is declared
    # - Check that the type declaration occures at the first occurance of the variable
    # - Check that the iterator variables are not used elsewhere
    # - TODO: check that the variables are used in the correct context. 
    #   - Types of lhs matches at least nearly the type of rhs
    #   - Types of variables appropriate as function argument
    check = function() {
      names <- unique(self$names)

      for (name in names) {
        nodes <- self$variable_list[[name]]

        if (is.null(nodes)) next # TODO: requires this a check? Is this case realistic?
        assignment_types <- lapply(nodes, self$determine_type_of_assignment)

        # Check: how many type declarations?
        num_declarations <- sum(assignment_types %in% c("function_argument", "declaration", "assign_to_declaration"))
        declaration_indices <- which(assignment_types %in% c("function_argument", "declaration", "assign_to_declaration"))

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

  )
)
