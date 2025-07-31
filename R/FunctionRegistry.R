# Singleton class holding all information of a function required
# ========================================================================
Functions <- R6::R6Class(
  "Functions",
  public = list(
    function_names = NULL,
    number_of_args = list(),
    type_check_fcts = NULL,
    is_infixs = NULL,
    groups = NULL,
    cpp_names = NULL,

    initialize = function() {},
    add = function(name, num_args,
                   check_fct, is_infix, group, cpp_name) {
      self$function_names <- c(self$function_names, name)
      self$number_of_args <- c(self$number_of_args, list(num_args))
      self$type_check_fcts <- c(self$type_check_fcts, check_fct)
      self$is_infixs <- c(self$is_infixs, is_infix)
      self$groups <- c(self$groups, group)
      self$cpp_names <- c(self$cpp_names, cpp_name)
    },

    permitted_fcts = function() self$function_names,
    expected_n_args = function(name) {
      self$number_of_args[which(self$function_names == name)]
    },
    check_fct = function(name) {
      self$type_check_fcts[which(self$function_names == name)]
    },
    get_cpp_name = function(name) {
      self$cpp_names[which(self$function_names == name)]
    },
    is_infix = function(name) {
      all(self$is_infixs[which(self$cpp_names == name)])
      # cpp names have to be used as this is called after the translation.
      # all is used due to duplicates in the cpp code (=)
    },
    is_group_functions = function(name) {
      idx <- which(self$function_names == name)
      if (identical(idx, integer())) {
        return(FALSE)
      }
      self$groups[which(self$function_names == name)] == "function_node"
    }
  )
)
function_registry_global <- Functions$new()
mock <- function(node, vars_types_list) {}

is_char <- function(node, vars_types_list) {
  if (inherits(node, "variable_node")) {
    type <- vars_types_list[[node$name]]
    if (type$base_type == "character") {
      return(TRUE)
    }
  }
  if (inherits(node, "literal_node")) {
    type <- determine_literal_type(node$name)
    if (type == "character") {
      return(TRUE)
    }
  }
  FALSE
}
is_vec_or_mat <- function(var_name, vars_types_list) {
  type <- vars_types_list[[var_name]]
  if (type$data_struct %in% c("matrix", "vector", "vec", "mat")) {
    return(TRUE)
  }
  FALSE
}
is_vec <- function(var_name, vars_types_list) {
  type <- vars_types_list[[var_name]]
  if (type$data_struct %in% c("vector", "vec")) {
    return(TRUE)
  }
  FALSE
}

function_registry_global$add(
  name = "type", num_args = 2,
  check_fct = function(node, vars_types_list) {
    if (!(inherits(node$left_node, "variable_node") &&
      !(inherits(node$right_node, "variable_node")) || inherits(node$right_node, "binary_node"))) {
      node$error <- error$new("the type function expects a variable as first argument and either a symbol or a function such as vec(double)")
    }
  },
  is_infix = FALSE, group = "binary_node", cpp_name = "type" # the removement of the type is handled in the node itself
)
function_registry_global$add(
  name = "=", num_args = 2,
  check_fct = function(node, vars_types_list) {
    if (!(node$context %in% c("<-", "=", "{"))) {
      node$error <- error$new("assignments cannot be done within another function")
    }
  },
  is_infix = TRUE,
  group = "binary_node", cpp_name = "="
)
function_registry_global$add(
  name = "<-", num_args = 2,
  check_fct = function(node, vars_types_list) {
    if (!(node$context %in% c("<-", "=", "{"))) {
      node$error <- error$new("assignments cannot be done within another function")
    }
  },
  is_infix = TRUE,
  group = "binary_node", cpp_name = "="
)
function_registry_global$add(
  name = "[", num_args = 2,
  check_fct = function(node, vars_types_list) {
    if (!is_vec_or_mat(find_var_lhs(node), vars_types_list)) {
      node$error <- error$new("You can only subset variables of type matrix or vector")
    }
    if (is_char(node$right_node, vars_types_list)) {
      node$error <- error$new("You cannot use character variables or literals for subsetting")
    }
  },
  is_infix = FALSE, group = "binary_node", cpp_name = "etr::subset"
)
function_registry_global$add(
  name = "at", num_args = 2,
  check_fct = function(node, vars_types_list) {
    if (!is_vec_or_mat(find_var_lhs(node), vars_types_list)) {
      node$error <- error$new("You can only subset variables of type matrix or vector")
    }
    if (is_char(node$right_node, vars_types_list)) {
      node$error <- error$new("You cannot use character variables or literals for subsetting")
    }
  },
  is_infix = FALSE, group = "binary_node", cpp_name = "etr::at"
)
function_registry_global$add(
  name = "[[", num_args = 2,
  check_fct = function(node, vars_types_list) {
    if (!is_vec_or_mat(find_var_lhs(node), vars_types_list)) {
      node$error <- error$new("You can only subset variables of type matrix or vector")
    }
    if (is_char(node$right_node, vars_types_list)) {
      node$error <- error$new("You cannot use character variables or literals for subsetting")
    }
  },
  is_infix = FALSE, group = "binary_node", cpp_name = "etr::at"
)
function_registry_global$add(
  name = "for", num_args = 3,
  check_fct = function(node, vars_types_list) {
    if (!inherits(node$i, "variable_node")) {
      node$error <- error$new(sprintf("The index variable cannot be of type %s", class(node$i)))
    }
  },
  is_infix = FALSE, group = "for_node", cpp_name = "for"
)
function_registry_global$add(
  name = "while", num_args = 2,
  check_fct = mock, is_infix = FALSE,
  group = "function_node", cpp_name = "while"
)
function_registry_global$add(
  name = "next", num_args = 0,
  check_fct = mock, is_infix = FALSE,
  group = "nullary_node", cpp_name = "continue"
)
function_registry_global$add(
  name = "break", num_args = 0,
  check_fct = mock, is_infix = FALSE,
  group = "nullary_node", cpp_name = "break"
)
function_registry_global$add(
  name = "c", num_args = NA,
  check_fct = function(node, vars_types_list) {
    for (i in seq_along(node$args)) {
      if (is_char(node$args[[i]], vars_types_list)) {
        node$error <- error$new("You cannot use character entries in c")
        return()
      }
    }
  },
  is_infix = FALSE, group = "function_node", cpp_name = "etr::c"
)
function_registry_global$add(
  name = ":", num_args = 2,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$left_node, vars_types_list)) {
      node$error <- error$new("You cannot use character entries in :")
    }
    if (is_char(node$right_node, vars_types_list)) {
      node$error <- error$new("You cannot use character entries in :")
    }
  },
  is_infix = FALSE, group = "binary_node", cpp_name = "etr::colon"
)
function_registry_global$add(
  name = "sin", num_args = 1,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$obj, vars_types_list)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::sinus"
)
function_registry_global$add(
  name = "asin", num_args = 1,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$obj, vars_types_list)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::asinus"
)
function_registry_global$add(
  name = "sinh", num_args = 1,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$obj, vars_types_list)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::sinush"
)
function_registry_global$add(
  name = "cos", num_args = 1,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$obj, vars_types_list)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::cosinus"
)
function_registry_global$add(
  name = "acos", num_args = 1,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$obj, vars_types_list)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::acosinus"
)
function_registry_global$add(
  name = "cosh", num_args = 1,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$obj, vars_types_list)) {
      node$error <- error::new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::cosinush"
)
function_registry_global$add(
  name = "tan", num_args = 1,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$obj, vars_types_list)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::tangens"
)
function_registry_global$add(
  name = "atan", num_args = 1,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$obj, vars_types_list)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::atangens"
)
function_registry_global$add(
  name = "tanh", num_args = 1,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$obj, vars_types_list)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::tangensh"
)
function_registry_global$add(
  name = "log", num_args = 1,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$obj, vars_types_list)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::ln"
)
function_registry_global$add(
  name = "sqrt", num_args = 1,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$obj, vars_types_list)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::sqroot"
)
function_registry_global$add(
  name = "exp", num_args = 1,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$obj, vars_types_list)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::exp"
)
function_registry_global$add(
  name = "^", num_args = 2,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$left_node, vars_types_list)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
    if (is_char(node$right_node, vars_types_list)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = TRUE, group = "binary_node", cpp_name = "etr::power"
)
function_registry_global$add(
  name = "+", num_args = 2,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$left_node, vars_types_list)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
    if (is_char(node$right_node, vars_types_list)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = TRUE, group = "binary_node", cpp_name = "+"
)
function_registry_global$add(
  name = "-", num_args = c(1, 2),
  check_fct = function(node, vars_types_list) {
    if (is_char(node$left_node, vars_types_list)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
    if (is_char(node$right_node, vars_types_list)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = TRUE, group = "binary_node", cpp_name = "-"
)
function_registry_global$add(
  name = "*", num_args = 2,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$left_node, vars_types_list)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
    if (is_char(node$right_node, vars_types_list)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = TRUE, group = "binary_node", cpp_name = "*"
)
function_registry_global$add(
  name = "/", num_args = 2,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$left_node, vars_types_list)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
    if (is_char(node$right_node, vars_types_list)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = TRUE, group = "binary_node", cpp_name = "/"
)
function_registry_global$add(
  name = "if", num_args = NA,
  check_fct = mock, is_infix = FALSE, group = "if_node", cpp_name = "if"
)
function_registry_global$add(
  name = "{", num_args = 1,
  check_fct = mock, is_infix = FALSE, group = "block_node", cpp_name = "{"
)
function_registry_global$add(
  name = "(", num_args = 1,
  check_fct = mock, is_infix = FALSE, group = "unary_node", cpp_name = "("
)
function_registry_global$add(
  name = "==", num_args = 2,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$left_node, vars_types_list)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
    if (is_char(node$right_node, vars_types_list)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = TRUE, group = "binary_node", cpp_name = "=="
)
function_registry_global$add(
  name = "!=", num_args = 2,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$left_node, vars_types_list)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
    if (is_char(node$right_node, vars_types_list)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = TRUE, group = "binary_node", cpp_name = "!="
)
function_registry_global$add(
  name = ">", num_args = 2,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$left_node, vars_types_list)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
    if (is_char(node$right_node, vars_types_list)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = TRUE, group = "binary_node", cpp_name = ">"
)
function_registry_global$add(
  name = ">=", num_args = 2,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$left_node, vars_types_list)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
    if (is_char(node$right_node, vars_types_list)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = TRUE, group = "binary_node", cpp_name = ">="
)
function_registry_global$add(
  name = "<", num_args = 2,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$left_node, vars_types_list)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
    if (is_char(node$right_node, vars_types_list)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = TRUE, group = "binary_node", cpp_name = "<"
)
function_registry_global$add(
  name = "<=", num_args = 2,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$left_node, vars_types_list)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
    if (is_char(node$right_node, vars_types_list)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = TRUE, group = "binary_node", cpp_name = "<="
)
function_registry_global$add(
  name = "&&", num_args = 2,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$left_node, vars_types_list)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
    if (is_char(node$right_node, vars_types_list)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = TRUE, group = "binary_node", cpp_name = "&&"
)
function_registry_global$add(
  name = "||", num_args = 2,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$left_node, vars_types_list)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
    if (is_char(node$right_node, vars_types_list)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = TRUE, group = "binary_node", cpp_name = "||"
)
function_registry_global$add(
  name = "print", num_args = 1,
  check_fct = mock, is_infix = FALSE, group = "unary_node", cpp_name = "etr::print"
)
function_registry_global$add(
  name = "return", num_args = c(0, 1),
  check_fct = mock, is_infix = FALSE, group = "unary_node", cpp_name = "return"
)
function_registry_global$add(
  name = "vector", num_args = 2,
  check_fct = function(node, vars_types_list) {
    if (!is_char(node$args[[1]], vars_types_list)) {
     node$error <- error$new("mode of vector has to be of type character")
    }
  },
  is_infix = FALSE, group = "function_node", cpp_name = "etr::vector"
)
function_registry_global$add(
  name = "numeric", num_args = 1,
  check_fct = mock, is_infix = FALSE, group = "unary_node", cpp_name = "etr::numeric"
)
function_registry_global$add(
  name = "matrix", num_args = 3,
  check_fct = function(node, vars_types_list) {
    if (is_char(node$args[[1]], vars_types_list)) {
     node$error <- error$new("You cannot fill a matrix with character entries")
    }
  },
  is_infix = FALSE, group = "function_node", cpp_name = "etr::matrix"
)
function_registry_global$add(
  name = "length", num_args = 1,
  check_fct = function(node, vars_types_list) {
    if (!is_vec_or_mat(node$obj, vars_types_list)) {
      node$error <- error$new("You can only call length on variables of type matrix or vector")
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::length"
)
function_registry_global$add(
  name = "dim", num_args = 1,
  check_fct = function(node, vars_types_list) {
    if (!is_vec_or_mat(node$obj, vars_types_list)) {
      node$error <- error$new("You can only call dim on variables of type matrix or vector")
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::dim"
)
function_registry_global$add(
  name = "!", num_args = 1,
  check_fct = function(node, vars_types_list) {
    if (!is_vec_or_mat(node$obj, vars_types_list)) {
      node$error <- error$new("You can only call dim on variables of type matrix or vector")
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "!"
)
function_registry_global$add(
  name = "is.na", num_args = 1,
  check_fct = function(node, vars_types_list) {
    if (!is_vec_or_mat(node$obj, vars_types_list)) {
      node$error <- error$new("You can only call dim on variables of type matrix or vector")
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::isNA"
)
function_registry_global$add(
  name = "is.infinite", num_args = 1,
  check_fct = function(node, vars_types_list) {
    if (!is_vec_or_mat(node$obj, vars_types_list)) {
      node$error <- error$new("You can only call dim on variables of type matrix or vector")
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::isInfinite"
)
function_registry_global$add(
  name = "is.finite", num_args = 1,
  check_fct = function(node, vars_types_list) {
    if (!is_vec_or_mat(node$obj, vars_types_list)) {
      node$error <- error$new("You can only call dim on variables of type matrix or vector")
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::isFinite"
)
function_registry_global$add(
  name = "cmr", num_args = 3,
  check_fct = function(node, vars_types_list) {
    if (!is_vec(node$args[[2]], vars_types_list)) {
      node$error <- error$new("The second argument to cmr has to be a vector")
    }
    if (!is_vec(node$args[[3]], vars_types_list)) {
      node$error <- error$new("The third argument to cmr has to be a vector")
    }
    if (inherits(node, "variable_node")) {
      type <- vars_types_list$variable_type_list[[str2lang(node$args[[2]]$name)]]$type
      if (type$base_type != "double") {
        node$error <- error$new("Vectors of second argument to cmr does not contain doubles")
      }
    }
    if (inherits(node, "variable_node")) {
      type <- vars_types_list$variable_type_list[[str2lang(node$args[[3]]$name)]]$type
      if (type$base_type != "double") {
        node$error <- error$new("Vectors of third argument to cmr does not contain doubles")
      }
    }
  },
  is_infix = FALSE, group = "function_node", cpp_name = "etr::cmr"
)
