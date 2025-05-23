# List of C++ keywords
cpp_keywords <- function() {
  c(
    "alignas", "alignof", "and", "and_eq", "asm",
    "atomic_cancel", "atomic_commit", "atomic_noexcept",
    "auto", "bitand", "bitor", "bool", "break", "case",
    "catch", "char", "char8_t", "char16_t", "char32_t",
    "class", "compl", "concept", "const", "consteval", "constexpr",
    "constinit", "const_cast", "continue", "contract_assert",
    "co_await", "co_return", "co_yield", "decltype", "default", "delete", "do",
    "double", "dynamic_cast", "else", "enum", "explicit",
    "export", "extern", "false", "float", "for", "friend",
    "goto", "if", "inline", "int", "long",
    "mutable", "namespace", "new", "noexcept", "not", "not_eq",
    "nullptr", "operator", "or", "or_eq", "private",
    "protected", "public", "reflexpr", "register",
    "reinterpret_cast", "requires",
    "return", "short", "signed", "sizeof", "static",
    "static_assert", "static_cast", "struct",
    "switch", "synchronized", "template",
    "this", "thread_local", "throw", "true", "try", "typedef",
    "typeid", "typename", "union", "unsigned", "using",
    "virtual", "void", "volatile", "wchar_t", "while", "xor", "xor_eq"
  )
}

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
      # NOTE: cpp names have to be used as this is called after the translation.
      # all is used due to duplicates in the cpp code (=)
    },
    is_group_functions = function(name) {
      self$groups[which(self$function_names == name)] == "function_node"
    }
  )
)
function_registry_global <- Functions$new()
mock <- function(node, variables) {}

is_char <- function(node, variables) {
  if (inherits(node, "variable_node")) {
    type <- variables$variable_type_list[[node$name]]$type
    if (type$base_type == "character") {
      return(TRUE)
      # TODO: check whether string can be assigned
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
is_vec_or_mat <- function(node, variables) {
  if (inherits(node, "variable_node")) {
    type <- variables$variable_type_list[[node$name]]$type
    if (type$data_struct %in% c("matrix", "vector")) {
      return(TRUE)
    }
  }
  FALSE
}
is_vec <- function(node, variables) {
  if (inherits(node, "variable_node")) {
    type <- variables$variable_type_list[[node$name]]$type
    if (type$data_struct == "vector") {
      return(TRUE)
    }
  }
  FALSE
}

function_registry_global$add(
  name = "type", num_args = 2,
  check_fct = function(node, variables) {
    if (!(inherits(node$left_node, "variable_node") && inherits(node$right_node, "variable_node"))) {
      node$error <- error$new("the type function expects two variables (symbols) as arguments")
    }
  },
  is_infix = FALSE, group = "binary_node", cpp_name = ""
)
function_registry_global$add(
  name = "=", num_args = 2,
  check_fct = mock, is_infix = TRUE,
  group = "binary_node", cpp_name = "="
)
function_registry_global$add(
  name = "<-", num_args = 2,
  check_fct = mock, is_infix = TRUE,
  group = "binary_node", cpp_name = "="
)
function_registry_global$add(
  name = "[", num_args = 2,
  check_fct = function(node, variables) {
    if (!is_vec_or_mat(node$left_node, variables)) {
      node$error <- error$new("You can only subset variables of type matrix or vector")
    }
    if (is_char(node$right_node)) {
      node$error <- error$new("You cannot use character variables or literals for subsetting")
    }
  },
  is_infix = FALSE, group = "binary_node", cpp_name = "etr::subset"
)
function_registry_global$add(
  name = "at", num_args = 2,
  check_fct = function(node, variables) {
    if (!is_vec_or_mat(node$left_node, variables)) {
      node$error <- error$new("You can only subset variables of type matrix or vector")
    }
    if (is_char(node$right_node)) {
      node$error <- error$new("You cannot use character variables or literals for subsetting")
    }
  },
  is_infix = FALSE, group = "binary_node", cpp_name = "etr::at"
)
function_registry_global$add(
  name = "[[", num_args = 2,
  check_fct = function(node, variables) {
    if (!is_vec_or_mat(node$left_node, variables)) {
      node$error <- error$new("You can only subset variables of type matrix or vector")
    }
    if (is_char(node$right_node)) {
      node$error <- error$new("You cannot use character variables or literals for subsetting")
    }
  },
  is_infix = FALSE, group = "binary_node", cpp_name = "etr::at"
)
function_registry_global$add(
  name = "for", num_args = 3,
  check_fct = function(node, variables) {
    if (!inherits(node$i, "variable_node")) {
      node$error <- error$new(sprintf("The index variable cannot be of type %s", class(node$i)))
    }
  },
  is_infix = FALSE, group = "for_node", cpp_name = "for"
)
function_registry_global$add(
  name = "while", num_args = 2,
  check_fct = mock, is_infix = FALSE,
  group = "function_node", cpp_name = "while" # TODO: require this a own node?
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
  check_fct = function(node, variables) {
    for (i in seq_along(node$args)) {
      if (is_char(node$args[[i]], variables)) {
        node$error <- error$new("You cannot use character entries in c")
        return()
      }
    }
  },
  is_infix = FALSE, group = "function_node", cpp_name = "etr::c"
)
function_registry_global$add(
  name = ":", num_args = 2,
  check_fct = function(node, variables) {
    if (is_char(node$left_node, variables)) {
      node$error <- error$new("You cannot use character entries in :")
    }
    if (is_char(node$right_node, variables)) {
      node$error <- error$new("You cannot use character entries in :")
    }
  },
  is_infix = FALSE, group = "binary_node", cpp_name = "etr::colon"
)
function_registry_global$add(
  name = "sin", num_args = 1,
  check_fct = function(node, variables) {
    if (is_char(node$obj, variables)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::sinus"
)
function_registry_global$add(
  name = "asin", num_args = 1,
  check_fct = function(node, variables) {
    if (is_char(node$obj, variables)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::asinus"
)
function_registry_global$add(
  name = "sinh", num_args = 1,
  check_fct = function(node, variables) {
    if (is_char(node$obj, variables)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::sinush"
)
function_registry_global$add(
  name = "cos", num_args = 1,
  check_fct = function(node, variables) {
    if (is_char(node$obj, variables)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::cosinus"
)
function_registry_global$add(
  name = "acos", num_args = 1,
  check_fct = function(node, variables) {
    if (is_char(node$obj, variables)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::acosinus"
)
function_registry_global$add(
  name = "cosh", num_args = 1,
  check_fct = function(node, variables) {
    if (is_char(node$obj, variables)) {
      node$error <- error::new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::cosinush"
)
function_registry_global$add(
  name = "tan", num_args = 1,
  check_fct = function(node, variables) {
    if (is_char(node$obj, variables)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::tangens"
)
function_registry_global$add(
  name = "atan", num_args = 1,
  check_fct = function(node, variables) {
    if (is_char(node$obj, variables)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::atangens"
)
function_registry_global$add(
  name = "tanh", num_args = 1,
  check_fct = function(node, variables) {
    if (is_char(node$obj, variables)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::tangensh"
)
function_registry_global$add(
  name = "log", num_args = 1,
  check_fct = function(node, variables) {
    if (is_char(node$obj, variables)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::ln" # TODO: requires change to log
)
function_registry_global$add(
  name = "sqrt", num_args = 1,
  check_fct = function(node, variables) {
    if (is_char(node$obj, variables)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::sqroot"
)
function_registry_global$add(
  name = "exp", num_args = 1,
  check_fct = function(node, variables) {
    if (is_char(node$obj, variables)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::exp"
)
function_registry_global$add(
  name = "^", num_args = 2,
  check_fct = function(node, variables) {
    if (is_char(node$left_node, variables)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
    if (is_char(node$right_node, variables)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = TRUE, group = "binary_node", cpp_name = "etr::power" # TODO: is infix argument used during translation to C++?
)
function_registry_global$add(
  name = "+", num_args = 2,
  check_fct = function(node, variables) {
    if (is_char(node$left_node, variables)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
    if (is_char(node$right_node, variables)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = TRUE, group = "binary_node", cpp_name = "+"
)
function_registry_global$add(
  name = "-", num_args = c(1, 2),
  check_fct = function(node, variables) {
    if (is_char(node$left_node, variables)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
    if (is_char(node$right_node, variables)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = TRUE, group = "binary_node", cpp_name = "-"
)
function_registry_global$add(
  name = "*", num_args = 2,
  check_fct = function(node, variables) {
    if (is_char(node$left_node, variables)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
    if (is_char(node$right_node, variables)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = TRUE, group = "binary_node", cpp_name = "*"
)
function_registry_global$add(
  name = "/", num_args = 2,
  check_fct = function(node, variables) {
    if (is_char(node$left_node, variables)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
    if (is_char(node$right_node, variables)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = TRUE, group = "binary_node", cpp_name = "/"
)
# NOTE: somewhen add checks that it evaluates to logical
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
  check_fct = function(node, variables) {
    if (is_char(node$left_node, variables)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
    if (is_char(node$right_node, variables)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = TRUE, group = "binary_node", cpp_name = "=="
)
function_registry_global$add(
  name = "!=", num_args = 2,
  check_fct = function(node, variables) {
    if (is_char(node$left_node, variables)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
    if (is_char(node$right_node, variables)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = TRUE, group = "binary_node", cpp_name = "!="
)
function_registry_global$add(
  name = ">", num_args = 2,
  check_fct = function(node, variables) {
    if (is_char(node$left_node, variables)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
    if (is_char(node$right_node, variables)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = TRUE, group = "binary_node", cpp_name = ">"
)
function_registry_global$add(
  name = ">=", num_args = 2,
  check_fct = function(node, variables) {
    if (is_char(node$left_node, variables)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
    if (is_char(node$right_node, variables)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = TRUE, group = "binary_node", cpp_name = ">="
)
function_registry_global$add(
  name = "<", num_args = 2,
  check_fct = function(node, variables) {
    if (is_char(node$left_node, variables)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
    if (is_char(node$right_node, variables)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = TRUE, group = "binary_node", cpp_name = "<"
)
function_registry_global$add(
  name = "<=", num_args = 2,
  check_fct = function(node, variables) {
    if (is_char(node$left_node, variables)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
    if (is_char(node$right_node, variables)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = TRUE, group = "binary_node", cpp_name = "<="
)
# TODO: add & and |
function_registry_global$add(
  name = "&&", num_args = 2,
  check_fct = function(node, variables) {
    if (is_char(node$left_node, variables)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
    if (is_char(node$right_node, variables)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
  },
  is_infix = TRUE, group = "binary_node", cpp_name = "&&"
)
function_registry_global$add(
  name = "||", num_args = 2,
  check_fct = function(node, variables) {
    if (is_char(node$left_node, variables)) {
      node$error <- error$new(sprintf("You cannot use character entries in %s", node$operator))
    }
    if (is_char(node$right_node, variables)) {
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
  check_fct = function(node, variables) {
    if (!is_char(node$left_node)) {
     node$error <- error$new("mode of vector has to be of type character")
    }
  },
  is_infix = FALSE, group = "function_node", cpp_name = "etr::vector"
)
function_registry_global$add(
  name = "matrix", num_args = 3,
  check_fct = function(node, variables) {
    if (is_char(node$args[[1]])) {
     node$error <- error$new("You cannot fill a matrix with character entries")
    }
  },
  is_infix = FALSE, group = "function_node", cpp_name = "etr::matrix"
)
function_registry_global$add(
  name = "length", num_args = 1,
  check_fct = function(node, variables) {
    if (!is_vec_or_mat(node$obj, variables)) {
      node$error <- error$new("You can only call length on variables of type matrix or vector")
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::length"
)
function_registry_global$add(
  name = "dim", num_args = 1,
  check_fct = function(node, variables) {
    if (!is_vec_or_mat(node$obj, variables)) {
      node$error <- error$new("You can only call dim on variables of type matrix or vector")
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::dim"
)
function_registry_global$add(
  name = "!", num_args = 1,
  check_fct = function(node, variables) {
    if (!is_vec_or_mat(node$obj, variables)) {
      node$error <- error$new("You can only call dim on variables of type matrix or vector")
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "!"
)
function_registry_global$add(
  name = "is.na", num_args = 1,
  check_fct = function(node, variables) {
    if (!is_vec_or_mat(node$obj, variables)) {
      node$error <- error$new("You can only call dim on variables of type matrix or vector")
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::isNA"
)
function_registry_global$add(
  name = "is.infinite", num_args = 1,
  check_fct = function(node, variables) {
    if (!is_vec_or_mat(node$obj, variables)) {
      node$error <- error$new("You can only call dim on variables of type matrix or vector")
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::isInfinite"
)
function_registry_global$add(
  name = "is.finite", num_args = 1,
  check_fct = function(node, variables) {
    if (!is_vec_or_mat(node$obj, variables)) {
      node$error <- error$new("You can only call dim on variables of type matrix or vector")
    }
  },
  is_infix = FALSE, group = "unary_node", cpp_name = "etr::isFinite"
)
function_registry_global$add(
  name = "cmr", num_args = 3,
  check_fct = function(node, variables) {
    if (!is_vec(node$args[[2]], variables)) {
      node$error <- error$new("The second argument to cmr has to be a vector")
    }
    if (!is_vec(node$args[[3]], variables)) {
      node$error <- error$new("The third argument to cmr has to be a vector")
    }
    if (inherits(node, "variable_node")) {
      type <- variables$variable_type_list[[str2lang(node$args[[2]]$name)]]$type
      if (type$base_type != "double") {
        node$error <- error$new("Vectors of second argument to cmr does not contain doubles")
      }
    }
    if (inherits(node, "variable_node")) {
      type <- variables$variable_type_list[[str2lang(node$args[[3]]$name)]]$type
      if (type$base_type != "double") {
        node$error <- error$new("Vectors of third argument to cmr does not contain doubles")
      }
    }
  },
  is_infix = FALSE, group = "function_node", cpp_name = "etr::cmr"
)

# Named arguments
# TODO: add this also into function_registry_global
named_args <- function() {
  list(
    "vector" = c("mode", "length"),
    "matrix" = c("data", "nrow", "ncol")
  )
}

permitted_base_types <- function() {
  c(
    "logical", "integer", "double",
    "int"
  )
}

permitted_data_structs <- function(r_fct) {
  if (r_fct) {
    c(
      "scalar", "vec", "mat",
      "vector", "matrix"
    )
  } else {
    c(
      "scalar", "vec", "mat",
      "vector", "matrix",
      "borrow_vec", "borrow_vector",
      "borrow_mat", "borrow_matrix"
    )
  }
}

convert_types_to_etr_types <- function(base_type, data_struct, r_fct, indent = "") {
  if (data_struct == "scalar") {
    list(
      "void" = "void",
      "R_NilValue" = "R_NilValue",
      "logical" = "bool",
      "integer" = "int", "int" = "int",
      "double" = "double")[base_type]
  } else if (data_struct %in% c("vector", "matrix", "vec", "mat")) {
    data_struct <- c(vector = "etr::Vec", vec = "etr::Vec", matrix = "etr::Mat", mat = "etr::Mat")[data_struct]
    return(paste0(indent, data_struct, "<", base_type, ">"))
  } else if (data_struct %in% c("borrow_vector", "borrow_matrix", "borrow_vec", "borrow_mat") && !r_fct) {
    data_struct <- c(borrow_vector = "etr::Vec", borrow_vec = "etr::Vec",  borrow_mattrix = "etr::Mat", borrow_mat = "etr::Mat")[data_struct]
    return(paste0(indent, data_struct, "<", base_type, ", etr::Borrow<", base_type, ">>"))
  } else if (data_struct %in% c("borrow_vector", "borrow_matrix", "borrow_vec", "borrow_mat") && !r_fct) {
    data_struct <- c(vector = "etr::Vec", vec = "etr::Vec", matrix = "etr::Mat", mat = "etr::Mat")[data_struct]
    return(paste0(indent, data_struct, "<", base_type, ", etr::BorrowSEXP<", base_type, ">>"))
  }
}

# Function to combine strings
combine_strings <- function(string_list, collapse = "\n") {
  paste0(string_list, collapse = collapse)
}
generate_new_names <- function(names, extension, all_vars) {
  sapply(names, function(x) {
    new_x <- paste0(x, extension)
    counter <- 1
    while(new_x %in% all_vars) {
      new_x <- paste0(x, extension)
      counter <- counter + 1
      if (counter == 10) {
        stop("Cannot generate a new name")
      }
    }
    return(new_x)
  })
}

remove_blank_lines <- function(chars) {
  chars <- strsplit(chars, split = "\n")[[1]]
  empty_line <- function(line) {
    line == "\t" || line == ""
  }
  Filter(Negate(empty_line), chars) |> combine_strings("\n")
}

r_fct_sig <- function() {
  combine_strings(
    c("// [[Rcpp::depends(ast2ast)]]",
      "// [[Rcpp::depends(RcppArmadillo)]]",
      "// [[Rcpp::plugins(cpp2a)]]",
      '#include "etr.hpp"\n',
      "// [[Rcpp::export]]\n"), "\n"
  )
}
xptr_sig <- function() {
  combine_strings(
    c(
      "// [[Rcpp::depends(ast2ast)]]",
      "// [[Rcpp::depends(RcppArmadillo)]]",
      "// [[Rcpp::plugins(cpp2a)]]",
      '#include "etr.hpp"\n',
      "// [[Rcpp::export]]",
      "SEXP getXPtr();\n"
    ),
    "\n"
  )
}
