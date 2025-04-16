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

# Assemble the types
build_types <- function(toa, doa, roa, output) {
  if (length(toa) == 0) {
    return()
  }

  r <- data.frame(r = c(FALSE, TRUE), l = c("", "&"))
  roa <- r[match(roa, r$r), 2]
  types <- character(length = length(toa))
  for (i in seq_along(doa)) {
    if (doa[i] == "scalar") {
      types[i] <- c_string(toa[i], roa[i], "")$value
    } else if (doa[i] == "vector") {
      if (output == "R") {
        types[i] <- "SEXP"
      } else {
        types[i] <- c_string("etr::Vec<", toa[i], ">", roa[i], "")$value
      }
    } else if (doa[i] == "borrow") {
      assert(output == "XPtr")
      types[i] <- c_string(
        "etr::Vec<", toa[i],
        ", etr::Borrow<", toa[i], ">>", roa[i], ""
      )$value
    }
  }
  return(types)
}

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

permitted_fcts <- function() {
  c(
    "type", "=", "<-",
    "[", "at",
    "for", "while", "next", "break",
    "c", ":",
    "sin", "asin", "sinh",
    "cos", "acos", "cosh",
    "tan", "atan", "tanh",
    "log", "sqrt",
    "^", "+", "-", "*", "/",
    "if", "{", "(",
    "==", "!=", ">", ">=", "<", "<=",
    "print", "return",
    "vector", "matrix", "length", "dim",
    "exp", "&&", "||", "!",
    "is.na", "is.infinite", "is.finite",
    "cmr", "power"
  )
}

expected_n_args <- function() {
  setNames(
    c(
      2, 2, 2,
      2, 2,
      3, 2, 0, 0,
      NA, 2,
      rep(1, 3),
      rep(1, 3),
      rep(1, 3),
      rep(1, 2),
      rep(2, 4),
      "MINUS",
      NA, 1, 1,
      rep(2, 6),
      1, NA,
      2, 3, 1, 1,
      1, 2, 2, 1,
      rep(1, 3),
      3, 2
    ),
    c(
      "type", "=", "<-",
      "[", "at",
      "for", "while", "next", "break",
      "c", ":",
      "sin", "asin", "sinh",
      "cos", "acos", "cosh",
      "tan", "atan", "tanh",
      "log", "sqrt",
      "^", "+", "*", "/",
      "-",
      "if", "{", "(",
      "==", "!=", ">", ">=", "<", "<=",
      "print", "return",
      "vector", "matrix", "length", "dim",
      "exp", "&&", "||", "!",
      "is.na", "is.infinite", "is.finite",
      "cmr", "power"
    )
  )
}

expected_type_of_args <- function() {
  list(
    "type" = c("symbol", "symbol"),
    "at" = c("any_except_char", list("integer", "double")),
    "for" = c("symbol", "any_except_char", "any_except_char"),
    "vector" = c("character", "any_except_char"),
    "cmr" = list(
      c("double", "double_vector"),
      "double_vector", "double_vector"
    )
  )
}

# Named arguments
named_args <- function() {
  list(
    "vector" = c("mode", "length"),
    "matrix" = c("data", "nrow", "ncol")
  )
}


# Defines the function with more than two arguments
function_fcts <- function() {
  c("vector", "matrix", "cmr", "c")
}

name_pairs <- function() {
  setNames(
    c(
      "type", "=", "=",
      "etr::subset", "etr::at",
      "for", "while", "continue", "break",
      "etr::coca", "etr::colon",
      "etr::sinus", "etr::asinus", "etr::sinush",
      "etr::cosinus", "etr::acosinus", "etr::cosinush",
      "etr::tangens", "etr::atangens", "etr::tangensh",
      "etr::ln", "etr::sqroot",
      "etr::power", "+", "-", "*", "/",
      "if", "{", "(",
      "==", "!=", ">", ">=", "<", "<=",
      "etr::print", "return",
      "etr::vector", "etr::matrix", "etr::length", "etr::dim",
      "etr::exp", "&&", "||", "!",
      "etr::isNA", "etr::isInfinite", "etr::isFinite",
      "etr::cmr", "etr::power"
    ),
    c(
      "type", "=", "<-",
      "[", "at",
      "for", "while", "next", "break",
      "c", ":",
      "sin", "asin", "sinh",
      "cos", "acos", "cosh",
      "tan", "atan", "tanh",
      "log", "sqrt",
      "^", "+", "-", "*", "/",
      "if", "{", "(",
      "==", "!=", ">", ">=", "<", "<=",
      "print", "return",
      "vector", "matrix", "length", "dim",
      "exp", "&&", "||", "!",
      "is.na", "is.infinite", "is.finite",
      "cmr", "power"
    )
  )
}

# Function to combine strings
combine_strings <- function(string_list, collapse = "\n") {
  paste0(string_list, collapse = collapse)
}

# Function to distinguish between infix and function calls
infix_or_function <- function(operator) {
  infix_list <- c(
    "+", "-", "*", "/", "^", ">", "<", ">=", "<=", "==", "!=",
    "&", "|", "&&", "||", ":", "<-", "=", "^"
  )
  if (operator %in% infix_list) {
    return("infix")
  }
  return("function")
}

permitted_types <- function() {
  c(
    "logical", "integer", "double",
    "logical_vector", "integer_vector", "double_vector",
    # NOTE: short forms
    "l", "i", "d",
    "lv", "iv", "dv"
  )
}

permitted_types_xptr_fct_args <- function() {
  c(
    permitted_types(),
    c(
      "double_borrow", "integer_borrow", "logical_borrow",
      "db", "ib", "lb"
    )
  )
}

permitted_handle_inputs <- function() {
  c("copy", "borrow")
}

convert_types_to_etr_types <- function(type) {
  list(
    "void" = "void",
    "logical" = "bool", "l" = "bool",
    "integer" = "int", "i" = "int",
    "double" = "double", "d" = "double",
    "logical_vector" = "etr::Vec<bool>", "lv" = "etr::Vec<bool>",
    "integer_vector" = "etr::Vec<int>", "iv" = "etr::Vec<int>",
    "double_vector" = "etr::Vec<double>", "dv" = "etr::Vec<double>",
    "logical_borrow" = "etr::Vec<bool, etr::Borrow<bool>>", "lb" = "etr::Vec<bool, etr::Borrow<bool>>",
    "integer_borrow" = "etr::Vec<int, etr::Borrow<int>>", "ib" = "etr::Vec<int, etr::Borrow<int>>",
    "double_borrow" = "etr::Vec<double, etr::Borrow<double>>", "db" = "etr::Vec<double, etr::Borrow<double>>"
  )[[type]]
}

convert_vecs_to_borrows <- function(type) {
  res <- list(
    "logical_vector" = "etr::Vec<bool, etr::Borrow<bool>>", "lv" = "etr::Vec<bool, etr::Borrow<bool>>",
    "integer_vector" = "etr::Vec<int, etr::Borrow<int>>", "iv" = "etr::Vec<int, etr::Borrow<int>>",
    "double_vector" = "etr::Vec<double, etr::Borrow<double>>", "dv" = "etr::Vec<double, etr::Borrow<double>>"
  )
  new_type <- try({
    res[[type]]
  })
  if (inherits(new_type , "try-error")) {
    return(convert_types_to_etr_types(type))
  }
  return(new_type)
}

converter_functions <- function(type) {
  list(
    "logical" = "etr::SEXP2Bool", "integer" = "etr::SEXP2Int", "double" = "etr::SEX2Double",
    "l" = "etr::SEXP2Bool", "i" = "etr::SEXP2Int", "d" = "etr::SEX2Double",
    "lv" = "etr::SEXP2BoolVec",
    "iv" = "etr::SEXP2IntVec",
    "dv" = "etr::SEXP2DoubleVec",
    "logical_vector" = "etr::SEXP2BoolVec",
    "integer_vector" = "etr::SEXP2IntVec",
    "double_vector" = "etr::SEXP2DoubleVec",
    "double_borrow", "integer_borrow", "logical_borrow",
    "lb" = "etr::SEXP2BoolVec",
    "ib" = "etr::SEXP2IntVec",
    "db" = "etr::SEXP2DoubleVec",
    "logical_borrow" = "etr::SEXP2BoolBorrow",
    "integer_borrow" = "etr::SEXP2IntBorrow",
    "double_borrow" = "etr::SEXP2DoubleBorrow"
  )[type]
}

converter_functions_ref <- function(type) {
  res <- list(
    "logical_vector" = "logical_borrow", "lv" = "logical_borrow",
    "integer_vector" = "integer_borrow", "iv" = "integer_borrow",
    "double_vector" = "double_borrow", "dv" = "double_borrow"
  )
  new_type <- try({
    res[[type]]
  })
  if (inherits(new_type , "try-error")) {
    return(converter_functions(type))
  }
  return(converter_functions(new_type))
}

remove_blank_lines <- function(chars) {
  chars <- strsplit(chars, split = "\n")[[1]]
  empty_line <- function(line) {
    line == "\t" || line == ""
  }
  Filter(Negate(empty_line), chars) |> combine_strings("\n")
}

# Stuff to create
# - includes
# - Function signature
# - Variable declarations
r_fct_sig <- function() {
  combine_strings(
    c("// [[Rcpp::depends(ast2ast)]]",
      "// [[Rcpp::depends(RcppArmadillo)]]",
      "// [[Rcpp::plugins(cpp2a)]]",
      '#include "etr.hpp"\n',
      "// [[Rcpp::export]]\n"), "\n"
  )
}
create_signature_r <- function(vars_types_df, name_f) {
  if (is.character(vars_types_df)) {
    signature <- paste0("SEXP ", name_f, "() {", collapse = "")
    return(signature)
  }
  vars_types_df <- vars_types_df[vars_types_df$function_input, ]
  new_names <- vars_types_df$new_name
  if (is.null(new_names) || identical(new_names, character())) {
    signature <- paste0("SEXP ", name_f, "() {", collapse = "")
    return(signature)
  }
  type <- "SEXP"
  args <- paste(type, new_names , collapse = ", ")
  signature <- paste0("SEXP ", name_f, "(", 
    args, ") {", collapse = ""
  )
}
create_variable_declarations_r <- function(vars_types_df) {
  if (class(vars_types_df) == "character") {
    return("")
  }
  res <- apply(vars_types_df, 1, function(i) {
    res <- NULL
    if (i["function_input"]) {
      if (i["are_ref"]) {
        cast_fct <- converter_functions_ref(i["type"])
        t <- convert_vecs_to_borrows(i["type"])
        res <- paste0("\t", t, " ", i["name"], " = ", 
          cast_fct, "(", i["new_name"], ");")
      } else {
        cast_fct <- converter_functions(i["type"])
        t <- convert_types_to_etr_types(i["type"])
        res <- paste0("\t", t, " ", i["name"], " = ", 
          cast_fct, "(", i["new_name"], ");")
      }
    } else {
      t <- i["type"] |> convert_types_to_etr_types()
      n <- i["name"]
      res <- paste0("\t", t, " ", n, ";")
    }
    res
  })
  combine_strings(res, "\n")
}

xptr_sig <- function(fct_name) {
  combine_strings(
    c(
      "// [[Rcpp::depends(ast2ast)]]",
      "// [[Rcpp::depends(RcppArmadillo)]]",
      "// [[Rcpp::plugins(cpp2a)]]",
      '#include "etr.hpp"\n',
      "// [[Rcpp::export]]"
    ),
    "\n"
  )
}
create_arg_type_pairs <- function(vars_types_df) {
  if (class(vars_types_df) == "character") {
    return("")
  }
  df <- vars_types_df[vars_types_df$function_input, ]
  arg_types <- apply(df, 1, function(obj) {
    res <- convert_types_to_etr_types(obj["type"])
    if (obj["are_ref"]) {
      res <- paste(res, "&", sep = "")
    }
    if (obj["are_const"]) {
      res <- paste("const ", res, sep = "")
    }
    paste(res, obj["name"], sep = " ")
  })
}
create_signature_xptr <- function(vars_types_df, name_f, return_type, forward_decl = FALSE) {
  arg_types <- create_arg_type_pairs(vars_types_df)
  arg_types <- paste(arg_types, collapse = ", ")
  return_type <- convert_types_to_etr_types(return_type)
  if (forward_decl) {
    return(
      signature <- paste0(return_type, " ",
        name_f, "(", arg_types, ");")
    )
  }
  paste0(return_type, " ",
    name_f, "(", arg_types, ") {")
}
create_variable_declarations_xptr <- function(vars_types_df) {
  if (class(vars_types_df) == "character") {
    return("")
  }
  df <- vars_types_df[!vars_types_df$function_input, ]
  if (nrow(df) == 0) return("")
  apply(df, 1, function(x) {
    t <- x[["type"]] |> convert_types_to_etr_types()
    n <- x[["name"]]
    paste0("\t", t, " ", n, ";")
  })
}
get_xptr <- function(vars_types_df, name_fct, new_name, return_type) {
  arg_types <- create_arg_type_pairs(vars_types_df)
  forward_decl <- create_signature_xptr(vars_types_df, new_name, return_type, TRUE)
  typedef_line <- NULL
  return_xptr_fct <- combine_strings(c("SEXP ", name_fct, "()", ""), "")
  if (length(arg_types) == 0) {
    typedef_line <- combine_strings(
      c(
        "typedef ", return_type,
        " (*fct_ptr) (", ");", ""
      ), " ")
  } else {
    typedef_line <- combine_strings(
      c(
        "typedef ", return_type,
        " (*fct_ptr) (", combine_strings(arg_types, ", "), ");", ""
      ), " ")
  }
  body <- combine_strings(
    c(
    "\t",
    "return Rcpp::XPtr<fct_ptr>(new fct_ptr(&", new_name, "));", ""
    ), ""
  )
  combine_strings(c(
    paste0(return_xptr_fct, ";"), "\n",
    forward_decl,
    typedef_line,
    paste0(return_xptr_fct, " {"), "\n",
    body,
    "}"), "\n")
}

# Helper dataframe empty
df_empty <- function(df) {
  (ncol(df) == 0) || (nrow(df) == 0)
}
