err_found <- function(string) {
  if (is.null(string)) return(FALSE)
  string <- gsub("\n|\t| ", "", string)
  string != ""
}

assert <- function(...) {
  expr <- c(...)
  message <- names(expr)
  if (!is.null(message)) {
    if (!expr) {
      stop(paste0("Error: ", message))
    }
  } else {
    if (length(expr) >= 1) {
      if (!expr) {
        stop(paste0(
          "Error: ",
          deparse(expr), " is not TRUE"
        ))
      }
    }
  }
}

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

convert_base_type <- function(r_type) {
  list(
    "void" = "void",
    "R_NilValue" = "R_NilValue",
    "logical" = "bool",
    "integer" = "int", "int" = "int",
    "double" = "double")[r_type]
}

convert_types_to_etr_types <- function(base_type, data_struct, r_fct, indent = "") {
  if (data_struct == "scalar") {
    convert_base_type(base_type)
  } else if (data_struct %in% c("vector", "matrix", "vec", "mat")) {
    data_struct <- c(vector = "etr::Vec", vec = "etr::Vec", matrix = "etr::Mat", mat = "etr::Mat")[data_struct]
    return(paste0(indent, data_struct, "<", convert_base_type(base_type), ">"))
  } else if (data_struct %in% c("borrow_vector", "borrow_matrix", "borrow_vec", "borrow_mat") && !r_fct) {
    data_struct <- c(borrow_vector = "etr::Vec", borrow_vec = "etr::Vec",  borrow_mattrix = "etr::Mat", borrow_mat = "etr::Mat")[data_struct]
    return(paste0(indent, data_struct, "<", convert_base_type(base_type), ", etr::Borrow<", convert_base_type(base_type), ">>"))
  } else if (data_struct %in% c("borrow_vector", "borrow_matrix", "borrow_vec", "borrow_mat") && !r_fct) {
    data_struct <- c(vector = "etr::Vec", vec = "etr::Vec", matrix = "etr::Mat", mat = "etr::Mat")[data_struct]
    return(paste0(indent, data_struct, "<", convert_base_type(base_type), ", etr::BorrowSEXP<", convert_base_type(base_type), ">>"))
  }
}

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

remove_empty_strings <- function(l) {
  l <- lapply(l, function(e) {
    if (e == "") return(NULL)
    e
  })
  Filter(Negate(is.null), l)
}

remove_double_quotes <- function(s) {
  gsub('"', "", s)
}
