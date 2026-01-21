is_symbol <- function (x) {
  if (typeof(x) != "symbol") {
    return(FALSE)
  }
  return(TRUE)
}

`%within%` <- function(elem, v) {
  for (i in seq_along(v)) {
    if (elem == v[i]) return(TRUE)
  }
  return(FALSE)
}

err_found <- function(string) {
  if (is.null(string)) return(FALSE)
  string <- gsub("\n|\t| ", "", string)
  string != ""
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
      "vector", "matrix", "array"
    )
  } else {
    c(
      "scalar", "vec", "mat",
      "vector", "matrix", "array",
      "borrow_vec", "borrow_vector",
      "borrow_mat", "borrow_matrix",
      "borrow_array"
    )
  }
}

convert_base_type <- function(r_type, real_type) {
  type_list <- list(
    "void" = "void",
    "R_NilValue" = "R_NilValue",
    "logical" = "etr::Logical",
    "bool" = "etr::Logical",
    "integer" = "etr::Integer", "int" = "etr::Integer",
    "double" = real_type)
  type_list[r_type]
}

convert_types_to_etr_types <- function(base_type, data_struct, r_fct, real_type, indent = "") {
  if (data_struct == "scalar") {
    convert_base_type(base_type, real_type)
  }
  else if (any(data_struct == c("vector", "matrix", "vec", "mat", "array"))) {
    data_struct <- "etr::Array"
    return(paste0(indent, data_struct, "<", convert_base_type(base_type, real_type), ", etr::Buffer<", convert_base_type(base_type, real_type), ">>"))
  }
  else if (any(data_struct == c("borrow_vector", "borrow_matrix", "borrow_vec", "borrow_mat", "borrow_array")) && !r_fct) {
    data_struct <- "etr::Array"
    return(paste0(indent, data_struct, "<", convert_base_type(base_type, real_type), ", etr::Borrow<", convert_base_type(base_type, real_type), ">>"))
  }
  else if (any(data_struct == c("borrow_vector", "borrow_matrix", "borrow_vec", "borrow_mat", "borrow_array")) && r_fct) {
    data_struct <- "etr::Array"
    return(paste0(indent, data_struct, "<", convert_base_type(base_type, real_type), ", etr::Borrow<", convert_base_type(base_type, real_type), ">>"))
  }
}

combine_strings <- function(string_list, collapse = "\n") {
  paste0(string_list, collapse = collapse)
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
    c("// [[Rcpp::depends(RcppArmadillo)]]",
      "// [[Rcpp::depends(ast2ast)]]",
      "// [[Rcpp::plugins(cpp2a)]]",
      '#include "etr.hpp"\n',
      "// [[Rcpp::export]]\n"), "\n"
  )
}
xptr_sig <- function() {
  combine_strings(
    c("// [[Rcpp::depends(RcppArmadillo)]]",
      "// [[Rcpp::depends(ast2ast)]]",
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

compile <- function(fct_code, r_fct,
                         verbose, name_f) {
  fct <- fct_code
  fct_ret <- NULL

  if (!r_fct) {
    tryCatch(
      expr = {
        if (verbose == TRUE) {
          cat(fct)
        }
        env <- new.env()
        Rcpp::sourceCpp(code = fct, verbose = verbose, env = env)
        fct_ret <- env$getXPtr()
        attributes(fct_ret) <- list(class = "XPtr")
      },
      error = function(e) {
        print(e)
        stop("Sorry compilation failed!")
      }
    )
  } else {
    # -fsanitize=alignment,bool,bounds,builtin,enum,integer-divide-by-zero,
    # nonnull-attribute,null,object-size,pointer-overflow,return,
    # returns-nonnull-attribute,shift,signed-integer-overflow,
    # unreachable,vla-bound,vptr
    # Sys.setenv("PKG_CXXFLAGS" = "-DRFCT -Wall -Wpedantic")
    # -fsanitize=address --param=max-vartrack-size=9000000 -fno-omit-frame-pointer

    tryCatch(
      expr = {
        env <- new.env()
        res <- Rcpp::sourceCpp(code = fct, verbose = verbose, env = env)
        fct_ret <- env[[name_f]]
      },
      error = function(e) {
        print(e)
        stop("Sorry compilation failed!")
      }
    )

    Sys.unsetenv("PKG_CXXFLAGS")

    if (verbose == TRUE) {
      cat(fct)
    }
  }

  return(fct_ret)
}
