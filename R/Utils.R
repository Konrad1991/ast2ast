is_symbol <- function (x) {
  if (typeof(x) != "symbol") {
    return(FALSE)
  }
  return(TRUE)
}

err_found <- function(string) {
  if (is.null(string)) return(FALSE)
  string <- gsub("\n|\t| ", "", string)
  string != ""
}

wrap_in_block <- function(expr) {
  if (is.null(expr)) return(expr)
  if (is.call(expr) && identical(expr[[1]], as.name("{"))) {
    return(expr)
  }
  as.call(list(as.name("{"), expr))
}

same_base_type <- function(is, should) {
  if (is == "int") is <- "integer"
  if (should == "int") should <- "integer"
  should == is
}
same_data_struct <- function(is, should) {
  correct <- function(ds) {
    if (ds == "vec") return("vector")
    if (ds == "borrow_vec") return("borrow_vector")
    if (ds == "mat") return("matrix")
    if (ds == "borrow_mat") return("borrow_matrix")
    ds
  }
  is <- correct(is)
  should <- correct(should)
  should == is
}

base_type_rank <- function(bt) {
  switch(bt,
    logical = 1L, bool = 1L,
    integer = 2L, int = 2L,
    double = 3L,
    NA_integer_
  )
}
data_struct_rank <- function(ds) {
  switch(ds,
    scalar = 1L,
    vec = 2L, vector = 2L, borrow_vec = 2L, borrow_vector = 2L,
    mat = 3L, matrix = 3L, borrow_mat = 3L, borrow_matrix = 3L,
    array = 4L, borrow_array = 4L,
    NA_integer_
  )
}

rhs_fits_lhs <- function(lhs_base, lhs_struct, rhs_base, rhs_struct) {
  lb <- base_type_rank(lhs_base)
  rb <- base_type_rank(rhs_base)
  ls <- data_struct_rank(lhs_struct)
  rs <- data_struct_rank(rhs_struct)
  if (anyNA(c(lb, rb, ls, rs))) return(TRUE)
  (rb <= lb) && (rs == ls || rhs_struct == "scalar")
}

is_at_or_double_bracket <- function(s) {
  s %in% c("at", "[[")
}

choose_fast_path <- function(types) {
  fulfilled <- function(t) {
    if (!inherits(t, "pre_type_node")) return(FALSE)
    if (t$get_data_struct() != "scalar" || t$get_base_type() == "logical") return(FALSE)
    return(TRUE)
  }
  if (is.list(types)) {
    for (t in types) {
      if (!fulfilled(t)) return(FALSE)
    }
    return(TRUE)
  } else {
    if (!fulfilled(types)) return(FALSE)
    return(TRUE)
  }
}

infix_ops <- c(
  "+","-","*","/","^","%%", "%/%",
  "==","!=","<","<=",">",">=",
  "&","&&","|","||","!","<-","=",
  "[","[[", ":", "$"
)

not_infix_in_cpp <- c(
  "etr::at", "etr::subset", "etr::colon", "etr::power", "etr::idiv", "etr::collection_at"
)

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
  type_list[[r_type]]
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
    if (real_type == "etr::ReverseDouble") {
      stop("You cannot use borrowed data structures in combination with reverse mode automatic differentiation")
    }
    if (real_type == "etr::Dual") {
      stop("You cannot use borrowed data structures in combination with forward mode automatic differentiation")
    }
    data_struct <- "etr::Array"
    return(paste0(indent, data_struct, "<", convert_base_type(base_type, real_type), ", etr::Borrow<", convert_base_type(base_type, real_type), ">>"))
  }
  else if (any(data_struct == c("borrow_vector", "borrow_matrix", "borrow_vec", "borrow_mat", "borrow_array")) && r_fct) {
    if (real_type == "etr::ReverseDouble") {
      stop("You cannot use borrowed data structures in combination with reverse mode automatic differentiation")
    }
    if (real_type == "etr::Dual") {
      stop("You cannot use borrowed data structures in combination with forward mode automatic differentiation")
    }
    data_struct <- "etr::Array"
    return(paste0(indent, data_struct, "<", convert_base_type(base_type, real_type), ", etr::Borrow<", convert_base_type(base_type, real_type), ">>"))
  }
}

combine_strings <- function(string_list, collapse = "\n") {
  paste0(string_list, collapse = collapse)
}

escape_cpp_string_literal <- function(s) {
  s <- gsub("\\\\", "\\\\\\\\", s)
  s <- gsub("\"", "\\\\\"", s)
  s <- gsub("\n", "\\\\n", s)
  s
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
    c(
      "#include <Rcpp.h>",
      "// [[Rcpp::depends(ast2ast)]]",
      "// [[Rcpp::plugins(cpp2a)]]",
      '#include "etr.hpp"\n'), "\n"
  )
}
xptr_sig <- function() {
  combine_strings(
    c(
      "#include <Rcpp.h>",
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

  # Windows needs Rtools; fail with a clear message before sourceCpp's noise
  if (.Platform$OS.type == "windows" &&
      requireNamespace("pkgbuild", quietly = TRUE) &&
      !pkgbuild::has_build_tools(debug = FALSE)) {
    stop(
      "ast2ast needs a C++ toolchain to compile the generated code.\n",
      "Install Rtools (matching your R version) from\n",
      "  https://cran.r-project.org/bin/windows/Rtools/\n",
      "and restart R.",
      call. = FALSE
    )
  }

  # link R's BLAS/LAPACK so dgemm_ resolves on all platforms (Windows is strict)
  old_libs <- Sys.getenv("PKG_LIBS", unset = NA)
  Sys.setenv(PKG_LIBS = "$(LAPACK_LIBS) $(BLAS_LIBS) $(FLIBS)")
  on.exit({
    if (is.na(old_libs)) Sys.unsetenv("PKG_LIBS") else Sys.setenv(PKG_LIBS = old_libs)
  }, add = TRUE)

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

is_base_type <- function(type) {
  type %in% permitted_base_types()
}

needs_to_SEXP_path <- function(type) {
  inherits(type, "new_type_node") ||
    (inherits(type, "pre_type_node") && type$get_data_struct() == "collection")
}

add_error <- function(env, msg) {
  env$errors <- c(env$errors, msg)
}

arity_check <- function(code, env) {
  operator <- deparse(code[[1L]])
  l <- length(code)
  if (operator == "type" && l < 3L) add_error(env, "Too less arguments to function type")
  if (operator == "type" && l > 3L) add_error(env, "Too many arguments to function type")
  if (operator %in% permitted_data_structs(FALSE) && l < 2L) add_error(env, sprintf("Too less arguments to function %s", operator))
  if (operator %in% permitted_data_structs(FALSE) && l > 2L) add_error(env, sprintf("Too many arguments to function %s", operator))
  if (operator == "const" && l < 2L) add_error(env, "Too less arguments to function const")
  if (operator == "const" && l > 2L) add_error(env, "Too many arguments to function const")
  if (operator == "ref" && l < 2L) add_error(env, "Too less arguments to function ref")
  if (operator == "ref" && l > 2L) add_error(env, "Too many arguments to function ref")
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

resolve_type_collection <- function(code, env) {
  elem <- deparse(code)
  new_ops <- vapply(env$list, function(o) {
    o$name
  }, character(1L))
  ops <- c(new_ops, permitted_base_types(), "void", "R_NilValue")
  if (!(elem %in% ops)) {
    if (!is.null(env$new_type) && identical(env$new_type$name, elem)) {
      add_error(env, sprintf("Self-referencing types are not supported: %s cannot reference itself in its own slot definition", elem))
    } else {
      add_error(env, sprintf("Found unsupported base type: %s", elem))
    }
  }
  if (elem == "int") {
    return("integer")
  }
  elem
}

resolve_base_type <- function(code, env) {
  elem <- deparse(code)
  if (!(elem %in% permitted_base_types())) {
    add_error(env, sprintf("Found unsupported base type: %s", elem))
  }
  if (elem == "int") {
    return("integer")
  }
  elem
}
