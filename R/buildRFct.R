r_fct_sig <- function() {
  cString(
    "// [[Rcpp::depends(ast2ast)]]",
    "// [[Rcpp::depends(RcppArmadillo)]]",
    "// [[Rcpp::plugins(cpp2a)]]",
    '#include "etr.hpp"\n',
    "// [[Rcpp::export]]\n", "\n"
  )
}

generate_new_name <- function(name, extension, delimiter, vars) {
  new_name <- cString(name, extension, delimiter)
  i <- 0
  repeat {
    if (i == 10) {
      stop("Cannot generate a new name for the double pointer")
    }
    if (!(new_name@value %in% vars)) {
      break
    } else {
      new_name <- cString(new_name@value, extension, delimiter)
      i <- i + 1
    }
  }
  return(new_name)
}

type_sig_r <- function(var_name, type) {
  new_name <- cString(type, var_name, " ")
  return(new_name@value)
}

signature_r <- function(arguments, name_fct, vars, argument_types, return_type) {
  type_list <- ""
  new_names <- list()
  for (i in seq_along(arguments)) {
    if (argument_types[[i]] == "SEXP") {
      current_name <- generate_new_name(
        arguments[[i]],
        "SEXP", "", vars
      )
    } else {
      current_name <- cString(arguments[[i]])
    }
    new_names[[i]] <- current_name@value
    if (i == length(arguments)) {
      type_list <- paste0(
        type_list,
        type_sig_r(current_name, argument_types[[i]])
      )
    } else {
      type_list <- paste0(
        type_list,
        type_sig_r(current_name, argument_types[[i]]), ", "
      )
    }
  }

  ret <- cString(
    return_type,
    # "SEXP",
    " ", name_fct, "(", type_list, ") {\n", ""
  )
  return(list(ret@value, new_names))
}

vars_declaration <- function(vars, types) {
  stopifnot(length(vars) == length(types))
  if (length(vars) == 0) {
    return("")
  }
  l <- list()
  for (i in seq_along(vars)) {
    l[[i]] <- cString(
      "\t ",
      cString(types[[i]], deparse(vars[[i]]), " ")@value, ";\n", ""
    )@value
  }
  l <- do.call(paste, l)
  return(l)
}

handle_sexp <- function(ls, rs, ls_types, reference) {
  stopifnot(length(ls) == length(rs))
  if (!reference) {
    res <- ""
    for (i in seq_along(ls)) {
      if (ls_types[[i]] == "SEXP") {
        temp <- cString(
          "\t etr::Vec<double>", ls[[i]], "; ",
          ls[[i]], " = ", rs[[i]], ";", ""
        )@value
        res <- paste(res, temp, "\n")
      }
    }
    return(res)
  } else {
    res <- ""
    for (i in seq_along(ls)) {
      if (ls_types[[i]] == "SEXP") {
        temp <- cString(
          "\t WrapperSEXP ", ls[[i]], "; ",
          ls[[i]], " = ", rs[[i]], ";", ""
        )@value
        res <- paste(res, temp, "\n")
      }
    }
    return(res)
  }
}

build_body <- function(body_content) {
  b <- lapply(body_content, function(x) {
    paste("\t", x)
  })
  paste(unlist(b), collapse = "")
}

build_fct_r <- function(fct, name_fct, references, return_type) {
  ac <- astClass$new(fct, name_fct, R_fct = TRUE)
  ac$getast()
  ac$ast2call()
  ac$call2char()
  vars <- ac$get_vars()
  f <- r_fct_sig()
  declarations <- vars_declaration(vars, ac$var_types)
  # print(ac$get_variable_type_pairs())
  # cat("\n")
  # print(vars)
  # cat("\n")
  # print(ac$var_all |> unique())
  # cat("\n")
  # print(ac$args)
  # cat("\n")
  # print(ac$return_variables)
  sig <- signature_r(ac$args, name_fct, ac$var_all, ac$args_types, return_type)
  hs <- handle_sexp(ac$args, sig[[2]], ac$args_types, references)
  if (!ac$return_TF) {
    ac$char <- c(ac$char, "\n", "return(R_NilValue); \n")
  }
  b <- build_body(ac$char)
  f <- cString(f, sig[[1]], hs, declarations, b, "}\n", "")
  return(f@value)
}
