xptr_sig <- function() {
  cString(
    "// [[Rcpp::depends(ast2ast)]]",
    "// [[Rcpp::depends(RcppArmadillo)]]",
    "// [[Rcpp::plugins(cpp2a)]]",
    '#include "etr.hpp"\n',
    "// [[Rcpp::export]]", "\n"
  )
}

ptr_vec_sig <- function(var_name, vars, type) {
  ptr_name <- generate_new_name(var_name, "_double_ptr", "", vars)
  size_name <- generate_new_name(var_name, "_int_size", "", vars)
  res <- cString(
    paste0(type, "*"), ptr_name, ",",
    "int", size_name, " "
  )
  return(res)
}

ptr_mat_sig <- function(var_name, vars, type) {
  ptr_name <- generate_new_name(var_name, "_double_ptr", "", vars)
  rows_name <- generate_new_name(var_name, "_int_rows", "", vars)
  cols_name <- generate_new_name(var_name, "_int_cols", "", vars)
  res <- cString(
    paste0(type, "*"), ptr_name, ",",
    "int ", rows_name, ",", "int ", cols_name, " "
  )
  return(res)
}

type_sig_xptr <- function(var_name, type, reference, all_vars) {
  new_name <- NULL
  if (type %in% c(
    "double_ptr", "int_ptr", "bool_ptr",
    "double_mat_ptr", "int_mat_ptr", "bool_mat_ptr"
  )) {
    temp <- strsplit(type, "_")[[1]]
    is_mat <- FALSE
    if (length(temp) == 3) {
      is_mat <- TRUE
    }
    data_type <- temp[[1]]
    if (is_mat) {
      new_name <- ptr_mat_sig(var_name, all_vars, data_type)
    } else {
      new_name <- ptr_vec_sig(var_name, all_vars, data_type)
    }
  } else if (type %in% c(
    "double", "int", "bool", "etr::Vec<double>",
    "etr::Vec<int>", "etr::Vec<bool>"
  )) {
    if (!reference) {
      new_name <- cString(type, var_name, " ")
    } else {
      new_name <- cString(type, "&", var_name, " ")
    }
  } else {
    stop("Found undefined type")
  }
  return(new_name@value)
}

define_sig_list_xptr <- function(arguments, types, name_fct,
                                 reference, ret_type, all_vars) {
  stopifnot(length(arguments) == length(types))
  allowed_types <- c(
    "etr::Vec<double>", "etr::Vec<int>", "etr::Vec<bool>",
    "double", "void", "int"
  )
  stopifnot(ret_type %in% allowed_types)
  type_list <- ""
  ptr_names <- list()
  counter <- 1
  for (i in seq_along(arguments)) {
    current_name <- type_sig_xptr(
      arguments[[i]],
      types[[i]], reference, all_vars
    )
    if (types[[i]] == "double_ptr") {
      split_name <- strsplit(current_name, split = " ")[[1]]
      ptr_names[[counter]] <- list(
        arguments[[i]],
        split_name[[2]], split_name[[5]]
      )
      counter <- counter + 1
    } else if (types[[i]] == "ptr_mat") {
      split_name <- strsplit(current_name, split = " ")[[1]]
      ptr_names[[counter]] <- list(
        arguments[[i]],
        split_name[[2]], split_name[[6]], split_name[[10]]
      )
      counter <- counter + 1
    }
    if (i == length(arguments)) {
      type_list <- paste0(type_list, current_name)
    } else {
      type_list <- paste0(type_list, current_name, ", ")
    }
  }
  return(list(type_list, ptr_names))
}

signature_xptr <- function(type_list, ret_type, name_fct) {
  ret <- cString(ret_type, " ", name_fct, "(", type_list, ") {\n", "")
  return(ret@value)
}

get_xptr <- function(type_list, ret_type, name_fct) {
  name <- generate_new_name("get", "XPtr", "", name_fct)
  f_line <- cString("SEXP", name, "()", " {", " ")
  s_line <- cString(
    "\t", "typedef ", ret_type,
    " (*fct_ptr) (", type_list, ");", ""
  )
  t_line <- cString(
    "\t",
    "return Rcpp::XPtr<fct_ptr>(new fct_ptr(&", name_fct, "));", ""
  )
  fo_line <- cString("}", "")
  res <- cString(f_line, s_line, t_line, fo_line, "\n")
  pre_declaration <- cString("SEXP ", name, "();", "")
  return(list(name = pre_declaration, fct = res@value))
}

handle_ptr <- function(ptr_list, borrow) {
  if (length(ptr_list) == 0) {
    return("")
  }
  ls <- lapply(ptr_list, function(x) x[[1]])
  rs <- lapply(ptr_list, function(x) x[2:length(x)])
  stopifnot(length(ls) == length(rs))
  res <- ""
  for (i in seq_along(ls)) {
    arg <- paste(unlist(rs[[i]]), collapse = ",")
    if (borrow) {
      temp <- cString("\t BorrowPtr ", ls[[i]], "(", arg, ");", "")@value
      res <- paste(res, temp, "\n")
    } else {
      temp <- cString("\t etr::Vec<double>", ls[[i]], "(", arg, ");", "")@value
      res <- paste(res, temp, "\n")
    }
  }
  return(res)
}

build_fct_xptr <- function(fct, reference, types, ret_type, name_fct) {
  ac <- astClass$new(fct, name_fct, R_fct = FALSE)
  ac$getast()
  ac$ast2call()
  ac$call2char()
  vars <- ac$get_vars()
  declarations <- vars_declaration(vars, ac$var_types)
  f <- xptr_sig()
  b <- build_body(ac$char)
  sig_list <- define_sig_list_xptr(
    ac$args, ac$args_types,
    name_fct, reference, ret_type, ac$var_all
  )
  declarations_ptr <- handle_ptr(sig_list[[2]], reference)
  sig <- signature_xptr(sig_list[[1]], ret_type, name_fct)
  xptr <- get_xptr(sig_list[[1]], ret_type, name_fct)
  f <- cString(
    f, xptr$name, "\n", sig,
    declarations, declarations_ptr, b, "}\n", xptr$fct, "\n"
  )
  return(f@value)
}
