transpile <- function(fct, name_fct, r_fct, toa, hoa, data_types_of_args) {
  ac <- astClass$new(fct, name_fct, R_fct = r_fct)
  ac$getast()
  ac$ast2call()
  ac$call2char()
  if (!ac$return_TF) {
    ac$char <- c(ac$char, "\n", "return(R_NilValue); \n")
  }
  b <- build_body(ac$char)

  check_variable_type_pairs(ac$args, ac$get_variable_type_pairs())
  variable_type_list <- deduce_types(
    ac$get_variable_type_pairs(), ac$var_all |> unique(), ac$args
  )
  return_type <- deduce_return_type(
    variable_type_list, toa, ac$args,
    ac$get_return_variable(), r_fct
  )
  decl <- declare_vars(variable_type_list)
  if (r_fct) {
    res <- sexp_declarations(
      ac$args, hoa,
      data_types_of_args, ac$var_all
    )
    sig <- signature_code(return_type, name_fct, toa, res[[2]])
    args_copy_or_borrow_decl <- res[[1]]
    result <- cString(
      r_fct_sig(),
      sig,
      decl,
      args_copy_or_borrow_decl,
      b, "}\n", ""
    )@value
    return(result)
  } else {
    res <- borrow_declarations(
      ac$args, hoa,
      data_types_of_args, ac$var_all
    )
    sig <- signature_code(return_type, name_fct, toa, res[[2]])
    args_copy_or_borrow_decl <- res[[1]]
    deps <- xptr_sig("get", name_fct)
    get_fct <- get_xptr(toa, return_type, name_fct, name = deps[[1]])
    result <- cString(
      deps[[2]],
      "\n",
      "\n",
      "\n",
      sig,
      decl,
      args_copy_or_borrow_decl,
      b, "}\n",
      "\n",
      get_fct,
      ""
    )@value
    return(result)
  }
}
