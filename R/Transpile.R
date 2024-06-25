transpile <- function(fct, name_fct, r_fct, toa, hoa) {
  ac <- astClass$new(fct, name_fct, R_fct = r_fct)
  ac$getast()
  ac$ast2call()
  ac$call2char()
  check_variable_type_pairs(ac$args, ac$get_variable_type_pairs())
  variable_type_list <- deduce_types(ac$get_variable_type_pairs(), ac$var_all |> unique())
  return_type <- deduce_return_type(
    variable_type_list, toa, ac$args,
    ac$get_return_variable(), r_fct
  )
  print(toa)
  print(hoa)
  print(ac$args)
  print(name_fct)
  print(return_type)
}
