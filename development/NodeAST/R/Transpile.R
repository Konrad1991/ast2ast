transpile <- function(fct, name_fct, r_fct, variables, handle_args) {
  # Translate to C++
  body_cpp <- translate_internally(fct, variables)

  # Deduce return type

  # Declare variables

  if (r_fct) {
    # Define SEXP type declarations
    # Define fct signature
    # Assemble all parts
  } else {
    # Define XPtrtype declarations
    # Define fct signature
    # Assemble all parts
  }
}
