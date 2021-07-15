# ================================================================================
# function that seperate the expressions
# ================================================================================
sep_expressions <- function(code) {
  out <- rlang::parse_exprs(code)
  return(out)
}

# ================================================================================
# function that extract abstract syntax tree
# ================================================================================
extractast <- function(seperated_code) {
  out <- purrr::map_if(as.list(seperated_code), is.call, extractast)
  return(out)
} 

