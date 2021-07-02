# ================================================================================
# Identify code in control blocks and replace it with variable _sub1
# It is not allowed to use '_' in names 
# ================================================================================


# ================================================================================
# load libraries
# ================================================================================
library(rlang)
library(purrr)

# ================================================================================
# function that seperate the expressions
# ================================================================================
sep_expressions <- function(code) {
  out <- parse_exprs(code)
  return(out)
}

# ================================================================================
# function that extract abstract syntax tree
# ================================================================================
extractast <- function(seperated_code) {
  out <- map_if(as.list(seperated_code), is.call, extractast)
  return(out)
} 

# ================================================================================
# environment which stores the variable list
# ================================================================================
vars <- new.env()

vars$list <- list()

# ================================================================================
# store arguments passed to funciton in vars$list
# ================================================================================
arguments <- function(vars) {
  vars <- strsplit(vars, ",")
  vars <- lapply(vars, function(x) gsub(" ", "", x) )
  vars <- unlist(vars)
  
  vars <- sep_expressions(vars)
  vars <- extractast(vars)
  
  return(vars)
}

vars$list <- arguments("num(x), num(y)")

# ================================================================================
# examine one line update var$list if necessary
# ================================================================================
check_symbols <- function(symb) {
  allowed_fct <- c("+", "-", "*", "/")
  
  check <- "var"

  print(symb)
  print("_:")
  for(i in seq_along(allowed_fct)) {
    if(symb == allowed_fct[i]) {
      check <- "fct"
      break
    }
  }
  
  return(check)
}



newvar <- function(codeast) {
  out <- lapply(codeast, function(x) {
    
    var_list_rhs <- list()
    counter <- 1
    
    type <- lapply(codeast, function(y) {
      
      if(x[[1]] == rlang::sym("<-")) {
        
        variables_at_rhs <- lapply(y[2:length(y)], function(x) {
          test <- check_symbols(x)
          if(test == "var") {
            elem_index <- grep(quote(x), vars$list)
            var_list_rhs[[counter]] <<- vars$list[[elem_index]]  
          }
        })
        
      }
      
    })
      
    })

}

# one fctloopüing over lines
# fct which examines different lines

code <- "y <- x + 1"
test <- sep_expressions(code)
test <- extractast(test)
vars <- new.env()
vars$list <- list()
vars$list <- arguments("num(x), num(z)")
test <- newvar(test)
