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
# fct which loops over lines
# ================================================================================
loop <- function(code_lines, fct) {
  index <- parent.frame()$i[]
  res <- lapply(code_lines, fct)
  
  return(res)
}

# ================================================================================
# fct which finds '<-'
# ================================================================================
find_assign <- function(element_one) {
  if(element_one == rlang::sym("<-")) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# ================================================================================
# fct which finds line with '<-'
# ================================================================================
find_assign_lines <- function(code) {
  
  fct <- function(code_line) {
    if(find_assign(code_line[[1]]) == TRUE) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  
  res <- loop(code, fct)
}


# ================================================================================
# examine one line update var$list if necessary
# ================================================================================
# does not work
check_symbols <- function(symb) {
  allowed_fct <- c("+", "-", "*", "/")
  
  check <- "var"

  for(i in seq_along(allowed_fct)) {
    if(symb == allowed_fct[i]) {
      check <- "fct"
      break
    }
  }
  
  return(check)
}


# ================================================================================
# fct which finds variables at rhs
# ================================================================================
check_rhs <- function(code, assign_lines) {
  
  ret <- list()
  counter <- 0
  for(i in 1:length(code)) {
    
    if(assign_lines[[i]] == TRUE) {
      counter <- counter + 1
      ret[[counter]] <- lapply(code[[i]][2:length(code[[i]])], function(x) {
        #test <- check_symbols(x) # does not work
        if(test == "var") {
          return(x)
        } else {
          return("novar")}
      } )
      
    }
  }
  
  return(ret)
  
}


#if(test == "var") {
# elem_index <- grep(quote(x), vars$list)
#var_list_rhs[[counter]] <<- vars$list[[elem_index]]
#}

code <- "y <- x + 1
         y <- y + 1
         print(y)"
test <- sep_expressions(code)
test <- extractast(test)
assign_lines<-find_assign_lines(test)
vars <- check_rhs(test, assign_lines)
vars <- new.env()
vars$list <- list()
vars$list <- arguments("num(x), num(z)")
test <- newvar(test)
