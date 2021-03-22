library(rlang)

get_num_calls <- function(x, layout = box_chars()) {
  if (is_quosure(x)) {
    x <- quo_squash(x)
  }
  
  # base cases
  if (rlang::is_syntactic_literal(x)) {
    return(x)
  } else if (is_symbol(x)) {
    return(x)
  } else if(!is.pairlist(x) && !is.call(x)){
    return(paste0("<inline ", paste0(class(x), collapse = "/"), ">"))
  } else {
    print(x)
    num <<- num + 1
  }
  # recursive case
  subtrees <- lapply(x, get_num_calls)
}


get_calls <- function(x, layout = box_chars()) {
  
  if (is_quosure(x)) {
    x <- quo_squash(x)
  }
  # base cases
  if (rlang::is_syntactic_literal(x)) {
    return(x)
  } else if (is_symbol(x)) {
    return(x)
  } else if(!is.pairlist(x) && !is.call(x)){
    return(paste0("<inline ", paste0(class(x), collapse = "/"), ">"))
  } else {
    
  }
  
  # recursive case
  
  subtrees <- lapply(x, get_calls)
  counter <<- counter + 1
  temp[[counter]] <<- subtrees

  print("") # this print is necessary why???
}

getast <- function(...) {
  args <- enexprs(...)
  return(args)
}



args <- getast(y <- 1 + x + z*z)
args

num <- 0
temp <- lapply(args, function(x) {
  y <- get_num_calls(x)
})
num

counter <- 0
temp <- rep(list(NULL), num)# NULL
out <- lapply(args, function(x) {
  y <- get_calls(x)
  return(y)
})

# temp[[1]]
# temp[[2]]
# temp[[3]]
# temp[[4]]
# 
# temp[[1]]
# temp[[2]]
# temp[[3]][[2]] <- temp[[1]]
# temp[[3]][[3]] <- temp[[2]]
# temp[[3]]


# next each element of fct has to be inspected.

values <- lapply(temp, unlist)
types <- lapply(values, function(x) {
  y <- lapply(x, typeof)
  y <- unlist(y)
  return(y)
})
types

bo <- list(assignement = '<-', assignment = '=', plus = '+', minus = '-', time = '*', divide = '/')

checker <- function(element) {
  return(match(element, bo))  
}

test <- lapply(temp, function(x) {
  y <- x
  ret <- lapply(y, function(z) {
    a <- NULL
    if(is_symbol(z)) {
      a <- as_string(z) 
    } else if(is_double(z)) {
      a <- as.character(z)
    }
    if(is_empty(a)) {
      a <- NULL
    } else {
      tester <- checker(a)
      
      if(!is.null(tester) && !is.na(tester)) {
      a <- names(bo)[[match(a, bo)]]
      } else {
      a <- z
      }
    }
    return(a)
  })
  return(ret)
})

test[[1]]
test[[2]]
test[[3]]
test[[4]]

find_vars <- lapply(test, function(x) {
  y <- lapply(x, typeof)
  y <- unlist(y)
  z <- lapply(y, function(a) {
    index <- parent.frame()$i[]   
    ret <- list()
    if(a == "symbol") {
    ret[[index]] <- c(x[[index]], "it_is_a_variable")
    }
    return(ret)
  })
  return(z)
})
find_vars

