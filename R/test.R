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



args <- getast(for(i in (1:5)) {
  print(i)
})
args

count_calls_of_test <- 0
result <- list()
test <- function(x) {
  count_calls_of_test <<- count_calls_of_test + 1
  print("number of call")
  print(count_calls_of_test)
  
  expr <- enexpr(x)
  #
  
  print(expr)
  print("____________________")
  print(is_quosure(expr))
  print("____________________")
  print(is_syntactic_literal(expr))
  print("____________________")
  print(is_symbol(expr))
  print("____________________")
  print(!is.pairlist(expr) && !is.call(expr))
  
  if(is_syntactic_literal(expr)) {
    result <<- c(result, expr)
    return(expr)
  } else if(is_symbol(expr)){
    result <<- c(result, expr)
    return(expr)
  } else if(!is.pairlist(expr) && !is.call(expr)) {
    result <<- c(result, expr)
    return(expr)
  }
  
  subtrees <- lapply(expr, test)
}

test(for(i in (1:5)) {
  print(i)
})

result
