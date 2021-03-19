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

args <- getast(y <- 1 + 2)

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
temp[[1]]
temp[[2]]
temp[[3]]
temp[[4]]

temp[[1]]
temp[[2]]
temp[[3]][[2]] <- temp[[1]]
temp[[3]][[3]] <- temp[[2]]
temp[[3]]
