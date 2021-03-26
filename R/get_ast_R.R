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
  ret <- c()
  counter <- 1
  counter2 <- 1
  for(i in y) {
    if(i == "symbol") {
      ret[[counter2]] <- c(x[[counter]], "variable")
      counter2 <- counter2 + 1
    }
    counter <- counter + 1
  }
  return(unlist(ret))
})
find_vars

# next step find constants and define types

# function misses logical etc. ...
constants <- lapply(test, function(x) {
  y <- lapply(x, typeof)
  y <- unlist(y)
  print(y)
  ret <- c()
  counter <- 1
  counter2 <- 1
  for(i in y) {
    if(i == "symbol") {
      ret[[counter2]] <- c(x[[counter]], "integer") # not correct
      counter2 <- counter2 + 1
    } else if(i == "double") {
      ret[[counter2]] <- c(x[[counter]], "double")
      counter2 <- counter2 + 1
    }
    counter <- counter + 1
  }
  return(unlist(ret))
})
constants


# store types of function arguments
# later 

# define type of variable
# 1. based on arguments of function
# 2. based on assignment, constants, type of function arguments

test[[1]]
# y <- 1 + x + z*z
# assume x and z are scalar doubles

lapply(test, function(x) {
  y <- lapply(x, typeof)
  print(y) 
})


size <- length(test)
lengths <- lapply(test, length)
nrows <- max(unlist(lengths))
df <- matrix(ncol = size, nrow = nrows) 
dat <- data.frame(1:nrows)

elongate <- function(vec) {
  #base
  if(length(vec) == nrows) {
    return(vec)
  }
  
  vec <- c(vec, NA)
  elongate(vec)
}

test <- lapply(test, elongate)

for(i in 1:size) {
  x <- test[[i]]
  vec_temp <- vector(length = nrows)
  for(j in 1:length(x)){
    if(is.null(x[[j]])) {
      df[j,i] <- NA
      vec_temp[j] <- NA
    } else {
      if(is.symbol(x[[j]])) {
        df[j,i] <- as.character(x[[j]])  
        vec_temp[j] <- as.character(x[[j]])
      } else {
      df[j,i] <- x[[j]] 
      vec_temp[j] <- x[[j]]
      }
    }
    
  }
  dat <- data.frame(dat, vec_temp)
}

df <- as.data.frame(df)
df

lapply(df, function(x) {
  y <- unlist(lapply(x, typeof))
})



