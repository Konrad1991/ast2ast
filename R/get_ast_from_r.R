library(rlang)

ast_tree <- function(x, layout = box_chars()) {
  if (is_quosure(x)) {
    x <- quo_squash(x)
  }
  
  # base cases
  if (rlang::is_syntactic_literal(x)) {
    return(x)
    #return(c(x, "constant"))#return(ast_leaf_constant(x))
  } else if (is_symbol(x)) {
    return(x)
    #return(c(x, "symbol"))#return(ast_leaf_symbol(x))
  } else if(!is.pairlist(x) && !is.call(x)){
    return(paste0("<inline ", paste0(class(x), collapse = "/"), ">"))
  }
  
  # recursive case
  subtrees <- lapply(x, ast_tree)
  
}

getast <- function(...) {
  args <- enexprs(...)
  return(args)
}

args <- getast(f(1,2,3,4), x <- 5, y, z <- 2L, df <- data.frame(a = 1:3))

out <- lapply(args, function(x) {
  print(is_syntactic_literal((x)))
})

out <- lapply(args, function(x) {
  print(is.call((x)))
})

out <- lapply(args, function(x) {
  print(is_call((x)))
})

out <- lapply(args, function(x) {
  print(is.symbol((x)))
})

out <- lapply(args, function(x) {
  print(is_symbol((x)))
})

out <- lapply(args, function(x) {
  print(is.pairlist((x)))
})

out <- lapply(args, function(x) {
  print(is_pairlist((x)))
})

out <- lapply(args, function(x) {
  print(is.expression((x)))
})

typeof(args)
str(args)

out <- lapply(args, function(x) {
  ast_tree(x)
})

lapply(out, function(x) {
  y <- unlist(x)
  y <- lapply(y, function(a) {
    b <- unlist(a)
    return(c(b, typeof(b)))
  })
  return(y)
})

r <- ast_tree(args[[1]])
r



# 1. fct that converts code into exprs
# 2. fct that converts exprs into tokens. Defining for each token the type.
# 3. fct that analyse each exprs and returns if it is an call, ... Not clear how this works
# 4. fct that translate code
