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
  ast_tree(x)
})


actions <- lapply(args, function(x) {
  y <- FALSE
  if(is_syntactic_literal(x)) {
    y <- "syntactic_literal"
  } else if(is.call(x)) {
    y <- "call"
  } else if(is.symbol(x)) {
    y <- "symbol"
  } else if(is.pairlist(x)) {
    y <- "pairlist"
  } else if(is.expression(x)) {
    y <- "expression"
  }
  return(y)
})
values <- lapply(out, unlist)
types <- lapply(values, function(x) {
  y <- lapply(x, typeof)
  y <- unlist(y)
  return(y)
})
