library(rlang)

ast_tree <- function(x, layout = box_chars()) {
  if (is_quosure(x)) {
    x <- quo_squash(x)
  }
  
  # base cases
  if (rlang::is_syntactic_literal(x)) {
    return(c(x, "constant"))#return(ast_leaf_constant(x))
  } else if (is_symbol(x)) {
    return(c(x, "symbol"))#return(ast_leaf_symbol(x))
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

args <- getast(f(1,2,3,4), x <- 5)

out <- lapply(args, function(x) {
  print(is.call((x)))
})

out <- lapply(args, function(x) {
  print(is.call((x)))
})

out <- lapply(args, function(x) {
  print(is.symbol((x)))
})

out <- lapply(args, function(x) {
  print(is.pairlist((x)))
})

out <- lapply(args, function(x) {
  print(is.expression((x)))
})

out <- lapply(args, function(x) {
  ast_tree(x)
})

r <- ast_tree(args[[1]])
r


