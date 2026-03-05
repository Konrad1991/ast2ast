numeric_type <- R6::R6Class(
  "numeric_type",
  public = list(
    name = NULL,
    data_struct = NULL,
    const_or_mut = "mutable",
    copy_or_ref = "copy",
    error = NULL,
    iterator = FALSE,
    fct_input = FALSE,
    type_dcl = FALSE,
    r_fct = FALSE,
    real_type = "etr::Double"
  )
)

scalar <- R6::R6Class(
  "scalar",
  public = list(
    base_type = NULL
  )
)
vector <- R6::R6Class(
  "vector",
  public = list(
    base_type = NULL
  )
)
matrix <- R6::R6Class(
  "matrix",
  public = list(
    base_type = NULL
  )
)
array <- R6::R6Class(
  "array",
  public = list(
    base_type = NULL
  )
)
borrow_vector <- R6::R6Class(
  "borrow_vector",
  public = list(
    base_type = NULL
  )
)
borrow_matrix <- R6::R6Class(
  "borrow_matrix",
  public = list(
    base_type = NULL
  )
)
borrow_array <- R6::R6Class(
  "borrow_array",
  public = list(
    base_type = NULL
  )
)

parse_type_fct_input <- function(code, r_fct, real_type) {

  arity_check <- function(code, env) {
    operator <- deparse(code[[1L]])
    l <- length(code)
    if (operator == "type" && l < 3L) env$error <- "Too less arguments to function type"
    if (operator == "type" && l > 3L) env$error <- "Too many arguments to function type"
    if (operator %in% permitted_data_structs(FALSE) && l < 2L) env$error <- sprintf("Too less arguments to function %s", operator)
    if (operator %in% permitted_data_structs(FALSE) && l > 2L) env$error <- sprintf("Too many arguments to function %s", operator)
    if (operator == "const" && l < 2L) env$error <- "Too less arguments to function const"
    if (operator == "const" && l > 2L) env$error <- "Too many arguments to function const"
    if (operator == "ref" && l < 2L) env$error <- "Too less arguments to function ref"
    if (operator == "ref" && l > 2L) env$error <- "Too many arguments to function ref"
  }

  create_type_tree <- function(code, env) {
    code <- as.list(code)
    operator <- deparse(code[[1L]])
    arity_check(code, env)
    if (!is.null(env$error)) return()
    
    if (operator == "type") {

      t <- numeric_type$new()

      t$fct_input <- env$fct_input
      t$r_fct <- env$r_fct
      t$real_type <- env$real_type
      t$type_dcl <- env$type_dcl

      t$name <- deparse(code[[2L]])
      if (is.symbol(code[[3L]])) {
        t$data_struct <- scalar$new()
        t$data_struct$base_type <- deparse(code[[3L]])
      } else {
        t$data_struct <- parse_type_tree(code[[3L]], env)
      }
      return(t)
    }

    else if (operator %in% c("vec", "vector")) {
      v <- vector$new()
      v$base_type <- deparse(code[[2L]])
      return(v)
    }
    else if (operator %in% c("mat", "matrix")) {
      m <- matrix$new()
      m$base_type <- deparse(code[[2L]])
      return(m)
    }
    else if (operator == "array") {
      a <- array$new()
      a$base_type <- deparse(code[[2L]])
      return(a)
    }
    else if (operator %in% c("borrow_vec", "borrow_vector")) {
      v <- borrow_vector$new()
      v$base_type <- deparse(code[[2L]])
      return(v)
    }
    else if (operator %in% c("borrow_mat", "borrow_matrix")) {
      m <- borrow_matrix$new()
      m$base_type <- deparse(code[[2L]])
      return(m)
    }
    else if (operator == "borrow_array") {
      a <- borrow_array$new()
      a$base_type <- deparse(code[[2L]])
      return(a)
    }

    else if (operator == "const") {
      t <- parse_type_tree(code[[2L]],env)
      t$const_or_mut <- "const"
      return(t)
    }
    else if (operator == "ref") {
      t <- parse_type_tree(code[[2L]], env)
      t$copy_or_ref <- "ref"
      return(t)
    }

    else  {
      env$error <- sprintf("Found unsupported function %s", operator)
    }

  }

  parse_type_tree <- function(code, env) {
    if (!is.symbol(code) && is.call(code)) {
      create_type_tree(code, env)
    }
  }

  env <- new.env(parent = emptyenv())
  env$iterator <- FALSE
  env$fct_input <- TRUE
  env$type_dcl <- TRUE
  env$r_fct <- r_fct
  env$real_type <- real_type
  env$error <- NULL

  res <- parse_type_tree(code, env)
  if (!is.null(env$error)) {
    stop(deparse(code), "\n", env$error)
  }

  return(res)
}
