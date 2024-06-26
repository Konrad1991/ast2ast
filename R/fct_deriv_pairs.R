fct <- setClass(
  "fct",
  slots = list(
    name = "character",
    dfdx = "function",
    name_deriv = "character",
    keep = "logical"
  )
)

setGeneric(
  name = "add_fct",
  def = function(obj, name, dfdx, keep = FALSE) {
    standardGeneric("add_fct")
  } 
)

# used only internally
setGeneric(
  name = "append_fct",
  def = function(obj, name, dfdx_new, name_deriv_new, keep_new) {
    standardGeneric("append_fct")
  } 
)

setGeneric(
  name = "get_derivative",
  def = function(obj, name) {
    standardGeneric("get_derivative")
  } 
)

setGeneric(
  name = "get_derivative_name",
  def = function(obj, name) {
    standardGeneric("get_derivative_name")
  } 
)

setGeneric(
  name = "get_keep",
  def = function(obj, name) {
    standardGeneric("get_keep")
  }
)

sin_deriv <- function(x) cos(x)
sinh_deriv <- function(x)  cosh(x) 
asin_deriv <- function(x) 1/(sqrt(1 - x^2)) 
cos_deriv <- function(x) -sin(x) 
cosh_deriv <- function(x) sinh(x) 
acos_deriv <- function(x) -asin(x) 
tan_deriv <- function(x) tan(x)^2 + 1 
tanh_deriv <- function(x) 1 - tanh(x)^2 
atan_deriv <- function(x) 1 / (1 + x^2) 
exp_deriv <- function(x) exp(x) 
log_deriv <- function(x) 1/x 
sqrt_deriv <- function(x) 0.5 * (x)^(-0.5) 
c_deriv <- function(...) c(...)
vector_deriv <- function(x) vector(length = x)
numeric_deriv <- function(x) numeric(x)
rep_deriv <- function(x, y) rep(x, y)
matrix_deriv <- function(val = 0, x, y) matrix(val, x, y)

fcts <- setClass(
  "fcts",
   slots = c(funs = "list")
)

fcts_add_fct <- function(lst, f, f_deriv, keep = FALSE) {
  name <- as.character(deparse(substitute(f)))
  lst@funs[[name]] = fct(name = name,
                         dfdx = f_deriv, keep = keep,
                         name_deriv = as.character(deparse(substitute(f_deriv))) )
  lst
}


setMethod(
  "initialize",
  signature = "fcts",
  def = function(.Object) {
    .Object@funs <- list()
    .Object
  }
)

setMethod(
  f = "add_fct",
  signature = "fcts",
  definition = function(obj, name, dfdx, keep = FALSE) {
    obj@funs[[name]] = fct(name = name, dfdx = dfdx, keep = keep,
                           name_deriv = as.character(deparse(substitute(dfdx))) )
    obj
  }
)

setMethod(
  f = "append_fct",
  signature = "fcts",
  definition = function(obj, name, dfdx_new, name_deriv_new, keep_new) {
    obj@funs[[name]] = fct(name = name, dfdx = dfdx_new, name_deriv =  name_deriv_new, keep = keep_new)
    obj
  }
)

setMethod(
  f = "get_derivative",
  signature = "fcts",
  definition = function(obj, name) {
    obj@funs[[name]]@dfdx
  } 
)

setMethod(
  f = "get_derivative_name",
  signature = "fcts",
  definition = function(obj, name) {
    obj@funs[[name]]@name_deriv
  } 
)

setGeneric(
  name = "get_names",
  def = function(obj) {
    standardGeneric("get_names")
  } 
)

setMethod(
  f = "get_names",
  signature = "fcts",
  definition = function(obj) {
    l <- obj@funs
    sapply(l, function(x) x@name)
  }
)

setMethod(
  f = "get_keep",
  signature = "fcts",
  definition = function(obj, name) {
    obj@funs[[name]]@keep
  }
)


sin_deriv <- function(x) cos(x)
sinh_deriv <- function(x)  cosh(x) 
asin_deriv <- function(x) 1/(sqrt(1 - x^2)) 
cos_deriv <- function(x) -sin(x) 
cosh_deriv <- function(x) sinh(x) 
acos_deriv <- function(x) -asin(x) 
tan_deriv <- function(x) tan(x)^2 + 1 
tanh_deriv <- function(x) 1 - tanh(x)^2 
atan_deriv <- function(x) 1 / (1 + x^2) 
exp_deriv <- function(x) exp(x) 
log_deriv <- function(x) 1/x 
sqrt_deriv <- function(x) 0.5 * (x)^(-0.5) 
c_deriv <- function(...) c(...)
vector_deriv <- function(x) vector(length = x)
numeric_deriv <- function(x) numeric(x)
rep_deriv <- function(x, y) rep(x, y)
matrix_deriv <- function(val = 0, x, y) matrix(val, x, y)



init_fct_list <- function() {
  f <- fcts()
  f <- add_fct(f, "sin",  sin_deriv)
  f <- add_fct(f, "sinh", sinh_deriv)
  f <- add_fct(f, "asin", asin_deriv)
  f <- add_fct(f, "cos",  cos_deriv)
  f <- add_fct(f, "cosh", cosh_deriv)
  f <- add_fct(f, "acos", acos_deriv)
  f <- add_fct(f, "tan",  tan_deriv)
  f <- add_fct(f, "tanh", tanh_deriv)
  f <- add_fct(f, "atan", atan_deriv)
  f <- add_fct(f, "exp",  exp_deriv)
  f <- add_fct(f, "log",  log_deriv)
  f <- add_fct(f, "sqrt", sqrt_deriv)
  f <- add_fct(f, "c", c_deriv, TRUE)
  f <- add_fct(f, "vector", vector_deriv, TRUE)
  f <- add_fct(f, "numeric", numeric_deriv, TRUE)
  f <- add_fct(f, "rep", rep_deriv, TRUE)
  f <- add_fct(f, "matrix", matrix_deriv, TRUE)
  return(f)
}