permitted_data_structs <- function(r_fct) {
  if (r_fct) {
    c(
      "scalar", "vec", "mat",
      "vector", "matrix", "array"
    )
  } else {
    c(
      "scalar", "vec", "mat",
      "vector", "matrix", "array",
      "borrow_vec", "borrow_vector",
      "borrow_mat", "borrow_matrix",
      "borrow_array"
    )
  }
}

pre_type_node <- R6::R6Class(
  "pre_type_node",
  public = list(
    name = NULL,
    data_struct = NULL,
    const_or_mut = "mutable",
    copy_or_ref = "copy",
    print = function() {
      cat(paste0(self$name, ": "))
      print(self$data_struct)
    },
    stringify = function(indent = "") {
      paste0(indent, self$data_struct$stringify(indent), " ", self$name, ";")
    }
  )
)

scalar <- R6::R6Class(
  "scalar",
  public = list(
    base_type = NULL,
    print = function() {
      cat(sprintf("scalar value of: %s", self$base_type), "\n")
    },
    stringify = function(indent = "") {
      self$base_type
    }
  )
)
vec <- R6::R6Class(
  "vec",
  public = list(
    base_type = NULL,
    print = function() {
      cat(sprintf("vector containing: %s", self$base_type), "\n")
    },
    stringify = function(indent = "") {
      sprintf("etr::Array<%s, etr::Buffer<%s>>", self$base_type, self$base_type)
    }
  )
)
mat <- R6::R6Class(
  "mat",
  public = list(
    base_type = NULL,
    print = function() {
      cat(sprintf("matrix containing: %s", self$base_type), "\n")
    }
  )
)
arr <- R6::R6Class(
  "arr",
  public = list(
    base_type = NULL,
    print = function() {
      cat(sprintf("array containing: %s", self$base_type), "\n")
    }
  )
)
borrow_vec <- R6::R6Class(
  "borrow_vec",
  public = list(
    base_type = NULL,
    print = function() {
      cat(sprintf("vector (borrowed) containing: %s", self$base_type), "\n")
    }
  )
)
borrow_mat <- R6::R6Class(
  "borrow_mat",
  public = list(
    base_type = NULL,
    print = function() {
      cat(sprintf("matrix (borrowed) containing: %s", self$base_type), "\n")
    }
  )
)
borrow_arr <- R6::R6Class(
  "borrow_arr",
  public = list(
    base_type = NULL,
    print = function() {
      cat(sprintf("array (borrowed) containing: %s", self$base_type), "\n")
    }
  )
)

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

    t <- pre_type_node$new()
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
    v <- vec$new()
    v$base_type <- deparse(code[[2L]])
    return(v)
  }
  else if (operator %in% c("mat", "matrix")) {
    m <- mat$new()
    m$base_type <- deparse(code[[2L]])
    return(m)
  }
  else if (operator == "array") {
    a <- arr$new()
    a$base_type <- deparse(code[[2L]])
    return(a)
  }

  else if (operator %in% c("borrow_vec", "borrow_vector") && env$fct_input) {
    v <- borrow_vec$new()
    v$base_type <- deparse(code[[2L]])
    return(v)
  }
  else if (operator %in% c("borrow_mat", "borrow_matrix") && env$fct_input) {
    m <- borrow_mat$new()
    m$base_type <- deparse(code[[2L]])
    return(m)
  }
  else if (operator == "borrow_array" && env$fct_input) {
    a <- borrow_arr$new()
    a$base_type <- deparse(code[[2L]])
    return(a)
  }

  else if (operator == "const" && env$fct_input) {
    t <- parse_type_tree(code[[2L]],env)
    t$const_or_mut <- "const"
    return(t)
  }
  else if (operator == "ref" && env$fct_input) {
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

new_type_node <- R6::R6Class(
  "new_type_node",
  public = list(
    name = NULL,
    slots = NULL,
    initialize = function(name) {
      self$name <- name
    },
    print = function() {
      cat(self$name, "\n")
      cat("=============================\n")
      for (i in self$slots) {
        print(i)
        cat("------------------------------\n")
      }
    },
    define_type = function() {
      ts <- lapply(self$slots, function(s) {
        s$stringify("\t")
      })
      paste0(
        "struct ", self$name, "{\n",
          paste0(ts, collapse = "\n"),
        "\n",
        "};"
      )
    },
    create_cast_function = function(indent = "") {
      sprintf(
        "static inline %s SEXP2%s(SEXP arg);", self$name, self$name
      )
    }
  )
)
parse_new_type <- function(code, env) {
  code <- as.list(code)
  operator <- deparse(code[[1L]])
  if (operator == "new_type") {
    env$new_type <- new_type_node$new(code[[2L]])
    parse_new_type(code[[3]], env)
  }
  if (operator == "slots") {
    code <- code[-1]
    env$new_type$slots <- list()
    for (i in seq_len(length(code))) {
      if (is.symbol(code[[i]])) {
        env$new_type$slots[[i]] <- env$list[[deparse(code[[i]])]]
      } else {
        operator <- deparse(code[[i]][[1L]])
        if (operator == "type") {
          env$new_type$slots[[i]] <- parse_type_tree(code[[i]], env)
        } else if (operator == "new_type") {
          env$new_type$slots[[i]] <- parse_type_tree(code[[i]], env)
        } else {
          env$error <- sprintf("Found unsupported function %s", operator)
        }
      }
    }
  }
}

parse_types <- function(block) {
  env <- new.env(parent = emptyenv())
  env$list <- list()
  for (i in seq_len(length(block))) {
    row <- block[[i]]
    operator <- deparse(row[[1L]])
    if (operator == "type") {
      env$list[[i]] <- parse_type_tree(row, env)
    } else if (operator == "new_type") {
      parse_new_type(row, env)
      env$list[[env$new_type$name]] <- env$new_type
    } else {
      env$error <- sprintf("Found unsupported function %s", operator)
    }
  }
  env$list
}

f <- function() {
  new_type(
    "cool_type",
    slots(
      a |> type(int),
      b |> type(double),
      c |> type(vec(int))
    )
  )
  new_type(
    "awesome_type",
    slots(
      cool_type,
      c |> type(mat(double))
    )
  )
}
res <- parse_types(body(f)[-1])
res
t <- res$cool_type$define_type()
cat(t, "\n")
t <- res$cool_type$create_cast_function()
cat(t, "\n")

Rcpp::sourceCpp("./development/read_list.cpp")
# Construction in R
ct <- structure(
  list(a = 1L, b = 3.1415, c = c(1L, 2L, 3L)),
  class = "cool_type"
)
inspect_list(ct)

l <- list(a = 1, b = 2)
l + l
structure(as.list(CO2), class = "CO2")
