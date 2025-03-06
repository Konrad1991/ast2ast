variable_node <- R6::R6Class(
  "variable_node",
  public = list(
    name = NULL,
    type = NULL,
    error = NULL,
    context = NULL,
    initialize = function(obj) {
      self$name <- deparse(obj)
    },
    stringify = function(indent = "") {
      return(paste0(indent, self$name))
    },
    stringify_error = function(indent = "") {
      return(paste0(indent, self$error$error_message))
    }
  )
)

literal_node <- R6::R6Class(
  "literal_node",
  public = list(
    name = NULL,
    error = NULL,
    context = NULL,
    initialize = function(obj) {
      self$name <- deparse(obj)
    },
    stringify = function(indent = "") {
      t_literal(self$context, self$name, indent)
    },
    stringify_error = function(indent = "") {
      return(paste0(indent, self$error$error_message))
    }
  )
)

handle_var <- function(code, context) {
  if (rlang::is_symbol(code)) {
    if (context == "[" && deparse(code) == "") {
      # NOTE: empty indexing --> change all entries
      ln <- literal_node$new(TRUE)
      ln$context <- context
      return(ln)
    }
    # NOTE: dont know why but short forms T and F
    # are returned as symbols and not as logicals
    if (deparse(code) == "T") {
      ln <- literal_node$new(TRUE)
      ln$context <- context
      return(ln)
    } else if (deparse(code) == "F") {
      ln <- literal_node$new(FALSE)
      ln$context <- context
      return(ln)
    }
    vn <- variable_node$new(code)
    vn$context <- context
    return(vn)
  }
  ln <- literal_node$new(code)
  ln$context <- context
  return(ln)
}

binary_node <- R6::R6Class(
  "binary_node",
  public = list(
    operator = NULL,
    left_node = NULL,
    left_node_name = NULL,
    right_node = NULL,
    right_node_name = NULL,
    error = NULL,
    context = NULL,
    remove_type_decl = FALSE,
    initialize = function() {},
    string_left = function() {
      return(self$left_node$stringify())
    },
    string_right = function() {
      return(self$right_node$stringify())
    },
    create_infix_string = function(indent = "") {
      paste0(
        indent,
        self$string_left(),
        " ",
        self$operator, " ",
        self$string_right(),
        ""
      )
    },
    create_function_string = function(indent = "") {
      paste0(
        indent,
        self$operator, "(",
        self$string_left(),
        ", ",
        self$string_right(),
        ")"
      )
    },
    stringify = function(indent = "") {
      ret <- ""
      if (self$operator == "%type%" && self$remove_type_decl) {
        if (self$context == "Start") {
          return(ret)
        }
        ret <- paste0(indent, self$string_left())
      } else if (infix_or_function(self$operator) == "infix") {
        ret <- self$create_infix_string(indent)
      } else if (infix_or_function(self$operator) == "function") {
        ret <- self$create_function_string(indent)
      } else {
        stop(paste0(
          "Unexpected error in Binary operator",
          self$operator, " ",
          self$string_left(),
          " ", self$string_right()
        ))
      }
      if (self$context == "Start") {
        ret <- paste0(ret, ";")
      }
      return(ret)
    },
    stringerror_left = function() {
      return(self$left_node$stringify_error())
    },
    stringerror_right = function() {
      return(self$right_node$stringify_error())
    },
    stringify_error = function(indent = "") {
      left_error <- self$stringerror_left()
      right_error <- self$stringerror_right()
      op_error <- self$error$error_message
      errors <- c(left_error, right_error, op_error)
      errors <- errors[errors != ""]
      errors <- combine_strings(errors)
      ret <- paste0(
        indent,
        errors
      )
      return(ret)
    },
    stringify_error_line = function(indent = "") {
      self$stringify()
    }
  )
)

# Define the unary_node class
unary_node <- R6::R6Class(
  "unary_node",
  public = list(
    operator = NULL,
    obj = NULL,
    obj_name = NULL,
    error = NULL,
    context = NULL,
    initialize = function() {},
    string_obj = function() {
      return(self$obj$stringify())
    },
    stringify = function(indent = "") {
      if (self$operator == "-") { # NOTE: for unary -
        ret <- paste0(
          indent, self$operator, "",
          self$string_obj(), ""
        )
      } else if (self$operator == "(") {
        ret <- paste0(
          indent, self$operator,
          self$string_obj(), ")"
        )
      } else {
        ret <- paste0(
          indent, self$operator, "(",
          self$string_obj(), ")"
        )
      }
      if (self$context == "Start") {
        ret <- paste0(ret, ";")
      }
      return(ret)
    },
    stringify_error = function(indent = "") {
      obj_error <- self$obj$stringify_error()
      op_error <- self$error$error_message
      errors <- c(obj_error, op_error)
      errors <- errors[errors != ""]
      errors <- combine_strings(errors)
      ret <- paste0(
        indent,
        errors
      )
      return(ret)
    },
    stringify_error_line = function(indent = "") {
      self$stringify()
    }
  )
)

nullary_node <- R6::R6Class(
  "nullary_node",
  public = list(
    operator = NULL,
    error = NULL,
    context = NULL,
    initialize = function() {},
    stringify = function(indent = "") {
      if (self$operator == "next" || self$operator == "break") {
        return(paste0(indent, self$operator, ";"))
      }
      ret <- paste0(indent, self$operator, "()")
      if (self$context == "Start") {
        ret <- paste0(ret, ";")
      }
      return(ret)
    },
    stringify_error = function(indent = "") {
      return(paste0(indent, self$error$error_message))
    },
    stringify_error_line = function(indent = "") {
      self$stringify()
    }
  )
)

# Define the function_node class
function_node <- R6::R6Class(
  "function_node",
  public = list(
    operator = NULL,
    error = NULL,
    context = NULL,
    args = list(),
    args_names = list(),
    initialize = function() {},
    stringify = function(indent = "") {
      args_string <- lapply(self$args, function(arg) {
        return(arg$stringify())
      }) |>
        unlist() |>
        c() |>
        paste(collapse = ", ")
      ret <- paste0(
        indent, self$operator, "(",
        args_string,
        ")"
      )
      if (self$context == "Start") {
        ret <- paste0(ret, ";")
      }
      return(ret)
    },
    stringify_error = function(indent = "") {
      args_errors <- lapply(self$args, function(arg) {
        arg$stringify_error()
      }) |>
        unlist() |>
        c()
      errors <- c(self$error$error_message, args_errors)
      errors <- errors[errors != ""]
      errors <- combine_strings(errors)
      ret <- paste0(
        indent,
        errors
      )
      return(ret)
    },
    stringify_error_line = function(indent = "") {
      self$stringify()
    }
  )
)

handle_if <- function(code, i_node, operator) {
  i_node$condition <- code[[2]] |> process(operator)
  i_node$true_node <- code[[3]] |> process(operator)
  if (length(code) == 4) {
    s <- code[[4]]
    while (is.call(s) && deparse(s[[1]]) == "if") {
      else_i_node <- if_node$new()
      else_i_node$condition <- s[[2]] |> process(operator)
      else_i_node$true_node <- s[[3]] |> process(operator)
      i_node$else_if_nodes[[
        length(i_node$else_if_nodes) + 1
      ]] <- else_i_node
      if (length(s) == 4) {
        s <- s[[4]]
      } else {
        break
      }
    }
    if (!is.null(s)) {
      if (deparse(s[[1]]) == "if") {
        else_i_node <- if_node$new()
        else_i_node$condition <- s[[2]] |> process(operator)
        else_i_node$true_node <- s[[3]] |> process(operator)
        i_node$else_if_nodes[[
          length(i_node$else_if_nodes) + 1
        ]] <- else_i_node
      } else {
        i_node$false_node <- process(s, operator)
      }
    }
  }
}

# Define the if_node class
if_node <- R6::R6Class(
  "if_node",
  public = list(
    condition = NULL,
    true_node = NULL,
    else_if_nodes = NULL,
    false_node = NULL,
    error = NULL,
    context = NULL,
    initialize = function() {},
    string_condition = function(indent) {
      return(self$condition$stringify(indent = paste0(indent, "")))
    },
    string_true = function(indent) {
      return(self$true_node$stringify(indent = paste0(indent, "  ")))
    },
    string_else_if = function(indent) {
      l <- lapply(self$else_if_nodes, function(node) {
        return(node$stringify(indent = paste0(indent, "")))
      })
      l <- paste0(" else ", l)
      combine_strings(l, "")
    },
    string_false = function(indent) {
      return(self$false_node$stringify(indent = paste0(indent, "  ")))
    },
    stringify = function(indent = "") {
      result <- ""
      result <- paste0(
        indent, "if (",
        self$string_condition(""), ") {\n"
      )
      result <- paste0(
        result,
        self$string_true(indent),
        "\n",
        indent,
        "}"
      )
      result <- combine_strings(result)
      if (!is.null(self$else_if_nodes)) {
        result <- paste0(
          result,
          self$string_else_if(indent)
        )
      }
      if (!is.null(self$false_node)) {
        result <- paste0(result, indent, " else {\n")
        result <- paste0(
          result, self$false_node$stringify(
            indent = paste0(indent, "  ")
          ), indent, "\n}"
        )
      }
      result <- combine_strings(result, "")
      return(result)
    },
    stringify_condition_error = function(indent = "") {
      return(self$condition$stringify_error(
        indent = paste0(indent, "")
      ))
    },
    stringify_true_node_error = function(indent = "") {
      return(self$true_node$stringify_error(
        indent = paste0(indent, "")
      ))
    },
    stringify_false_node_error = function(indent = "") {
      if (is.null(self$false_node)) {
        return(indent)
      }
      return(self$false_node$stringify_error(
        indent = paste0(indent, "")
      ))
    },
    stringify_error = function(indent = "") {
      condition_error <- self$stringify_condition_error()
      true_error <- self$stringify_true_node_error()
      false_error <- self$stringify_false_node_error()
      errors <- c(condition_error, true_error, false_error)
      errors <- errors[errors != ""]
      errors <- combine_strings(errors)
      ret <- paste0(
        indent,
        errors
      )
      return(ret)
    },
    stringify_error_line = function(indent = "") {
      condition_error <- self$stringify_condition_error()
      if (condition_error != "") {
        return(paste0(
          indent, "if (",
          self$string_condition(""), ") {"
        ))
      }
      true_error <- self$stringify_true_node_error()
      if (true_error != "") {
        return(self$true_node$stringify_error_line())
      }
      false_error <- self$stringify_false_node_error()
      if (false_error != "") {
        return(self$false_node$stringify_error_line())
      }
    }
  )
)

# Define the block_node class
block_node <- R6::R6Class(
  "block_node",
  public = list(
    block = NULL,
    error = NULL,
    context = NULL,
    initialize = function() {},
    stringify = function(indent = "") {
      result <- list()
      for (stmt in self$block) {
        result[[length(result) + 1]] <-
          paste0(
            stmt$stringify(indent = indent), ";"
          )
      }
      result <- combine_strings(result)
      return(result)
    },
    stringify_error = function(indent = "") {
      if (!inherits(self$block, "list")) {
        return("")
      }
      res <- lapply(self$block, function(elem) {
        return(elem$stringify_error())
      })
      for (i in seq_along(res)) {
        if (!is.null(res[[i]]) && res[[i]] != "") {
          return(res[[i]])
        }
      }
      return("")
    },
    stringify_error_line = function(indent = "") {
      if (!inherits(self$block, "list")) {
        return("")
      }
      # In which line is the error
      error_lines <- lapply(self$block, function(elem) {
        return(elem$stringify_error())
      })
      idx <- c()
      for (i in seq_along(error_lines)) {
        if (!is.null(error_lines[[i]]) && error_lines[[i]] != "") {
          idx <- c(idx, i)
        }
      }
      # Stringify the error line(s)
      res <- lapply(self$block, function(elem) {
        return(elem$stringify())
      })

      res <- combine_strings(res[idx])
      return(res)
    }
  )
)

# Define the error_node class
error_node <- R6::R6Class(
  "error_node",
  public = list(
    error_message = NULL,
    context = NULL,
    initialize = function(error_message) {
      self$error_message <- error_message
    }
  )
)

# Define the for loop node
for_node <- R6::R6Class(
  "for_node",
  public = list(
    error = NULL,
    i = NULL,
    seq = NULL,
    block = NULL,
    context = NULL,
    initialize = function() {},
    stringify = function(indent = "") {
      idx <- self$i$stringify(indent)
      sequence <- self$seq$stringify(indent)
      b <- self$block$stringify(paste0(indent, "  "))
      return(paste0(
        indent,
        "for(auto& ", idx, " : ", sequence, ") {\n",
        b, "\n",
        "}\n"
      ))
    },
    stringify_error = function(indent = "") {
      block_err <- self$block$stringify_error()
      i_err <- self$i$stringify_error()
      seq_err <- self$seq$stringify_error()
      res <- c(i_err, seq_err, block_err)
      res <- res[res != ""]
      e <- paste0(i_err, seq_err, block_err, collapse = "")
      if (e != "") {
        return(
          combine_strings(c(i_err, seq_err, block_err))
        )
      }
      return("")
    },
    stringify_error_line = function(indent = "") {
      i_err <- self$i$stringify_error()
      if (i_err != "") {
        # NOTE: const can be used as assignment to index variable is not allowed
        # Even though this is a resitriction
        return(paste0(
          indent, "for (const auto&",
          self$i$stringify(""), " : ", self$seq$stringify(), ") {"
        ))
      }
      seq_err <- self$seq$stringify_error()
      if (seq_err != "") {
        return(paste0(
          indent, "for (",
          self$i$stringify(""), " in ", self$seq$stringify(), ") {"
        ))
      }
      block_err <- self$block$stringify_error()
      if (block_err != "") {
        return(self$block$stringify_error_line())
      }
    }
  )
)
