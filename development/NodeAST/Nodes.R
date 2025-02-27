# Node class.
# All nodes inherit from it
# Thereby, one can use inherits(instance, "Node")
Node <- R6::R6Class(
  "Node",
  public = list(root = "root")
)

VariableNode <- R6::R6Class(
  "VariableNode",
  inherit = Node,
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

LiteralNode <- R6::R6Class(
  "LiteralNode",
  inherit = Node,
  public = list(
    name = NULL,
    error = NULL,
    context = NULL,
    dont_change_number = c("print", ":", "[", "at"),
    initialize = function(obj) {
      self$name <- deparse(obj)
    },
    stringify = function(indent = "") {
      if (self$context %in% self$dont_change_number) {
        return(paste0(indent, self$name))
      } else if (numeric_char(self$name)) {
        if (!grepl("\\.", self$name)) {
          return(paste0(self$name, ".0"))
        }
        return(paste0(indent, self$name))
      }
    },
    stringify_error = function(indent = "") {
      return(paste0(indent, self$error$error_message))
    }
  )
)

handle_var <- function(code, context) {
  if (rlang::is_symbol(code)) {
    vn <- VariableNode$new(code)
    vn$context <- context
    return(vn)
  }
  ln <- LiteralNode$new(code)
  ln$context <- context
  return(ln)
}

# TODO: add exception for %type%
# Define the BinaryNode
BinaryNode <- R6::R6Class(
  "BinaryNode",
  inherit = Node,
  public = list(
    operator = NULL,
    left_node = NULL,
    left_node_name = NULL,
    right_node = NULL,
    right_node_name = NULL,
    error = NULL,
    context = NULL,
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
      if (infix_or_function(self$operator) == "infix") {
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
      if (inherits(self$left_node, "Node")) {
        return(self$left_node$stringify_error())
      }
      return("")
    },
    stringerror_right = function() {
      if (inherits(self$right_node, "Node")) {
        return(self$right_node$stringify_error())
      }
      return("")
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

# Define the UnaryNode class
UnaryNode <- R6::R6Class(
  "UnaryNode",
  inherit = Node,
  public = list(
    operator = NULL,
    obj = NULL,
    obj_name = NULL,
    error = NULL,
    context = NULL,
    initialize = function() {},
    string_obj = function() {
      if (inherits(self$obj, "Node")) {
        return(self$obj$stringify())
      }
      return(self$obj)
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
      if (!inherits(self$obj, "Node")) {
        return(self$error$error_message)
      }
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

# TODO: add exception for next/continue and break
# Define the NullaryNode class
NullaryNode <- R6::R6Class(
  "NullaryNode",
  inherit = Node,
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

# Define the FunctionNode class
FunctionNode <- R6::R6Class(
  "FunctionNode",
  inherit = Node,
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

# Define the IfNode class
IfNode <- R6::R6Class(
  "IfNode",
  inherit = Node,
  public = list(
    condition = NULL,
    true_node = NULL,
    false_node = NULL,
    error = NULL,
    context = NULL,
    initialize = function() {},
    string_condition = function(indent) {
      if (inherits(self$condition, "Node")) {
        return(self$condition$stringify(indent = paste0(indent, "")))
      }
      return(self$condition)
    },
    string_true = function(indent) {
      if (inherits(self$true_node, "Node")) {
        return(self$true_node$stringify(indent = paste0(indent, "  ")))
      }
      return(self$true_node)
    },
    string_false = function(indent) {
      if (inherits(self$false_node, "Node")) {
        return(self$false_node$stringify(indent = paste0(indent, "  ")))
      }
      return(self$false_node)
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
        "\n"
      )
      if (!is.null(self$false_node)) {
        result <- paste0(result, indent, "} else {\n")
        result <- paste0(
          result, self$false_node$stringify(
            indent = paste0(indent, "  ")
          ), "\n"
        )
      }
      result <- paste0(result, indent, "}")
      return(result)
    },
    stringify_condition_error = function(indent = "") {
      if (!inherits(self$condition, "Node")) {
        return(indent)
      }
      return(self$condition$stringify_error(
        indent = paste0(indent, "")
      ))
    },
    stringify_true_node_error = function(indent = "") {
      if (!inherits(self$true_node, "Node")) {
        return(indent)
      }
      return(self$true_node$stringify_error(
        indent = paste0(indent, "")
      ))
    },
    stringify_false_node_error = function(indent = "") {
      if (is.null(self$false_node)) {
        return(indent)
      }
      if (!inherits(self$false_node, "Node")) {
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

# Define the BlockNode class
BlockNode <- R6::R6Class(
  "BlockNode",
  inherit = Node,
  public = list(
    block = NULL,
    error = NULL,
    context = NULL,
    initialize = function() {},
    stringify = function(indent = "") {
      result <- list()
      for (stmt in self$block) {
        if (inherits(stmt, "Node")) {
          result[[length(result) + 1]] <-
            stmt$stringify(indent = indent)
        } else {
          result[[length(result) + 1]] <- paste0(indent, stmt)
        }
      }
      result <- combine_strings(result)
      return(result)
    },
    stringify_error = function(indent = "") {
      if (!inherits(self$block, "list")) {
        return("")
      }
      res <- lapply(self$block, function(elem) {
        if (inherits(elem, "Node")) {
          return(elem$stringify_error())
        }
        return("")
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
        if (inherits(elem, "Node")) {
          return(elem$stringify_error())
        }
        return("")
      })
      idx <- c()
      for (i in seq_along(error_lines)) {
        if (!is.null(error_lines[[i]]) && error_lines[[i]] != "") {
          idx <- c(idx, i)
        }
      }
      # Stringify the error line(s)
      res <- lapply(self$block, function(elem) {
        if (inherits(elem, "Node")) {
          return(elem$stringify())
        }
        return("")
      })

      res <- combine_strings(res[idx])
      return(res)
    }
  )
)

# Define the ErrorNode class
ErrorNode <- R6::R6Class(
  "ErrorNode",
  public = list(
    error_message = NULL,
    context = NULL,
    initialize = function(error_message) {
      self$error_message <- error_message
    }
  )
)

# Define the for loop node
ForNode <- R6::R6Class(
  "ForNode",
  inherit = Node,
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
        return(paste0(
          indent, "for (auto&",
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
