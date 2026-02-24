variable_node <- R6::R6Class(
  "variable_node",
  public = list(
    id = NULL,
    name = NULL,
    error = NULL,
    context = NULL,
    internal_type = NULL,
    initialized = FALSE,
    initialize = function(obj) {
      self$name <- obj
    },
    stringify = function(indent = "") {
      return(paste0(indent, self$name))
    },
    stringify_error = function(indent = "") {
      return(paste0(indent, self$error))
    },
    print = function() {
      cat("Id:", self$id, "Variable:", deparse(self$name), "\n")
    }
  )
)

literal_node <- R6::R6Class(
  "literal_node",
  public = list(
    id = NULL,
    name = NULL,
    literal_type = NULL,
    error = NULL,
    context = NULL,
    internal_type = NULL,
    wrap = FALSE,
    real_type = NULL,
    initialize = function(obj) {
      self$literal_type <- determine_literal_type(obj)
      self$name <- deparse(obj)
    },
    stringify = function(indent = "") {
      t_literal(self$context, self$name, indent, self$literal_type, self$wrap, self$real_type)
    },
    stringify_error = function(indent = "") {
      return(paste0(indent, self$error))
    },
    print = function() {
      cat("Id:", self$id, self$stringify())
    }
  )
)

handle_var <- function(code, context) {
  if (is_symbol(code) && context == "{") {
    nn <- nullary_node$new()
    operator <- deparse(code)
    nn$operator <- operator
    nn$context <- context
    return(nn)
  } else
  if (is_symbol(code) && context != "{") {
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
    id = NULL,
    operator = NULL,
    left_node = NULL,
    right_node = NULL,
    error = NULL,
    context = NULL,
    internal_type = NULL,
    remove_type_decl = FALSE,
    type = NULL,
    is_infix = NULL,
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
    create_r_subsetting_string = function(indent = "") {
      if (self$operator == "[") {
        sprintf("%s%s[%s]", indent, self$string_left(), self$string_right())
      } else if (self$operator == "[[") {
        sprintf("%s%s[[%s]]", indent, self$string_left(), self$string_right())
      }
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
      if (self$operator == "type" && self$remove_type_decl) {
        if (self$context == "Start") {
          return(ret)
        }
        ret <- paste0(indent, self$string_left())
      } else if (self$operator %in% c("<-", "=") && inherits(self$right_node, "fn_node")) {
        return(self$create_infix_string(indent))
      } else if (self$operator %in% c("[", "[[")) {
        ret <- self$create_r_subsetting_string(indent)
      } else if (self$is_infix && !(self$operator %in% not_infix_in_cpp)) {
        ret <- self$create_infix_string(indent)
      } else if (!self$is_infix || self$operator %in% not_infix_in_cpp) {
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
      op_error <- self$error
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
    },
    print = function() {
      cat("Id:", self$id, self$stringify())
    }
  )
)

unary_node <- R6::R6Class(
  "unary_node",
  public = list(
    id = NULL,
    operator = NULL,
    obj = NULL,
    error = NULL,
    context = NULL,
    internal_type = NULL,
    handle_return = FALSE,
    output_is_r_fct = TRUE,
    initialize = function() {},
    string_obj = function() {
      return(self$obj$stringify())
    },
    stringify = function(indent = "") {
      ret <- NULL
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
        if (self$handle_return) {
          if (self$output_is_r_fct) {
            ret <- paste0(
              indent, self$operator, "(etr::Cast(",
              # This cast to SEXP and evals expressions which are not variables or literals
              self$string_obj(), "))"
            )
          } else {
            ret <- paste0(
              indent, self$operator, "(etr::Evaluate(", self$string_obj(), "))"
            )
          }
        } else {
          ret <- paste0(
            indent, self$operator, "(",
            self$string_obj(), ")"
          )
        }
      }
      if (self$context == "Start") {
        ret <- paste0(ret, ";")
      }
      return(ret)
    },
    stringify_error = function(indent = "") {
      obj_error <- self$obj$stringify_error()
      op_error <- self$error
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
    },
    print = function() {
      cat("Id: ", self$id, "\n", self$stringify(), "\n")
    }
  )
)

nullary_node <- R6::R6Class(
  "nullary_node",
  public = list(
    id = NULL,
    operator = NULL,
    error = NULL,
    context = NULL,
    internal_type = NULL,
    handle_return = FALSE,
    output_is_r_fct = TRUE,
    initialize = function() {},
    stringify = function(indent = "") {
      if (self$operator == "next" || self$operator == "break" || self$operator == "continue") {
        return(paste0(indent, self$operator, ";"))
      }
      ret <- NULL
      if (self$operator == "return") {
        # Handles void stuff
        if (self$output_is_r_fct) {
          ret <- paste0(indent, self$operator, "(etr::Cast())")
        } else {
          ret <- paste0(indent, self$operator, "(etr::Evaluate())")
        }
      } else {
        ret <- paste0(indent, self$operator, "()")
      }
      if (self$context == "Start") {
        ret <- paste0(ret, ";")
      }
      return(ret)
    },
    stringify_error = function(indent = "") {
      return(paste0(indent, self$error))
    },
    stringify_error_line = function(indent = "") {
      self$stringify()
    },
    print = function() {
      cat("Id: ", self$id, "\n", self$stringify(), "\n")
    }
  )
)

# Define the function_node class
function_node <- R6::R6Class(
  "function_node",
  public = list(
    id = NULL,
    operator = NULL,
    error = NULL,
    context = NULL,
    internal_type = NULL,
    args = list(),
    handle_vector = FALSE,
    initialize = function() {},
    create_r_subsetting_string = function(indent = "", obj_string, args_string) {
      if (self$operator == "[") {
        sprintf("%s%s[%s]", indent, obj_string, args_string)
      } else if (self$operator == "[[") {
        sprintf("%s%s[[%s]]", indent, obj_string, args_string)
      }
    },
    stringify = function(indent = "") {
      if (self$handle_vector) {
        s <- self$args[[1]]$stringify() |> remove_double_quotes()
        s <- paste0("etr::", s)
        self$operator <- s
        self$args[[1]] <- NULL
        self$args <- Filter(Negate(is.null), self$args)
      }
      if (self$operator %in% c("[", "[[")) {
        args_string <- lapply(self$args[-1], function(arg) {
          return(arg$stringify())
        }) |> unlist() |> c() |> paste(collapse = ", ")
        ret <- self$create_r_subsetting_string(indent, self$args[[1]]$stringify(), args_string)
      } else {
        args_string <- lapply(self$args, function(arg) {
          return(arg$stringify())
        }) |> unlist() |> c() |> paste(collapse = ", ")
        ret <- paste0(
          indent, self$operator, "(",
          args_string,
          ")"
        )
      }
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
      errors <- c(self$error, args_errors)
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
    },
    print = function() {
      cat("Id: ", self$id, "\n", self$stringify(), "\n")
    }
  )
)

if_node <- R6::R6Class(
  "if_node",
  public = list(
    id = NULL,
    condition = NULL,
    true_node = NULL,
    else_if_nodes = NULL,
    false_node = NULL,
    error = NULL,
    context = NULL,
    internal_type = NULL,

    initialize = function() {},
    string_condition = function(indent) {
      return(self$condition$stringify(indent = paste0(indent, "")))
    },
    string_true = function(indent) {
      return(self$true_node$stringify(indent = paste0(indent, "    ")))
    },
    string_else_if = function(indent) {
      l <- lapply(self$else_if_nodes, function(node) {
        return(node$stringify(indent = paste0(indent, "")))
      })
      # Remove white space until if
      l <- sub("^\\s*.*?(if\\s*\\()", "\\1", l)
      l <- paste0(" else ", l)
      return(l)
    },
    string_false = function(indent) {
      l <- self$false_node$stringify(indent = paste0(indent, "    "))
      return(l)
    },
    stringify = function(indent = "") {
      result <- ""
      result <- paste0(indent, "if (", self$string_condition(""), ") {\n")
      result <- paste0(result, self$string_true(indent), "\n", indent, "}")
      if (!is.null(self$else_if_nodes)) {
        result <- paste0(result, self$string_else_if(indent))
      }
      if (!is.null(self$false_node)) {
        result <- paste0(result, " else {\n")
        result <- paste0(result, self$string_false(indent), indent, "\n", indent, "}")
      }
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
    string_else_if_node_error = function(indent = "") {
      l <- lapply(self$else_if_nodes, function(node) {
        return(node$stringify_error(indent = paste0(indent, "")))
      })
      combine_strings(l, "")
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
      else_if_error <- self$string_else_if_node_error()
      false_error <- self$stringify_false_node_error()
      errors <- c(condition_error, true_error, else_if_error, false_error)
      errors <- errors[errors != ""]
      errors <- combine_strings(errors)
      ret <- paste0(
        indent,
        errors
      )
      return(ret)
    },
    stringify_error_line = function(indent = "") {
      s <- ""
      # Check condition
      condition <- self$stringify_condition_error() |> combine_strings("\n")
      if (condition != "") {
        s <- paste0(
          indent, "if (",
          self$string_condition(""), ") {\n",
          condition
        )
      }
      # Check true block
      true_block <- self$true_node$stringify_error_line()
      if (s == "" && true_block != "") {
        s <- true_block
      } else if (s != "" && true_block != "") {
        s <- sprintf("%s\n%s", s, true_block)
      }
      # Check the else if blocks
      else_if_blocks <- lapply(self$else_if_nodes, \(b) {
        b$stringify_error_line()
      }) |> combine_strings("\n")
      if (s == "" &&  else_if_blocks != "") {
        s <- else_if_blocks
      } else if (s != "" &&  else_if_blocks != "") {
        s <- sprintf("%s\n%s", s, else_if_blocks)
      }

      # Check the false block
      false_block <- ""
      if (!is.null(self$false_node)) {
        false_block <- self$false_node$stringify_error_line()
      }
      if (s == "" &&  false_block != "") {
        s <- false_block
      } else if (s != "" &&  false_block != "") {
        s <- sprintf("%s\n%s", s, false_block)
      }

      s
    },
    print = function() {
      cat("Id:", self$id, "if")
    }
  )
)

handle_if <- function(code, context, r_fct, i_node, function_registry) {
  i_node$condition <- code[[2]] |> process(context, r_fct, function_registry)
  i_node$true_node <- code[[3]] |> wrap_in_block() |> process(context, r_fct, function_registry)
  if (length(code) == 4) {
    s <- code[[4]]
    while (is.call(s) && deparse(s[[1]]) == "if") {
      else_i_node <- if_node$new()
      else_i_node$condition <- s[[2]] |> process(context, r_fct, function_registry)
      else_i_node$true_node <- s[[3]] |> wrap_in_block() |> process(context, r_fct, function_registry)
      i_node$else_if_nodes[[
        length(i_node$else_if_nodes) + 1
        ]] <- else_i_node
      if (length(s) == 4) {
        s <- s[[4]]
      } else {
        return()
      }
    }
    if (!is.null(s)) {
      if (deparse(s[[1]]) == "if") {
        else_i_node <- if_node$new()
        else_i_node$condition <- s[[2]] |> process(context, r_fct, function_registry)
        else_i_node$true_node <- s[[3]] |> wrap_in_block() |> process(context, r_fct, function_registry)
        i_node$else_if_nodes[[
          length(i_node$else_if_nodes) + 1
          ]] <- else_i_node
      } else {
        i_node$false_node <- process(wrap_in_block(s), context, r_fct, function_registry)
      }
    }
  }
}

block_node <- R6::R6Class(
  "block_node",
  public = list(
    id = NULL,
    block = NULL,
    error = NULL,
    context = NULL,
    internal_type = NULL,
    initialize = function() {},
    stringify = function(indent = "") {
      result <- list()
      for (stmt in self$block) {
        end <- ";"
        if (inherits(stmt, c("if_node", "for_node", "while_node", "repeat_node", "fn_node"))) {
          end <- ""
        }
        s <- stmt$stringify(indent = indent)
        result[[length(result) + 1]] <- paste0(s, end)
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
      }) |> remove_empty_strings() |> combine_strings("\n")
      return(res)
    },
    stringify_error_line = function(indent = "") {
      if (!inherits(self$block, "list")) {
        return("")
      }
      res <- lapply(self$block, function(elem) {
        if (class(elem)[[1]] %within% c("for_node", "if_node", "block_node", "while_node", "repeat_node")) {
          return(elem$stringify_error_line(indent))
        }
        error <- elem$stringify_error() |> combine_strings("\n")
        if (!is.null(error) && error != "") {
          line <- elem$stringify_error_line(indent)
          return(combine_strings(list(line, error), "\n"))
        }
        return(NULL)
      })
      res <- Filter(Negate(is.null), res)
      return(combine_strings(res, "\n\n"))
    },
    print = function() {
      cat("Id:", self$id, "{")
    }
  )
)

fn_node <- R6::R6Class(
  "fn_node",
  public = list(
    id = NULL,
    error = NULL,
    context = NULL,

    fct_name = NULL,
    args_f = NULL,
    args_f_raw = NULL,
    return_type = NULL,
    AST = NULL,
    parsed_AST = NULL,

    real_type = NULL,
    vars_types_list = NULL,
    internal_type = NULL,
    outermost = NULL,
    function_registry = NULL,
    function_registry_outer = NULL,

    initialize = function() {},

    stringify_signature = function(r_fct = FALSE) {
      return("")
    },
    stringify_declaration = function(indent = "", r_fct) {
      if (length(self$vars_types_list) >= 1L) {
        if (length(self$vars_types_list) >= 1L) {
          args <- lapply(self$vars_types_list, \(x) x$stringify_signature(r_fct = FALSE)) |> unlist() |> c()
          args <- args[args != ""]
          args <- paste0(args, collapse = ", ")
        } else {
          args <- ""
        }
      }
      ret_type <- self$return_type$generate_type("")
      paste0(indent, sprintf("std::function<%s(%s)>%s;\n", ret_type, args, self$fct_name))
    },

    stringify = function(indent = "") {
      name <- self$fct_name
      indent0 <- indent

      if (length(self$vars_types_list) >= 1L) {
        args <- lapply(self$vars_types_list, \(x) x$stringify_signature(FALSE)) |> unlist() |> c()
        args <- args[args != ""]
      } else {
        args <- ""
      }
      declarations <- lapply(self$vars_types_list, \(x) {
        res <- x$stringify_declaration(indent = "", r_fct = FALSE)
        paste0(indent, "     ", res)
      }) |> unlist() |> c()
      declarations <- declarations[declarations != ""]
      declarations <- paste0(declarations, collapse = "\n")

      ret_type <- self$return_type$generate_type("")

      body <- ""
      if (inherits(self$AST, "block_node")) {
        body <- self$AST$stringify(indent = paste0(indent, "    "))
      }
      body <- paste0(body, collapse = "")
      args <- paste0(args, collapse = ", ")

      paste0(
        paste0("[&]( ", args, " ) -> ", ret_type, " {\n"),
        indent, declarations, "\n",
        body, "\n",
        paste0(indent, "  }")
      )
    },

    stringify_error = function(indent = "") {
      e <- self$error
      if (is.null(e)) return("")
      if (inherits(e, "list")) e <- unlist(e) |> c()
      e <- e[e != ""]
      if (length(e) == 0) return("")
      combine_strings(e, "\n") |> paste0(indent, .)
    },

    stringify_error_line = function(indent = "") {
      header <- ""
      name <- self$fct_name
      if (is.null(name) || name == "") name <- paste0("fn_", self$id)
      args <- character()
      if (!is.null(self$args_f) && inherits(self$args_f, "list")) {
        normal_vars <- self$vars_types_list[sapply(self$vars_types_list, \(x) inherits(x, "type_node"))]
        args <- lapply(normal_vars, function(x) {
          x$stringify_signature(r_fct = FALSE)
        }) |> unlist() |> c()
        args <- args[args != ""]
      }
      ret_type <- self$return_type$generate_type("")
      header <- paste0(
        indent, "auto ", name, " = [&]( ", paste(args, collapse = ", "), " ) -> ",
        ret_type, " { ... };"
      )

      err <- self$stringify_error(indent = "")
      if (err == "") return("")

      combine_strings(list(header, err), "\n")
    }
  )
)

for_node <- R6::R6Class(
  "for_node",
  public = list(
    id = NULL,
    error = NULL,
    i = NULL,
    seq = NULL,
    block = NULL,
    context = NULL,
    internal_type = NULL,
    initialize = function() {},
    stringify = function(indent = "") {
      idx <- self$i$stringify("")
      sequence <- self$seq$stringify("")
      b <- self$block$stringify(paste0(indent, " "))
      if (inherits(self$seq, "unary_node") && self$seq$operator %in% c("etr::seq_len", "etr::seq_along")) {
        until <- paste0("etr::length_seq(", self$seq$obj$stringify(indent = ""))
        if (self$seq$operator == "etr::seq_along") until <- paste0("etr::length(", self$seq$obj$stringify(indent = ""))
        return(paste0(
          indent,
          "for(etr::Integer ", idx, " = 1; ",
          idx, " <= ", until, "); ", idx, " = ", idx, " + etr::Integer(1)) {\n",
          b, "\n",
          indent, "}\n"
        ))
      } else {
        return(paste0(
          indent,
          "for(const auto ", idx, " : ", sequence, ") {\n",
          b, "\n",
          indent, "}\n"
        ))
      }
    },
    stringify_error = function(indent = "") {
      block_err <- self$block$stringify_error()
      i_err <- self$i$stringify_error()
      seq_err <- self$seq$stringify_error()
      res <- c(i_err, seq_err, block_err)
      res <- res[res != ""]
      e <- combine_strings(list(res), "\n")
      if (e != "") {
        return(e)
      }
      return("")
    },
    stringify_error_line = function(indent = "") {
      i_err <- self$i$stringify_error()
      seq_err <- self$seq$stringify_error()
      i_seq_line <- (paste0(
        "for (", self$i$stringify(""), " in ", self$seq$stringify(""), ") {"
      ))
      if (i_err != "" && seq_err != "") {
        i_seq_line <- paste0(i_seq_line, "\n", i_err, "\n", seq_err)
      } else if (i_err == "" && seq_err != "") {
        i_seq_line <- paste0(i_seq_line, "\n", seq_err)
      } else if (i_err != "" && seq_err == "") {
        i_seq_line <- paste0(i_seq_line, "\n", i_err)
      } else {
        i_seq_line <- ""
      }
      block_res <- self$block$stringify_error_line(paste0(indent, "  "))
      if (i_seq_line == "" && block_res == "") {
        return("")
      } else if (i_seq_line != "" && block_res == "") {
        return(i_seq_line)
      } else if (i_seq_line == "" && block_res != "") {
        return(block_res)
      }
      return(combine_strings(list(i_seq_line, block_res), "\n\n"))
    },
    print = function() {
      cat(self$stringify(), "\n")
    }
  )
)

while_node <- R6::R6Class(
  "while_node",
  public = list(
    id = NULL,
    error = NULL,
    condition = NULL,
    block = NULL,
    context = NULL,
    internal_type = NULL,
    initialize = function() {},
    stringify = function(indent = "") {
      cond <- self$condition$stringify(indent)
      b <- self$block$stringify(paste0(indent, " "))
      return(paste0(
        indent,
        "while(", cond, ") {\n",
        b, "\n",
        "}\n"
      ))
    },
    stringify_error = function(indent = "") {
      block_err <- self$block$stringify_error()
      cond_err <- self$condition$stringify_error()
      res <- c(cond_err, block_err)
      res <- res[res != ""]
      e <- combine_strings(list(res), "\n")
      if (e != "") {
        return(e)
      }
      return("")
    },
    stringify_error_line = function(indent = "") {
      block_res <- self$block$stringify_error_line(paste0(indent, "  "))
      cond_err <- self$condition$stringify_error()
      res <- ""
      if (block_res == "" && cond_err == "") {
        return("")
      }
      if (cond_err != "" && block_res == "") {
        cond <- self$condition$stringify(indent)
        return(paste0(cond, "\n", cond_err))
      }
      if (cond_err == "" && block_res != "") {
        return(block_res)
      }
      res <- combine_strings(list(res, block_res), "\n\n")
      return(res)
    },
    print = function() {
      cat(self$stringify(), "\n")
    }
  )
)

repeat_node <- R6::R6Class(
  "repeat_node",
  public = list(
    id = NULL,
    error = NULL,
    block = NULL,
    context = NULL,
    internal_type = NULL,
    initialize = function() {},
    stringify = function(indent = "") {
      b <- self$block$stringify(paste0(indent, " "))
      return(paste0(
        indent,
        "while(true) {\n",
        b, "\n",
        "}\n"
      ))
    },
    stringify_error = function(indent = "") {
      block_err <- self$block$stringify_error()
      block_err <- block_err[block_err != ""]
      e <- combine_strings(list(block_err), "\n")
      if (e != "") {
        return(e)
      }
      return("")
    },
    stringify_error_line = function(indent = "") {
      block_res <- self$block$stringify_error_line(paste0(indent, "  "))
      if (block_res == "") {
        return("")
      }
      return(block_res)
    },
    print = function() {
      cat(self$stringify(), "\n")
    }
  )
)

unknown_type <- R6::R6Class(
  "unknown_type",
  public = list()
)

type_node <- R6::R6Class(
  "type_node",
  public = list(
    id = NULL,
    name = NULL,
    tree = NULL,
    error = NULL,
    base_type = NULL,
    data_struct = NULL,
    const_or_mut = "mutable",
    copy_or_ref = "copy",
    fct_input = FALSE,
    type_dcl = FALSE,
    iterator = FALSE,
    r_fct = TRUE,
    real_type = "etr::Double",

    within_type_call = FALSE,

    initialize = function(tree, fct_input, r_fct) {
      self$tree <- tree
      self$fct_input <- fct_input
      self$r_fct <- r_fct
    },

    handle_type = function(type) {
      self$name <- deparse(type[[1]])
      type <- as.list(type[[2]])
      if (length(type) == 1) {
        self$data_struct <- "scalar"
        self$base_type <- deparse(type[[1]])
      } else if (length(type) == 2) {
        self$data_struct <- deparse(type[[1]])
        self$base_type <- deparse(type[[2]])
      } else {
        self$error <- c(self$error, "Too many arguments to function type")
      }
    },

    handle_const = function(fct, const) {
      if (length(const) > 1) {
        self$error <- c(self$error, "Function const accept only one argument")
      } else {
        self$const_or_mut <- fct
      }
    },

    handle_copy_or_ref = function(fct, copy_or_ref) {
      if (length(copy_or_ref) > 1) {
        self$error <- c(self$error, "Functions copy and ref accept only one argument")
      } else {
        self$copy_or_ref <- fct
      }
    },

    traverse = function(tree) {
      if (!is.call(tree)) {
        return(tree)
      }
      tree <- as.list(tree)
      fct <- deparse(tree[[1]])
      if (fct == "type") {
        self$within_type_call <- TRUE
        self$handle_type(tree[-1])
      } else if (fct == "const") {
        self$handle_const(fct, tree[-1])
        self$within_type_call <- FALSE
      } else if (fct %within% c("copy", "ref", "reference")) {
        self$handle_copy_or_ref(fct, tree[-1])
        self$within_type_call <- FALSE
      } else if (!self$within_type_call) {
        self$error <- c(self$error, sprintf("Found unsupported function: %s", fct))
        self$within_type_call <- FALSE
      }
      lapply(tree, self$traverse)
    },

    traverse_within_fct = function(tree) {
      if (!is.call(tree)) {
        return(tree)
      }
      tree <- as.list(tree)
      fct <- deparse(tree[[1]])
      if (fct == "type") {
        self$within_type_call <- TRUE
        self$handle_type(tree[-1])
      } else if (!self$within_type_call) {
        self$error <- c(self$error, sprintf("Found unsupported function: %s", fct))
        self$within_type_call <- FALSE
      }
      lapply(tree, self$traverse)
    },

    get_variable_from_arg = function() {
      types <- c(permitted_base_types(), permitted_data_structs(self$r_fct))
      av <- all.vars(self$tree)
      av <- av[!(av %within% types)]
      if (length(av) == 0) {
        self$error <- c(self$error,
          sprintf("Didn't found a variable within: %s", deparse(self$tree))
        )
      }
      if (length(av) > 1) {
        self$error <- c(self$error,
          sprintf("Found invalid types. Allowed are: %s and the type found is: %s",
            paste(types, collapse = ", "), deparse(self$tree)))
      }
      av
    },

    init = function() {
      self$traverse(self$tree)
      # Set default values
      if (is.null(self$base_type)) self$base_type <- "double"
      if (is.null(self$data_struct)) self$data_struct <- "vector"
      if (is.null(self$name)) {
        self$name <- self$get_variable_from_arg()
      }
    },

    init_within_fct = function() {
      self$traverse_within_fct(self$tree)
      # Set default values
      if (is.null(self$base_type)) self$base_type <- "double"
      if (is.null(self$data_struct)) self$data_struct <- "vector"
      if (is.null(self$name)) {
        self$name <- self$get_variable_from_arg()
      }
    },

    check_allowed_base_types = function() {
      if (!self$base_type %within% permitted_base_types()) {
        self$error <- c(self$error, sprintf("Found unsupported base type: %s", self$base_type))
      }
    },

    check_allowed_data_structs = function() {
      if (!(self$data_struct %within% permitted_data_structs(self$r_fct))) {
        self$error <- c(self$error, sprintf("Found unsupported data structure: %s", self$data_struct))
      }
    },

    check = function() {
      self$check_allowed_base_types()
      self$check_allowed_data_structs()
    },

    stringify = function(indent = "") {
      if (self$data_struct == "scalar") {
        return(paste0(indent, self$base_type))
      }
      return(paste0(indent, self$data_struct, "(", self$base_type, ")"))
    },
    stringify_error = function(indent = "") {
      return(paste0(indent, self$error))
    },

    generate_type = function(indent = "") {
      convert_types_to_etr_types(self$base_type, self$data_struct, self$r_fct, self$real_type, indent)
    },
    stringify_signature = function(r_fct) {
      if (!self$fct_input) return("")
      if (r_fct) {
        return(paste0("SEXP ", self$name, "SEXP"))
      }
      type <- convert_types_to_etr_types(self$base_type, self$data_struct, self$r_fct, self$real_type, "")
      if (self$const_or_mut == "const") type <- paste0("const ", type)
      if (self$copy_or_ref %within% c("ref", "reference")) type <- paste0(type, "&")
      paste0(type, " ", self$name)
    },
    stringify_declaration = function(indent = "", r_fct) {
      if (self$iterator) return("")
      if (!r_fct && self$fct_input) return("")
      if (r_fct && self$fct_input) {
        data_struct <- self$data_struct
        if(self$copy_or_ref != "copy") {
          data_struct <- paste0("borrow_", self$data_struct)
        }
        type <- convert_types_to_etr_types(self$base_type, data_struct, self$r_fct, self$real_type, indent)

        if (data_struct == "scalar") {
          conv_base_type <- convert_base_type(self$base_type, self$real_type)
          return(
            paste0(indent, type, " ", self$name, " = ",
              paste0("etr::SEXP2Scalar<", conv_base_type, ">(", self$name, "SEXP);")
            )
          )
        }
        return(
          paste0(indent, type, " ", self$name, " = ", paste0(self$name, "SEXP;"))
        )
      }
      type <- convert_types_to_etr_types(self$base_type, self$data_struct, self$r_fct, self$real_type, indent)
      paste0(indent, type, " ", self$name, ";")
    },

    print = function() {
      print("Type:")
      cat(
        "Name: ", self$name, "\n",
        "base type: ", self$base_type, "\n",
        "data struct: ", self$data_struct, "\n",
        "const or mut", self$const_or_mut, "\n",
        "copy or ref", self$copy_or_ref, "\n",
        "fct input", self$fct_input, "\n",
        "iterator", self$iterator, "\n"
      )
    }
  )
)
