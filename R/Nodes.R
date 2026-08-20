variable_node <- R6::R6Class(
  "variable_node",
  public = list(
    name = NULL,
    error = NULL,
    context = NULL,
    internal_type = NULL,
    initialized = FALSE,
    field_name = FALSE,
    pre_translate_line = NULL,
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
      cat("Variable:", deparse(self$name), "\n")
    }
  )
)

literal_node <- R6::R6Class(
  "literal_node",
  public = list(
    name = NULL,
    literal_type = NULL,
    error = NULL,
    context = NULL,
    internal_type = NULL,
    wrap = FALSE,
    real_type = NULL,
    pre_translate_line = NULL,
    initialize = function(obj) {
      self$literal_type <- determine_literal_type(obj)
      self$name <- deparse(obj)
      if (self$literal_type == "NULL") {
        self$error <- "NULL is not supported"
      }
    },
    stringify = function(indent = "") {
      t_literal(self$context, self$name, indent, self$literal_type, self$wrap, self$real_type)
    },
    stringify_error = function(indent = "") {
      return(paste0(indent, self$error))
    },
    stringify_error_line = function(indent = "") {
      self$stringify()
    },
    print = function() {
      cat(self$stringify())
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
      # empty indexing --> change all entries
      ln <- literal_node$new(TRUE)
      ln$context <- context
      return(ln)
    }
    # dont know why but short forms T and F
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
    right_node = NULL,
    error = NULL,
    context = NULL,
    internal_type = NULL,
    remove_type_decl = FALSE,
    type = NULL,
    is_infix = NULL,
    pre_translate_line = NULL,
    initialize = function() {},
    string_left = function() {
      return(self$left_node$stringify())
    },
    string_right = function() {
      if (inherits(self$right_node, "pre_type_node")) {
        return(self$right_node$describe())
      }
      return(self$right_node$stringify())
    },
    create_infix_string = function(indent = "") {
      if (self$operator %in% c("$", ".")) {
        return(paste0(indent, self$string_left(), self$operator, self$string_right()))
      }
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
      cat(self$stringify())
    }
  )
)

unary_node <- R6::R6Class(
  "unary_node",
  public = list(
    operator = NULL,
    obj = NULL,
    error = NULL,
    context = NULL,
    internal_type = NULL,
    handle_return = FALSE,
    output_is_r_fct = TRUE,
    pre_translate_line = NULL,
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
          if (needs_to_SEXP_path(self$obj$internal_type) && self$output_is_r_fct) {
            ret <- paste0(
              indent, self$operator, "(",
              self$string_obj(), ".to_SEXP())"
            )
          } else if (needs_to_SEXP_path(self$obj$internal_type)) {
            ret <- paste0(
              indent, self$operator, "(",
              self$string_obj(), ")"
            )
          } else if (self$output_is_r_fct) {
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
      cat(self$stringify(), "\n")
    }
  )
)

nullary_node <- R6::R6Class(
  "nullary_node",
  public = list(
    operator = NULL,
    error = NULL,
    context = NULL,
    internal_type = NULL,
    handle_return = FALSE,
    output_is_r_fct = TRUE,
    pre_translate_line = NULL,
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
      cat(self$stringify(), "\n")
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
    internal_type = NULL,
    args = list(),
    pre_translate_line = NULL,
    initialize = function() {},
    create_r_subsetting_string = function(indent = "", obj_string, args_string) {
      if (self$operator == "[") {
        sprintf("%s%s[%s]", indent, obj_string, args_string)
      } else if (self$operator == "[[") {
        sprintf("%s%s[[%s]]", indent, obj_string, args_string)
      }
    },
    stringify = function(indent = "") {
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
      cat(self$stringify(), "\n")
    }
  )
)

if_node <- R6::R6Class(
  "if_node",
  public = list(
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
      return(paste(l, collapse = ""))
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
      own_error <- self$error
      condition_error <- self$stringify_condition_error()
      true_error <- self$stringify_true_node_error()
      else_if_error <- self$string_else_if_node_error()
      false_error <- self$stringify_false_node_error()
      errors <- c(own_error, condition_error, true_error, else_if_error, false_error)
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
      condition <- c(self$error, self$stringify_condition_error())
      condition <- combine_strings(condition[condition != ""], "\n")
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
      cat("if")
    }
  )
)

handle_if <- function(code, context, env, i_node, function_registry) {
  i_node$condition <- code[[2]] |> process(context, env, function_registry)
  i_node$true_node <- code[[3]] |> wrap_in_block() |> process(context, env, function_registry)
  if (length(code) == 4) {
    s <- code[[4]]
    while (is.call(s) && deparse(s[[1]]) == "if") {
      else_i_node <- if_node$new()
      else_i_node$condition <- s[[2]] |> process(context, env, function_registry)
      else_i_node$true_node <- s[[3]] |> wrap_in_block() |> process(context, env, function_registry)
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
        else_i_node$condition <- s[[2]] |> process(context, env, function_registry)
        else_i_node$true_node <- s[[3]] |> wrap_in_block() |> process(context, env, function_registry)
        i_node$else_if_nodes[[
          length(i_node$else_if_nodes) + 1
          ]] <- else_i_node
      } else {
        i_node$false_node <- process(wrap_in_block(s), context, env, function_registry)
      }
    }
  }
}

block_node <- R6::R6Class(
  "block_node",
  public = list(
    block = NULL,
    error = NULL,
    context = NULL,
    internal_type = NULL,
    initialize = function() {},
    stringify = function(indent = "") {
      result <- list()
      for (stmt in self$block) {
        end <- ";"
        s <- stmt$stringify(indent = indent)
        if (inherits(stmt, c("if_node", "for_node", "while_node", "repeat_node", "fn_node"))) {
          end <- ""
        } else {
          line <- stmt$pre_translate_line
          if (is.null(line)) line <- s
          label <- escape_cpp_string_literal(line)
          s <- paste0(indent, "etr::current_line() = \"", label, "\";\n", s)
        }
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
      cat("{")
    }
  )
)

for_node <- R6::R6Class(
  "for_node",
  public = list(
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
      own_err <- self$error
      res <- c(i_err, seq_err, own_err, block_err)
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
      own_err <- self$error
      all_errs <- c(i_err, seq_err, own_err)
      all_errs <- all_errs[all_errs != ""]
      i_seq_line <- paste0(
        "for (", self$i$stringify(""), " in ", self$seq$stringify(""), ") {"
      )
      if (length(all_errs) > 0) {
        i_seq_line <- paste0(i_seq_line, "\n", combine_strings(all_errs, "\n"))
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
      res <- c(self$error, cond_err, block_err)
      res <- res[res != ""]
      e <- combine_strings(list(res), "\n")
      if (e != "") {
        return(e)
      }
      return("")
    },
    stringify_error_line = function(indent = "") {
      block_res <- self$block$stringify_error_line(paste0(indent, "  "))
      cond_err <- c(self$error, self$condition$stringify_error())
      cond_err <- combine_strings(cond_err[cond_err != ""], "\n")
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
  public = list(
    initialize = function() {}
  )
)

fn_node <- R6::R6Class(
  "fn_node",
  public = list(
    error = NULL,
    context = NULL,

    fct_name = NULL,
    known_fcts = NULL,
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
    known_types = NULL,

    initialize = function() {},

    stringify_signature = function(r_fct = FALSE) {
      return("")
    },
    signature = function(indent = "") {
      ""
    },
    declare = function(indent = "") {
      self$stringify_declaration(indent = indent, r_fct = FALSE)
    },
    stringify_declaration = function(indent = "", r_fct) {
      if (length(self$vars_types_list) >= 1L) {
        args <- lapply(self$vars_types_list, \(x) x$signature()) |> unlist() |> c()
        args <- args[args != ""]
        args <- paste0(args, collapse = ", ")
      } else {
        args <- ""
      }
      ret_type <- if (inherits(self$return_type, "new_type_node")) {
        self$return_type$stringify("")
      } else {
        self$return_type$data_struct$stringify("")
      }
      paste0(indent, sprintf("std::function<%s(%s)>%s;\n", ret_type, args, self$fct_name))
    },

    stringify = function(indent = "") {
      name <- self$fct_name
      indent0 <- indent

      if (length(self$vars_types_list) >= 1L) {
        args <- lapply(self$vars_types_list, \(x) x$signature()) |> unlist() |> c()
        args <- args[args != ""]
      } else {
        args <- ""
      }
      declarations <- lapply(self$vars_types_list, \(x) {
        res <- x$declare(indent = "")
        paste0(indent, "     ", res)
      }) |> unlist() |> c()
      declarations <- declarations[declarations != ""]
      declarations <- paste0(declarations, collapse = "\n")

      ret_type <- if (inherits(self$return_type, "new_type_node")) {
        self$return_type$stringify("")
      } else {
        self$return_type$data_struct$stringify("")
      }

      body <- ""
      if (inherits(self$AST, "block_node")) {
        body <- self$AST$stringify(indent = paste0(indent, "    "))
      }
      body <- paste0(body, collapse = "")
      args <- paste0(args, collapse = ", ")

      bracket_args <- paste0("&", self$known_fcts, collapse = ", ")
      if (bracket_args == "&") bracket_args <- ""

      paste0(
        paste0("[", bracket_args, "]( ", args, " ) -> ", ret_type, " {\n"),
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
      combine_strings(e, "\n")
    },

    stringify_error_line = function(indent = "") {
      header <- ""
      name <- self$fct_name
      args <- character()
      if (!is.null(self$args_f) && inherits(self$args_f, "list")) {
        normal_vars <- self$vars_types_list[sapply(self$vars_types_list, \(x) inherits(x, c("pre_type_node", "new_type_node")))]
        args <- lapply(normal_vars, function(x) {
          x$signature()
        }) |> unlist() |> c()
        args <- args[args != ""]
      }
      ret_type <- if (inherits(self$return_type, "new_type_node")) {
        self$return_type$stringify("")
      } else {
        self$return_type$data_struct$stringify("")
      }
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

# =====================================================================
# Type nodes
# =====================================================================
pre_type_node <- R6::R6Class(
  "pre_type_node",
  public = list(
    name = NULL,
    data_struct = NULL,
    const_or_mut = "mutable",
    copy_or_ref = "copy",
    r_fct = NULL,
    real_type = NULL,
    iterator = NULL,
    in_scope = TRUE,
    type_decl = NULL,
    fct_input = NULL,
    error = NULL,
    initialize = function(iterator, type_decl, fct_input, error) {
      self$iterator <- iterator
      self$type_decl <- type_decl
      self$fct_input <- fct_input
      self$error <- error
    },
    print = function() {
      cat(paste0(self$name, ": "))
      print(self$data_struct)
    },
    stringify = function(indent = "") {
      paste0(indent, self$data_struct$stringify(indent), " ", self$name, ";")
    },
    get_name = function() {
      self$name
    },
    get_base_type = function() {
      self$data_struct$get_base_type()
    },
    get_data_struct = function() {
      self$data_struct$get_data_struct()
    },
    get_data_struct_verbose = function() {
      self$data_struct$get_data_struct_verbose()
    },
    get_const_or_mut = function() {
      self$const_or_mut
    },
    get_copy_or_ref = function() {
      self$copy_or_ref
    },
    get_iterator = function() {
      self$iterator
    },
    get_in_scope = function() {
      self$in_scope
    },
    get_type_decl = function() {
      self$type_decl
    },
    get_fct_input = function() {
      self$fct_input
    },
    get_error = function() {
      self$error
    },
    stringify_error = function(indent = "") {
      paste0(indent, self$error)
    },
    describe = function(indent = "") {
      if (self$get_data_struct() == "scalar") {
        return(paste0(indent, self$get_base_type()))
      }
      paste0(indent, self$get_data_struct(), "(", self$get_base_type(), ")")
    },
    set_name = function(name) {
      self$name <- name
    },
    set_const_or_mut = function(const_or_mut) {
      self$const_or_mut <- const_or_mut
    },
    set_copy_or_ref = function(copy_or_ref) {
      self$copy_or_ref <- copy_or_ref
    },
    set_iterator = function(iterator) {
      self$iterator <- iterator
    },
    set_in_scope = function(in_scope) {
      self$in_scope <- in_scope
    },
    set_fct_input = function(fct_input) {
      self$fct_input <- fct_input
    },
    set_data_struct = function(data_struct) {
      base_type <- NULL
      if (!is.null(self$data_struct)) {
        base_type <- self$data_struct$get_base_type()
      }
      ds <- switch(data_struct,
        scalar = scalar$new(),
        vector = vec$new(),
        matrix = mat$new(),
        array = arr$new()
      )
      ds$base_type <- base_type
      ds$r_fct <- self$r_fct
      ds$real_type <- self$real_type
      self$data_struct <- ds
    },
    set_base_type = function(base_type) {
      self$data_struct$base_type <- base_type
    },
    flatten = function() {
      if (inherits(self$data_struct$base_type, "pre_type_node")) {
        self$set_base_type(self$data_struct$base_type$get_base_type())
        self$flatten()
      }
      if (inherits(self$data_struct, "pre_type_node")) {
        self$data_struct <- self$data_struct$data_struct
        self$flatten()
      }
      invisible(self)
    },
    declare = function(indent = "") {
      if (self$iterator) return("")
      if (!self$r_fct && self$fct_input) return("")
      if (self$r_fct && self$fct_input) {
        res <- paste0(self$data_struct$stringify(indent), " ", self$name)
        cast_fct <- self$data_struct$cast_fct()
        if (is.null(cast_fct)) {
          return(paste0(indent, res, " = ", self$name, "SEXP;"))
        }
        return(paste0(indent, res, " = ", cast_fct, "(", self$name, "SEXP);"))
      }
      paste0(indent, self$data_struct$stringify(indent), " ", self$name, ";")
    },
    signature = function(indent = "") {
      if (!self$fct_input) return("")
      if (self$r_fct) {
        return(sprintf("SEXP %sSEXP", self$name))
      }
      cm <- "const "
      if (self$const_or_mut != "const") cm <- ""
      cr <- "&"
      if (self$copy_or_ref == "copy") cr <- ""
      paste0(cm, self$data_struct$stringify(indent), cr, " ", self$name)
    }
  )
)

scalar <- R6::R6Class(
  "scalar",
  public = list(
    base_type = NULL,
    r_fct = NULL,
    real_type = NULL,
    print = function() {
      cat(sprintf("scalar value of: %s", self$base_type), "\n")
    },
    stringify = function(indent = "") {
      basic <- is_base_type(self$get_base_type())
      if (basic) {
        res <- convert_base_type(self$base_type, self$real_type)
      } else {
        res <- self$base_type
      }
      res
    },
    cast_fct = function() {
      paste0("etr::SEXP2Scalar<", convert_base_type(self$base_type, self$real_type), ">")
    },
    get_base_type = function() {
      self$base_type
    },
    get_data_struct = function() {
      "scalar"
    },
    get_data_struct_verbose = function() {
      "scalar"
    }
  )
)
unresolved_node <- R6::R6Class(
  "unresolved_node",
  public = list(
    name = NULL,
    base_type = NULL,
    get_base_type = function() {
      self$base_type
    },
    get_data_struct = function() {
      self$name
    },
    get_data_struct_verbose = function() {
      self$name
    }
  )
)
vec <- R6::R6Class(
  "vec",
  public = list(
    base_type = NULL,
    r_fct = NULL,
    real_type = NULL,
    print = function() {
      cat(sprintf("vector containing: %s", self$base_type), "\n")
    },
    stringify = function(indent = "") {
      bt <- convert_base_type(self$base_type, self$real_type)
      sprintf("etr::Array<%s, etr::Buffer<%s>>", bt, bt)
    },
    cast_fct = function() {
      NULL
    },
    get_base_type = function() {
      self$base_type
    },
    get_data_struct = function() {
      "vector"
    },
    get_data_struct_verbose = function() {
      "vector"
    }
  )
)

mat <- R6::R6Class(
  "mat",
  public = list(
    base_type = NULL,
    r_fct = NULL,
    real_type = NULL,
    print = function() {
      cat(sprintf("matrix containing: %s", self$base_type), "\n")
    },
    stringify = function(indent = "") {
      bt <- convert_base_type(self$base_type, self$real_type)
      sprintf("etr::Array<%s, etr::Buffer<%s>>", bt, bt)
    },
    cast_fct = function() {
      NULL
    },
    get_base_type = function() {
      self$base_type
    },
    get_data_struct = function() {
      "matrix"
    },
    get_data_struct_verbose = function() {
      "matrix"
    }
  )
)
arr <- R6::R6Class(
  "arr",
  public = list(
    base_type = NULL,
    r_fct = NULL,
    real_type = NULL,
    print = function() {
      cat(sprintf("array containing: %s", self$base_type), "\n")
    },
    stringify = function(indent = "") {
      bt <- convert_base_type(self$base_type, self$real_type)
      sprintf("etr::Array<%s, etr::Buffer<%s>>", bt, bt)
    },
    cast_fct = function() {
      NULL
    },
    get_base_type = function() {
      self$base_type
    },
    get_data_struct = function() {
      "array"
    },
    get_data_struct_verbose = function() {
      "array"
    }
  )
)
borrow_vec <- R6::R6Class(
  "borrow_vec",
  public = list(
    base_type = NULL,
    r_fct = NULL,
    real_type = NULL,
    print = function() {
      cat(sprintf("vector (borrowed) containing: %s", self$base_type), "\n")
    },
    stringify = function(indent = "") {
      bt <- convert_base_type(self$base_type, self$real_type)
      sprintf("etr::Array<%s, etr::Borrow<%s>>", bt, bt)
    },
    cast_fct = function() {
      NULL
    },
    get_base_type = function() {
      self$base_type
    },
    get_data_struct = function() {
      "vector"
    },
    get_data_struct_verbose = function() {
      "borrow_vector"
    }
  )
)

borrow_mat <- R6::R6Class(
  "borrow_mat",
  public = list(
    base_type = NULL,
    r_fct = NULL,
    real_type = NULL,
    print = function() {
      cat(sprintf("matrix (borrowed) containing: %s", self$base_type), "\n")
    },
    stringify = function(indent = "") {
      bt <- convert_base_type(self$base_type, self$real_type)
      sprintf("etr::Array<%s, etr::Borrow<%s>>", bt, bt)
    },
    cast_fct = function() {
      NULL
    },
    get_base_type = function() {
      self$base_type
    },
    get_data_struct = function() {
      "matrix"
    },
    get_data_struct_verbose = function() {
      "borrow_matrix"
    }
  )
)
borrow_arr <- R6::R6Class(
  "borrow_arr",
  public = list(
    base_type = NULL,
    r_fct = NULL,
    real_type = NULL,
    print = function() {
      cat(sprintf("array (borrowed) containing: %s", self$base_type), "\n")
    },
    stringify = function(indent = "") {
      bt <- convert_base_type(self$base_type, self$real_type)
      sprintf("etr::Array<%s, etr::Borrow<%s>>", bt, bt)
    },
    cast_fct = function() {
      NULL
    },
    get_base_type = function() {
      self$base_type
    },
    get_data_struct = function() {
      "array"
    },
    get_data_struct_verbose = function() {
      "borrow_array"
    }
  )
)

new_type_node <- R6::R6Class(
  "new_type_node",
  public = list(
    name = NULL,
    slots = NULL,
    r_fct = NULL,
    real_type = NULL,
    var_name = NULL,
    type_decl = NULL,
    fct_input = NULL,
    iterator = NULL,
    in_scope = TRUE,
    error = NULL,
    const_or_mut = "mutable",
    copy_or_ref = "copy",
    # TRUE for builtin types whose struct already lives in namespace etr
    # (hardcoded in a header, e.g. uniroot_result) rather than being
    # codegen'd into the translated .cpp's own anonymous namespace --
    # references to the type name need the etr:: qualifier.
    part_of_etr = FALSE,
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

    create_constructor_SEXP = function() {
      n_slots <- length(self$slots)
      input <- vapply(seq_len(n_slots), function(i) {
        s <- self$slots[[i]]
        basic <- is_base_type(s$get_base_type())
        elt <- sprintf("etr::checked_elt(arg, \"%s\", %s, \"%s\")", s$get_name(), n_slots, self$name)
        if (inherits(s, "pre_type_node")) {
          res <- elt
          if (s$get_data_struct() == "scalar" && basic) {
            res <- sprintf("etr::SEXP2Scalar<%s>(%s)", convert_base_type(s$get_base_type(), self$real_type), res)
          }
          res
        } else if (inherits(s, "new_type_node")) {
          sprintf("%s(%s)", s$name, elt)
        } else {
          # Handle error
        }
      }, character(1L))
      names <- vapply(self$slots, function(s) s$get_name(), character(1L))
      input <- paste0(names, "(", input, ")", collapse = ", ")
      sprintf("\texplicit %s(SEXP arg) :\n\t %s {}\n", self$name, input)
    },
    create_cast_to_SEXP = function() {
      n_slots <- length(self$slots)
      field_names <- vapply(self$slots, function(s) s$get_name(), character(1L))
      value_lines <- vapply(seq_len(n_slots), function(i) {
        s <- self$slots[[i]]
        field <- field_names[i]
        # Collection<T>, like a new_type_node, provides its own to_SEXP()
        # and has no etr::Cast() overload -- everything else routes through Cast().
        if (inherits(s, "new_type_node") || (inherits(s, "pre_type_node") && s$get_data_struct() == "collection")) {
          sprintf("\t\tSET_VECTOR_ELT(res, %s, %s.to_SEXP());", i - 1, field)
        } else {
          sprintf("\t\tSET_VECTOR_ELT(res, %s, etr::Cast(%s));", i - 1, field)
        }
      }, character(1L))
      name_lines <- vapply(seq_len(n_slots), function(i) {
        sprintf("\t\tSET_STRING_ELT(names, %s, Rf_mkChar(\"%s\"));", i - 1, field_names[i])
      }, character(1L))
      paste0(
        "\tSEXP to_SEXP() const {\n",
        "\t\tSEXP res = PROTECT(Rf_allocVector(VECSXP, ", n_slots, "));\n",
        paste0(value_lines, collapse = "\n"), "\n",
        "\t\tSEXP names = PROTECT(Rf_allocVector(STRSXP, ", n_slots, "));\n",
        paste0(name_lines, collapse = "\n"), "\n",
        "\t\tRf_setAttrib(res, R_NamesSymbol, names);\n",
        "\t\tRf_setAttrib(res, R_ClassSymbol, Rf_mkString(\"", self$name, "\"));\n",
        "\t\tUNPROTECT(2);\n",
        "\t\treturn res;\n",
        "\t}\n"
      )
    },
    create_special_member_functions = function() {
      n <- self$name
      paste0(
        "\t", n, "() = default;\n",
        "\t", n, "(const ", n, "&) = default;\n",
        "\t", n, "(", n, "&&) noexcept = default;\n",
        "\t", n, "& operator=(const ", n, "&) = default;\n",
        "\t", n, "& operator=(", n, "&&) noexcept = default;\n"
      )
    },
    define_type = function() {
      ctr <- self$create_constructor_SEXP()
      cast <- self$create_cast_to_SEXP()
      special <- self$create_special_member_functions()
      ts <- lapply(self$slots, function(s) {
        if (inherits(s, "new_type_node")) {
          paste0("\t", s$stringify(""), " ", s$get_name(), ";")
        } else {
          s$stringify("\t")
        }
      })
      paste0(
        "struct ", self$name, "{\n",
        paste0(ts, collapse = "\n"),
        "\n",
        ctr,
        "\n",
        cast,
        "\n",
        special,
        "\n",
        "};"
      )
    },
    define_print = function() {
      field_names <- vapply(self$slots, function(s) s$get_name(), character(1L))
      lines <- vapply(seq_along(self$slots), function(i) {
        s <- self$slots[[i]]
        field <- field_names[i]
        if (inherits(s, "new_type_node")) {
          sprintf("\t\tPRINT_STREAM << \"  %s:\" << std::endl;\n\t\tprint(x.%s);", field, field)
        } else if (s$get_data_struct() == "scalar") {
          sprintf("\t\tPRINT_STREAM << \"  %s: \" << x.%s << std::endl;", field, field)
        } else {
          sprintf("\t\tPRINT_STREAM << \"  %s:\" << std::endl;\n\t\tetr::print(x.%s);", field, field)
        }
      }, character(1L))
      paste0(
        "inline void print(const ", self$name, "& x) {\n",
        "\tPRINT_STREAM << \"", self$name, "\" << std::endl;\n",
        paste0(lines, collapse = "\n"), "\n",
        "}"
      )
    },
    stringify = function(indent = "") {
      if (isTRUE(self$part_of_etr)) {
        return(paste0("etr::", self$name))
      }
      self$name
    },
    cast_fct = function() {
      self$stringify()
    },
    get_base_type = function() {
      sprintf("Class %s does not possess a base type", self$name)
    },
    get_data_struct = function() {
      self$name
    },
    get_data_struct_verbose = function() {
      self$name
    },
    get_const_or_mut = function() {
      self$const_or_mut
    },
    get_copy_or_ref = function() {
      self$copy_or_ref
    },
    get_name = function() {
      self$var_name
    },
    set_name = function(name) {
      self$var_name <- name
    },
    get_type_decl = function() {
      self$type_decl
    },
    get_fct_input = function() {
      self$fct_input
    },
    get_iterator = function() {
      self$iterator
    },
    set_iterator = function(iterator) {
      self$iterator <- iterator
    },
    get_in_scope = function() {
      self$in_scope
    },
    set_in_scope = function(in_scope) {
      self$in_scope <- in_scope
    },
    get_error = function() {
      self$error
    },
    stringify_error = function(indent = "") {
      paste0(indent, self$error)
    },
    declare = function(indent = "") {
      if (self$iterator) return("")
      if (!self$r_fct && self$fct_input) return("")
      if (self$r_fct && self$fct_input) {
        res <- paste0(self$stringify(indent), " ", self$var_name)
        cast_fct <- self$cast_fct()
        if (is.null(cast_fct)) {
          return(paste0(indent, res, " = ", self$var_name, "SEXP;"))
        }
        return(paste0(indent, res, " = ", cast_fct, "(", self$var_name, "SEXP);"))
      }
      paste0(indent, self$stringify(indent), " ", self$var_name, ";")
    },
    signature = function(indent = "") {
      if (!self$fct_input) return("")
      if (self$r_fct) {
        return(sprintf("SEXP %sSEXP", self$var_name))
      }
      cm <- "const "
      if (self$get_const_or_mut() != "const") cm <- ""
      cr <- "&"
      if (self$get_copy_or_ref() == "copy") cr <- ""
      paste0(cm, self$stringify(indent), cr, " ", self$var_name)
    }
  )
)

collection <- R6::R6Class(
  "collection",
  public = list(
    type = NULL,
    element_type = NULL,
    r_fct = NULL,
    real_type = NULL,
    print = function() {
      cat(sprintf("collection containing: %s", self$type), "\n")
    },
    stringify = function(indent = "") {
      basic <- is_base_type(self$type)
      if (basic) {
        stop("collections cannot contain basic types")
      }
      sprintf("etr::Collection<%s>", self$type)
    },
    cast_fct = function() {
      self$stringify()
    },
    get_base_type = function() {
      sprintf("Class collection of %s does not possess a base type", self$type)
    },
    get_data_struct = function() {
      "collection"
    },
    get_data_struct_verbose = function() {
      "collection"
    }
  )
)
