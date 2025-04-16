variables <- R6::R6Class(
  "variables",
  public = list(
    names = NULL,
    types = NULL,
    lines = NULL,
    contexts = NULL,
    are_const = NULL,
    are_ref = NULL,
    df = data.frame(),
    errors = list(),
    initialize = function(names = NULL, types = NULL, contexts = NULL) {
      if (is.null(names)) self$names <- c() else self$names <- names
      if (is.null(types)) self$types <- c() else self$types <- types
      if (is.null(contexts)) self$contexts <- c() else self$contexts <- contexts
    },
    add_entry = function(name, line, type, context, const = NA, ref = NA) {
      self$names <- c(self$names, name) |> unique()
      self$lines <- c(self$lines, line) |> unique()
      self$types <- c(self$types, type) |> unique()
      self$contexts <- c(self$contexts, context) |> unique()
      self$are_const <- c(self$are_const, const) |> unique()
      self$are_ref <- c(self$are_ref, ref) |> unique()
    },
    valid_type = function(name, type, line) {
      if (!(type %in% permitted_types())) {
        self$errors[[name]] <- paste0(
          "Invalid type: ", deparse(type), " at line ", line
        )
        return("error")
      }
    },
    check_single_type = function(name, type, line) {
      if (type == "unknown") {
        type <- "double_vector" # Default
      } else {
        self$valid_type(name, type, line)
      }
      return(type)
    },
    one_declaration = function(sub) {
      if (all(sub$type == "unknown")) {
        return("double_vector")
      } else {
        # Check type declaration during first assignment/definition
        if (sub$type[1] == "unknown") {
          first_type <- which(sub$type != "unknown")[1]
          self$errors[[unique(sub$name)]] <- paste0(
            "The first declaration of ", unique(sub$name),
            " is at line ", sub$lines[1],
            " but later a type is specified: ",
            sub$lines[first_type],
            ". \nIf you want to define a type",
            " you should do it at the first declaration."
          )
          return("error")
        }
        # Contracdicting type declarations
        found_types <- which(sub$type != "unknown")
        if (length(found_types) > 1) {
          lines <- sub$lines[found_types] |> combine_strings()
          self$errors[[unique(sub$name)]] <- paste0(
            "The variable ", unique(sub$name),
            " is declared with multiple types: \n",
            lines,
            ". \nA variable can only have one type declaration."
          )
          return("error")
        } else {
          return(sub$type[1])
        }
      }
    },
    iteration_var = function(sub) {
      if (any(sub$contexts == "Iteration")) {
        if (!all(sub$contexts == "Iteration")) {
          iter_line <- which(sub$contexts == "Iteration")[1]
          non_iter_line <- which(sub$contexts != "Iteration")[1]
          error_string <- paste(
            "You cannot assign to index variables",
            sub$lines[iter_line],
            sub$lines[non_iter_line],
            sep = "\n"
          )
          self$errors[[unique(sub$name)]] <- error_string
        }
      }
    },
    check = function() {
      if (is.null(self$names) || identical(self$names, list())) {
        self$df <- "No variables found"
        return()
      }
      res <- data.frame(
        name = unique(self$names) |> unlist(),
        type = NA,
        are_const = NA,
        are_ref = NA,
        function_input = FALSE,
        contexts = NA
      )
      df <- data.frame(
        name = self$names |> unlist(),
        type = self$types |> unlist(),
        lines = self$lines |> unlist(),
        contexts = self$contexts |> unlist(),
        are_const = self$are_const |> unlist(),
        are_ref = self$are_ref |> unlist()
      )
      for (i in unique(df$name)) {
        sub <- df[df$name == i, ]
        self$iteration_var(sub)
        if (nrow(sub) == 1) {
          res[res$name == i, "type"] <-
            self$check_single_type(sub$name, sub$type, sub$lines)
          res[res$name == i, "Iteration"] <- "Iteration" %in% sub$contexts
        } else {
          res[res$name == i, "type"] <-
            self$one_declaration(sub)
          res[res$name == i, "Iteration"] <- "Iteration" %in% sub$contexts
        }
        if ("FunctionInput" %in% sub$contexts) {
          sub_function_input <- sub[sub$contexts == "FunctionInput", ]
          res[res$name == i, "are_const"] <- sub_function_input[sub_function_input$name == i, "are_const"]
          res[res$name == i, "are_ref"] <- sub_function_input[sub_function_input$name == i, "are_ref"]
          res[res$name == i, "function_input"] <- TRUE
          res[res$name == i, "contexts"] <- "function_input"
        }
      }
      self$df <- res[!res$Iteration, ]
      if ((nrow(self$df) == 0) || (ncol(self$df) == 0)) {
        self$df <- "No variables found"
      }
      return(self$errors)
    }
  )
)
