variables <- R6::R6Class(
  "variables",
  public = list(
    names = NULL,
    types = NULL,
    lines = NULL,
    contexts = NULL,
    errors = list(),
    initialize = function() {
      self$names <- c()
      self$types <- c()
      self$contexts <- c()
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
          self$errors[[unique(sub$name)]] <- paste0(
            "If a variable is used as index variable in a for loop ",
            "it cannot be used somewhere else as 'normal' variable",
            "\nIteration: ", sub$lines[iter_line],
            "Elsewhere: ", sub$lines[non_iter_line]
          )
        }
      }
    },
    check = function() {
      if (is.null(self$names)) {
        return("No variables found")
      }
      res <- data.frame(
        name = unique(self$names),
        type = NA
      )
      df <- data.frame(
        name = self$names,
        type = self$types,
        lines = self$lines,
        contexts = self$contexts
      )
      for (i in unique(df$name)) {
        sub <- df[df$name == i, ]
        if (nrow(sub) == 1) {
          res[res$name == i, "type"] <-
            self$check_single_type(sub$name, sub$type, sub$lines)
        } else {
          res[res$name == i, "type"] <-
            self$one_declaration(sub)
        }
        self$iteration_var(sub)
      }
      return(res)
    }
  )
)
