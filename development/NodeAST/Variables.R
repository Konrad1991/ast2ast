Variables <- R6::R6Class(
  "Variables",
  public = list(
    names = NULL,
    types = NULL,
    lines = NULL,
    errors = list(),
    initialize = function() {
      self$names <- c()
      self$types <- c()
    },
    valid_type = function(name, type, line) {
      if (!(type %in% permitted_types())) {
        self$errors[[name]] <- paste0(
          "Invalid type: ", deparse(type), " at line ", line
        )
        return("Error")
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
    one_declaraion = function(sub) {
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
          return("Error")
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
          return("Error")
        } else {
          return(sub$type[1])
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
        lines = self$lines
      )
      for (i in unique(df$name)) {
        sub <- df[df$name == i, ]
        if (nrow(sub) == 1) {
          res[res$name == i, "type"] <-
            self$check_single_type(sub$name, sub$type, sub$lines)
        } else {
          res[res$name == i, "type"] <-
            self$one_declaraion(sub)
        }
      }
      return(res)
    }
  )
)
