printClass <- R6::R6Class("printClass",
  public = list(
    getAST = function(code) {
      if (!is.call(code)) {
        return(code)
      }
      code <- as.list(code)
      lapply(code, self$getAST)
    },
    getCALLS = function(code) {
      for (i in seq_along(1:length(code))) {
        if (is.list(code[[i]])) {
          code[[i]] <- self$getCALLS(code[[i]])
        }
      }
      code <- as.call(code)
      return(code)
    },
    indentationLevel = 0,
    printAST = function(code) {
      for (i in seq_along(1:length(code))) {
        if (is.list(code[[i]])) {
          self$indentationLevel <- self$indentationLevel + 1
          fct <- deparse(code[[i]][[1]])
          times <- rep("...", self$indentationLevel)
          print(paste(c(times, fct), collapse = ""))
          self$indentationLevel <- self$indentationLevel + 1
          self$printAST(code[[i]][2:length(code[[i]])])
          self$indentationLevel <- self$indentationLevel - 2
        } else {
          obj <- deparse(code[[i]])
          times <- paste(rep("...", self$indentationLevel), collapse = "")
          print(paste(c(times, obj), collapse = " "), collapse = "")
        }
      }
    },
    customAsCall = function(code) {
      if (as.character(code[[1]]) == "diamond") {
        assert(length(code) == 3)
        return(paste0(code[[2]], "<", code[[3]], ">"))
      } else {
        return(as.call(code))
      }
    },
    getSTRING = function(code) {
      for (i in seq_along(1:length(code))) {
        if (is.list(code[[i]])) {
          code[[i]] <- self$getSTRING(code[[i]])
        }
      }
      code <- self$customAsCall(code)
      return(code)
    },
    removeBackslashes = function(str) {
      str <- deparse(str)
      str <- paste("r{", str, "}", collapse = "")
      print(str)
      str <- gsub("\"", "", str)
      return(substr(str, 3, nchar(str) - 1))
    },
    Call2STRING = function(code) {
      code <- self$getSTRING(code)
      return(self$removeBackslashes(code))
    }
  )
)

# pC <- printClass$new()
# ast <- pC$getAST(quote( (a+b) * (a*b*c) ))
# pC$getCALLS(ast)
# pC$printAST(ast)
# ast <- pC$getAST(quote(diamond(a, 12)*b + c*d + diamond(b, 5) * diamond(h, diamond(k, j)) ) )
# code <- pC$Call2STRING(ast)
# code
