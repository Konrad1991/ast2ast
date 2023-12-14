
TreeInformation <- R6::R6Class(
  public = list(
    variableList = c(),
    mapName = "",

    initialize = function(variableList, mapName) {
      self$variableList = variableList
      self$mapName = mapName
    },

    isVariable = function(variable) {
      if(c(deparse(variable)) %in% self$variableList) {
        return(match(c(deparse(variable)), self$variableList))
      } else {
        return(-1)
      }
    },

    getAst = function(code) {
      if(!is.call(code)) {
        return(code)
      }

      code <- as.list(code)
      if(code[[1]] != as.name("VarPointer")) {
        for(i in seq_along(1:length(code))) {
          Idx = self$isVariable(code[[i]])
          if(Idx != -1) {
            code[[i]] <- str2lang(paste("VarPointer(", self$mapName, ",", Idx, ");"))
          }
        }  
      }
      

      lapply(code, function(x) {
        self$getAst(x)
      })
    },

    getCalls = function(code) {
      out <- purrr::map_if(code, is.list, self$getCalls)
      out <- as.call(out)
      return(out)
    },

    convert = function(code) {
      modCode = self$getAst(code)
      modCode = self$getCalls(modCode)
      return(modCode)
    }

  )
)
#code <- quote(a*b[1] + b*b*a)
#ti <- TreeInformation$new(c("a", "b"), "Map" )
#ti$convert(code)
