setClass(
  "String",
  representation(value = "character"),
  prototype(value = character())
)

setMethod("initialize", signature(.Object = "String"), function(.Object, value) {
  .Object@value <- value
  .Object
})

setMethod("+", c("String","String"), function(e1,e2) {
   newValue <- paste0(e1@value, e2@value)
   new("String", value = newValue)
})

setMethod("+", c("character","String"), function(e1,e2) {
   newValue <- paste0(e1, e2@value)
   new("String", value = newValue)
})

setMethod("+", c("String","character"), function(e1,e2) {
   newValue <- paste0(e1@value, e2)
   new("String", value = newValue)
})

cString <- function(...) {
  l <- list(...)
  delim = tail(l, n = 1)[[1]]
  values <- sapply(l[1:(length(l)-1)], function(x) {
    if(is.character(x)) {
      return(x)
    } else if(is(x, "String")) {
      return(x@value)
    } else {
      stop("Error can only handle class String and character")
    }
  })
  new_value <- paste0(values, collapse = delim)
  new("String", value = new_value)
}

String <- function(value) {
  new("String", value = value)
}

MA <- R6::R6Class("MA",
  public = list(
    R_fct = NULL,
    name = NULL,
    args = NULL,
    args_2_fct = NULL,
    body = NULL,
    desired_type = NULL,
    temp = list(),
    ast = list(),
    code = list(),
    char = list(),
    var_all = c(),
    var_types = c(),
    var_index = c(),
    return_TF = c(),
    return_valtype = c(),
    initialize = function(fct, desired_type, name_f, R_fct, return_valtype) {
      self$name <- name_f # as.character(substitute(fct))
      self$args <- methods::formalArgs(fct)
      self$body <- body(fct)
      self$body <- self$body[2:length(self$body)]
      self$desired_type <- desired_type
      self$args_2_fct <- methods::formalArgs(fct) # as.list(args(fct))
      self$R_fct <- R_fct
      self$return_valtype <- return_valtype
    },
    getast = function() {
      for (i in seq_along(self$body)) {
        # convert ast R --> ETR
        self$temp[[i]] <- LC$new(self$body[[i]], self$R_fct)
        self$ast[[i]] <- self$temp[[i]]$ast
        # get vars
        self$var_all <- c(self$var_all, self$temp[[i]]$vars)
        self$var_index <- c(self$var_index, self$temp[[i]]$index_vars)

        # return was found?
        self$return_TF <- c(self$return_TF, self$temp[[i]]$found_return)
        if (any(self$return_TF == TRUE)) self$return_TF <- TRUE else self$return_TF <- FALSE
      }
    },
    get_calls = function(code) {
      out <- purrr::map_if(code, is.list, self$get_calls)
      out <- as.call(out)
      return(out)
    },
    ast2call = function() {
      for (i in seq_along(self$ast)) {
        self$code[[i]] <- self$get_calls(self$ast[[i]])
      }
    },
    call2char = function() {
      j <- 1
      for (i in seq_along(self$code)) {
        temp <- deparse(self$code[[i]], width.cutoff = 500)
        temp <- gsub("\\bin\\b", "", temp)
        temp <- gsub("`", "", temp)
        temp <- paste0(temp, ";")

        # loop over temp
        temp2 <- character(length(temp) * 2)
        counter <- 1
        for (i in seq_along(temp)) {
          temp2[counter] <- temp[i]
          counter <- counter + 1
          temp2[counter] <- "\n"
          counter <- counter + 1
        }
        temp <- temp2

        self$char[[j]] <- temp
        j <- j + 1
        #self$char[[j]] <- "\n"
        #j <- j + 1
      }
    },
    get_vars = function() {
      temp1 <- unique(self$var_all)
      temp2 <- unique(self$var_index)

      # remove index variables
      # necessary in order to remove i and auto$i:
      temp3 <- list()
      for (i in seq_along(temp2)) {
        tm <- gsub("auto&", "", temp2[[i]])

        tm <- gsub(":", "", tm)
        temp3[[i]] <- tm
      }

      temp2 <- c(temp2, unlist(temp3))

      for (i in seq_along(temp1)) {
        for (j in seq_along(temp2)) {
          if (temp1[[i]] == temp2[[j]]) {
            temp1[[i]] <- NA
            break
          }
        }
      }

      temp1 <- temp1[!is.na(temp1)]

      # remove arguments passed to fct
      for (i in seq_along(temp1)) {
        for (j in seq_along(self$args)) {
          if (temp1[[i]] == self$args[[j]]) {
            temp1[[i]] <- NA
            break
          }
        }
      }

      temp1 <- temp1[!is.na(temp1)]

      # remove true and false
      for (i in seq_along(temp1)) {
        if (temp1[[i]] == as.name("true") || temp1[[i]] == as.name("false")) {
          temp1[[i]] <- NA
        }
      }

      ret <- temp1[!is.na(temp1)]

      self$var_types <- vector("list", length = length(ret))
      self$var_types <- lapply(self$var_types, function(x) {
        return("sexp")
      })

      # check for doubles
      for (i in seq_along(ret)) {
        nm <- as.character(ret[[i]])
        nm <- unlist(strsplit(nm, ""))
        if (length(nm) >= 4) {
          if ((nm[[length(nm)]] == "b") && (nm[[length(nm) - 1]] == "d") &&
            (nm[[length(nm) - 2]] == "_")) {
            self$var_types[[i]] <- "double"
          }
        }
      }

      return(ret)
    },
    signature_own = function(desired_type, reference, extern = FALSE, typedef = FALSE) {
      # arguments passed to f & define signature

      arguments_string <- character(length(self$args))

      for (i in seq_along(self$args)) {
        y <- paste0(self$args[[i]])
        type <- NULL
        type_name <- NULL
        temp <- NULL
        if (reference) {
          type <- paste0(desired_type[i], "&")
          type_name <- desired_type[i]
        } else {
          type <- desired_type[i]
          type_name <- desired_type[i]
        }

        if (desired_type[i] == "ptr_vec") {
          if (reference == TRUE) {
            stop("Reference cannot be set to TRUE in combination with ptr_vec or ptr_mat")
          }

          temp <- paste("double*", paste0(y, "_double_ptr"), ",",
            "int", paste0(y, "_int_size"), ",",
            collapse = ""
          )
          if (i == length(self$args)) {
            temp <- paste("double*", paste0(y, "_double_ptr"), ",",
              "int", paste0(y, "_int_size"),
              collapse = ""
            )
          }
        } else if (desired_type[i] == "ptr_mat") {
          if (reference == TRUE) {
            stop("Reference cannot be set to TRUE in combination with ptr_vec or ptr_mat")
          }
          temp <- paste("double*", paste0(y, "_double_ptr"), ",",
            "int", paste0(y, "_int_rows"), ",",
            "int", paste0(y, "_int_cols"), ",",
            collapse = ""
          )
          if (i == length(self$args)) {
            temp <- paste("double*", paste0(y, "_double_ptr"), ",",
              "int", paste0(y, "_int_rows"), ",",
              "int", paste0(y, "_int_cols"),
              collapse = ""
            )
          }
        } else if (self$R_fct == TRUE) { # not ptr_vec or ptr_mat
          temp <- paste(type, paste0(y, type_name), ",", collapse = "")

          if (i == length(self$args)) {
            temp <- paste(type, paste0(y, type_name), collapse = "")
          }
        } else if (self$R_fct == FALSE) { # XPtr interface
          temp <- paste(type, y, ",", collapse = "")

          if (i == length(self$args)) {
            temp <- paste(type, y, collapse = "")
          }
        }

        arguments_string[i] <- temp
      }

      arguments_string <- paste(arguments_string, collapse = " ")

      sig <- NULL
      if (extern == FALSE) {
        sig <- paste(self$return_valtype, self$name, "(", arguments_string, ")", "{", collapse = " ")
      } else {
        sig <- paste('extern "C" {', self$return_valtype, self$name, "(", arguments_string, ");}", collapse = " ")
      }

      if (typedef == TRUE) {
        sig <- paste(self$return_valtype, "(*fct_ptr)", "(", arguments_string, ");", collapse = " ")
        return(sig)
      }

      return(sig)
    },
    vars_declaration_own = function(desired_type) {
      variables <- self$get_vars()

      args_dec <- sapply(seq_along(variables), function(x) {
        temp <- paste(gsub(" ", "", self$var_types[[x]]),
          paste0(variables[[x]], ";", collapse = ""),
          collapse = ""
        )
        return(temp)
      })
      args_dec <- paste(args_dec, collapse = "")
      args_dec <- paste("\n", args_dec, "\n")

      fct_args <- self$args_2_fct

      fct_args_ptr <- sapply(seq_along(fct_args), function(x) {
        if (desired_type[[x]] == "ptr_vec") {
          return(x)
        } else if (desired_type[[x]] == "ptr_mat") {
          return(x)
        }
        return(NA)
      })
      fct_args_ptr <- fct_args_ptr[!is.na(fct_args_ptr)]

      fct_args_dec <- sapply(seq_along(fct_args), function(x) {
        temp <- NULL
        if (desired_type[[x]] == "ptr_vec") {
          temp <- paste("sexp", fct_args[[x]], "(",
            paste0(fct_args[[x]], "_int_size,"),
            paste0(fct_args[[x]], "_double_ptr,"),
            "2);",
            collapse = ""
          ) # borrow
        } else if (desired_type[[x]] == "ptr_mat") {
          temp <- paste("sexp", fct_args[[x]], "(",
            paste0(fct_args[[x]], "_int_rows,"), paste0(fct_args[[x]], "_int_cols,"),
            paste0(fct_args[[x]], "_double_ptr,"),
            "2);",
            collapse = ""
          ) # borrow
        } else {
          temp <- paste("WrapperSEXP", fct_args[[x]], ";")
          temp <- c(temp, fct_args[[x]], "=", paste0(self$args[[x]], desired_type[x], "; ", "\n", collapse = ""))
        }
        return(temp)
      })

      if (self$R_fct == TRUE) {
        args_dec <- c(args_dec, fct_args_dec)
      } else if (length(fct_args_ptr) >= 1) {
        args_dec <- c(args_dec, fct_args_dec[fct_args_ptr])
      }

      return(args_dec)
    },
    build_own = function(verbose = FALSE, reference = FALSE) {
      self$getast()
      self$ast2call()
      self$call2char()

      fct <- c(
        "// [[Rcpp::depends(ast2ast)]] \n",
        "// [[Rcpp::depends(RcppArmadillo)]] \n",
        "// [[Rcpp::plugins(cpp20)]] \n",
        '#include "etr.hpp"', "\n",
        "// [[Rcpp::export]]", "\n",
        "SEXP getXPtr();", "\n",
        self$signature_own(self$desired_type, reference, extern = FALSE),
        self$vars_declaration_own(self$desired_type),
        self$char,
        "}\n",
        "SEXP getXPtr() {", "\n",
        paste("typedef", self$signature_own(self$desired_type, reference, extern = FALSE, typedef = TRUE), ";\n"),
        paste("return Rcpp::XPtr<fct_ptr>(new fct_ptr(& ", self$name, "));\n"),
        "}\n"
      )

      fct <- paste(unlist(fct), collapse = "")
    },

    # build with own types
    build_own_SEXP = function(verbose = FALSE, reference = FALSE) {
      self$getast()
      self$ast2call()
      self$call2char()
      fct <- NULL

      if (self$return_TF == FALSE) {
        fct <- c(
          "// [[Rcpp::depends(ast2ast)]] \n",
          "// [[Rcpp::depends(RcppArmadillo)]] \n",
          "// [[Rcpp::plugins(cpp20)]] \n",
          '#include "etr.hpp"', "\n",
          "// [[Rcpp::export]] \n",
          self$signature_own(self$desired_type, reference, extern = FALSE), "\n",
          self$vars_declaration_own(self$desired_type), "\n",
          self$char, "\n",
          "return R_NilValue;\n",
          "}\n"
        )
      } else {
        fct <- c(
          "// [[Rcpp::depends(ast2ast)]] \n",
          "// [[Rcpp::depends(RcppArmadillo)]] \n",
          "// [[Rcpp::plugins(cpp20)]] \n",
          '#include "etr.hpp"', "\n",
          "// [[Rcpp::export]] \n",
          self$signature_own(self$desired_type, reference, extern = FALSE), "\n",
          self$vars_declaration_own(self$desired_type), "\n",
          self$char, "\n",
          "}\n"
        )
      }

      fct <- paste(unlist(fct), collapse = "")
    }
  )
)


astClass <- R6::R6Class("astClass",
  public = list(
    args = NULL,
    args_2_fct = NULL,
    body = NULL,
    ast = list(),
    code = list(),
    char = list(),
    var_all = c(),
    var_types = c(),
    var_index = c(),
    return_TF = c(),
    R_fct = NULL,

    initialize = function(fct, name_f, R_fct) {
      self$args <- methods::formalArgs(fct)
      self$args_2_fct <- methods::formalArgs(fct) 
      self$body <- body(fct)[2:length(body(fct))]
      self$R_fct <- R_fct
    },

    getast = function() {
      for (i in seq_along(self$body)) {
        temp <- LC$new(self$body[[i]], self$R_fct)
        self$ast[[i]] <- temp$ast
        self$var_all <- c(self$var_all, temp$vars)
        self$var_index <- c(self$var_index, temp$index_vars)
        self$return_TF <- c(self$return_TF, temp$found_return)
        if (any(self$return_TF == TRUE)) self$return_TF <- TRUE else self$return_TF <- FALSE
      }
    },

    get_calls = function(code) {
      out <- purrr::map_if(code, is.list, self$get_calls)
      out <- as.call(out)
      return(out)
    },

    ast2call = function() {
      for (i in seq_along(self$ast)) {
        self$code[[i]] <- self$get_calls(self$ast[[i]])
      }
    },

    call2char = function() {
      j <- 1
      for (i in seq_along(self$code)) {
        temp <- deparse(self$code[[i]], width.cutoff = 500)
        temp <- gsub("\\bin\\b", "", temp)
        temp <- gsub("`", "", temp)
        temp <- paste0(temp, ";")
        temp2 <- character(length(temp) * 2)
        counter <- 1
        for (i in seq_along(temp)) {
          temp2[counter] <- temp[i]
          counter <- counter + 1
          temp2[counter] <- "\n"
          counter <- counter + 1
        }
        temp <- temp2
        self$char[[j]] <- temp
        j <- j + 1
      }
    },

    get_vars = function() {
      temp1 <- unique(self$var_all)
      temp2 <- unique(self$var_index)
      # remove index variables
      # necessary in order to remove i and auto$i:
      temp3 <- list()
      for (i in seq_along(temp2)) {
        tm <- gsub("auto&", "", temp2[[i]])
        tm <- gsub(":", "", tm)
        temp3[[i]] <- tm
      }
      temp2 <- c(temp2, unlist(temp3))
      for (i in seq_along(temp1)) {
        for (j in seq_along(temp2)) {
          if (temp1[[i]] == temp2[[j]]) {
            temp1[[i]] <- NA
            break
          }
        }
      }
      temp1 <- temp1[!is.na(temp1)]
      # remove arguments passed to fct
      for (i in seq_along(temp1)) {
        for (j in seq_along(self$args)) {
          if (temp1[[i]] == self$args[[j]]) {
            temp1[[i]] <- NA
            break
          }
        }
      }
      temp1 <- temp1[!is.na(temp1)]
      # remove true and false
      for (i in seq_along(temp1)) {
        if (temp1[[i]] == as.name("true") || temp1[[i]] == as.name("false")) {
          temp1[[i]] <- NA
        }
      }
      ret <- temp1[!is.na(temp1)]
      self$var_types <- vector("list", length = length(ret))
      self$var_types <- lapply(self$var_types, function(x) {
        return("sexp")
      })
      # check for doubles
      for (i in seq_along(ret)) {
        nm <- as.character(ret[[i]])
        nm <- unlist(strsplit(nm, ""))
        if (length(nm) >= 4) {
          if ((nm[[length(nm)]] == "b") && (nm[[length(nm) - 1]] == "d") &&
            (nm[[length(nm) - 2]] == "_")) {
            self$var_types[[i]] <- "double"
          }
        }
      }
      return(ret)
    }
    
  )
)

buildBody <- function(bodyContent) {
  b <- lapply(bodyContent, function(x) {
    paste("\t", x)
  })
  paste(unlist(b), collapse = "")
}

generateNewName <- function(name, extension, delimiter, vars) {
  newName <- cString(name, extension, delimiter)
  i <- 0
  repeat {
    if(i == 10) {
      stop("Cannot generate a new name for the double pointer")
    }
    if(!(newName@value %in% vars)) {
      break
    } else {
      newName <- cString(newName@value, extension, delimiter)
      i <- i + 1
    }
  }
  return(newName)
}

varsDeclaration <- function(vars, types) {
  stopifnot(length(vars) == length(types))
  l <- list()
  for(i in seq_along(1:length(vars))) {
    l[[i]] <- cString("\t ", cString(types[[i]], deparse(vars[[i]]), " ")@value, ";\n", "")@value
  }
  l <- do.call(paste, l)
  return(l)
}

ptrVecSig <- function(varName, vars) {
  ptrName <- generateNewName(varName, "_double_ptr", "", vars)
  sizeName <- generateNewName(varName, "_int_size", "", vars)
  res <- cString("etr::BaseType*", ptrName, ",",
                 "int", sizeName, " ")
  return(res@value)
}

ptrMatSig <- function(varName, vars) {
  ptrName <- generateNewName(varName, "_double_ptr", "", vars)
  rowsName <- generateNewName(varName, "_int_rows", "", vars)
  colsName <- generateNewName(varName, "_int_cols", "", vars)
  res <- cString("etr::BaseType*", ptrName, ",",
                 "int ", rowsName, ",", "int ", colsName, " ")
  return(res@value)
}

typeSigXPtr <- function(varName, type, reference, allVars) {
  if(reference && type == "ptr_vec") stop("ptr_vec cannot be used by reference")
  if(reference && type == "ptr_mat") stop("ptr_vec cannot be used by reference")
  if(!reference) {
    if(type == "ptr_vec") {
      newName <- ptrVecSig(varName, allVars)
      return(newName)
    }
    if(type == "ptr_mat") {
      newName <- ptrMatSig(varName, allVars)
      return(newName)
    }
    newName <- cString(type, varName, " ")
    return(newName@value)  
  } else {
    newName <- cString(type, "&", varName, " ")
    return(newName@value)  
  }
}

defineSigListXPtr <- function(arguments, types, nameFct, reference, retType, allVars) {
  stopifnot(length(arguments) == length(types))
  allowedTypes <- c("sexp", "double", "ptr_vec", "ptr_mat", "Borrow")
  if(reference) allowedTypes <- c("sexp", "double", "Borrow") 
  lapply(types, function(x) {stopifnot(x %in% allowedTypes)})
  allowedTypes <- c("sexp", "double", "Borrow", "void")
  stopifnot(retType %in% allowedTypes)
  typeList <- ""
  ptrNames <- list()
  counter <- 1
  for(i in seq_along(1:length(arguments))) {
    currentName <- typeSigXPtr(arguments[[i]], types[[i]], reference, allVars)
    if(types[[i]] == "ptr_vec") {
      splitName <- strsplit(currentName, split = " ")[[1]]
      ptrNames[[counter]] <- list(arguments[[i]], splitName[[2]], splitName[[5]])
      counter <- counter + 1
    } else if(types[[i]] == "ptr_mat") {
      splitName <- strsplit(currentName, split = " ")[[1]]
      ptrNames[[counter]] <- list(arguments[[i]], splitName[[2]], splitName[[6]], splitName[[10]])
      counter <- counter + 1
    }
    if(i == length(arguments)) {
      typeList <- paste0(typeList, currentName)
    } else {
      typeList <- paste0(typeList, currentName, ", ")
    }
  }
  return(list(typeList, ptrNames))
}

signatureXPtr <- function(typeList, retType, nameFct) {
  ret <- cString(retType, " ", nameFct, "(", typeList, ") {\n", "")
  return(ret@value)
} 

getXPtr <- function(typeList, retType, nameFct) {
  name <- generateNewName("get", "XPtr", "", nameFct)
  fLine <- cString("SEXP", name, "()", " {", " ")
  sLine <- cString("\t", "typedef ", retType, " (*fct_ptr) (", typeList, ");", "")
  tLine <- cString("\t", "return Rcpp::XPtr<fct_ptr>(new fct_ptr(&", nameFct, "));", "")
  foLine <- cString("}", "")
  res <- cString(fLine, sLine, tLine, foLine, "\n")
  preDeclaration <- cString("SEXP ", name, "();", "")
  return(list(name = preDeclaration, fct = res@value))
}

handlePtr <- function(ptrList, borrow) {
  ls <- lapply(ptrList, function(x) x[[1]])
  rs <- lapply(ptrList, function(x) x[2:length(x)])
  stopifnot(length(ls) == length(rs))
  res <- ""
  for(i in seq_along(1:length(ls))) {
    arg <- paste(unlist(rs[[i]]), collapse = ",")
    if(borrow) {
      temp <- cString("\t Borrow ", ls[[i]], "(",arg, ");", "")@value
      res <- paste(res, temp, "\n")
    } else {
      temp <- cString("\t sexp ", ls[[i]], "(",arg, ");", "")@value
      res <- paste(res, temp, "\n")
    }
  }
  return(res)
}

buildFctXPtr <- function(fct, reference, types, retType, nameFct) {
  bodyFct <- body(fct)[2:length(body(fct))]
  arguments <- methods::formalArgs(fct)
  ac <- astClass$new(fct, nameFct, R_fct = FALSE)
  ac$getast()
  ac$ast2call()
  ac$call2char()
  vars <- ac$get_vars()
  declarations <- varsDeclaration(vars, ac$var_types)
  f <- cString("// [[Rcpp::depends(ast2ast)]]",
        "// [[Rcpp::depends(RcppArmadillo)]]",
        "// [[Rcpp::plugins(cpp20)]]",
        '#include "etr.hpp"\n',
        "// [[Rcpp::export]]", "\n")
  b <- buildBody(ac$char)
  sigList <- defineSigListXPtr(ac$args, types, nameFct, reference, retType, ac$var_all)
  declarationsPtr <- handlePtr(sigList[[2]], reference)
  sig <- signatureXPtr(sigList[[1]], retType, nameFct)
  xptr <- getXPtr(sigList[[1]], retType, nameFct)
  f <- cString(f, xptr$name, "\n", sig, declarations, declarationsPtr, b, "}\n", xptr$fct, "\n")
  return(f@value)
}

typeSigR <- function(varName, type) {
  newName <- cString(type, varName, " ")
  return(newName@value)  
}

signatureR <- function(arguments, nameFct, vars) {
  typeList <- ""
  newNames <- list()
  for(i in seq_along(1:length(arguments))) {
    currentName <- generateNewName(arguments[[i]], "SEXP", "", vars)
    newNames[[i]] <- currentName@value
    if(i == length(arguments)) {
      typeList <- paste0(typeList, typeSigR(currentName, "SEXP"))
    } else {
      typeList <- paste0(typeList, typeSigR(currentName, "SEXP"), ", ")
    }
  }
  ret <- cString("SEXP", " ", nameFct, "(", typeList, ") {\n", "")
  return(list(ret@value, newNames))
}

handleSEXP <- function(ls, rs) {
  stopifnot(length(ls) == length(rs))
  res <- ""
  for(i in seq_along(1:length(ls))) {
    temp <- cString("\t WrapperSEXP ", ls[[i]], "; ",
                   ls[[i]], " = ", rs[[i]], ";", "")@value
    res <- paste(res, temp, "\n")
  }
  return(res)
}

buildFctR <- function(fct, nameFct) {
  bodyFct <- body(fct)[2:length(body(fct))]
  arguments <- methods::formalArgs(fct)
  ac <- astClass$new(fct, nameFct, R_fct = TRUE)
  ac$getast()
  ac$ast2call()
  ac$call2char()
  vars <- ac$get_vars()
  declarations <- varsDeclaration(vars, ac$var_types)
  f <- cString("// [[Rcpp::depends(ast2ast)]]",
        "// [[Rcpp::depends(RcppArmadillo)]]",
        "// [[Rcpp::plugins(cpp20)]]",
        '#include "etr.hpp"\n',
        "// [[Rcpp::export]]\n", "\n")
  sig <- signatureR(ac$args, nameFct, ac$var_all)
  hs <- handleSEXP(ac$args, sig[[2]])
  b <- buildBody(ac$char)
  f <- cString(f, sig[[1]], hs, declarations, b, "}\n", "")
  return(f@value)
}


