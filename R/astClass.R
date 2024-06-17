astClass <- R6::R6Class("astClass",
  public = list(
    args = NULL,
    args_types = NULL,
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
      self$identify_type_sig()
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
      for (i in seq_along(code)) {
        if (is.list(code[[i]])) {
          code[[i]] <- self$get_calls(code[[i]])
        }
      }
      code <- as.call(code)
      return(code)
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
    identify_type_sig = function() {
      ret <- self$args
      self$args_types <- vector("list", length = length(ret))
      if (!is.null(self$R_fct) && (self$R_fct == TRUE)) {
        self$args_types <- lapply(seq_along(ret), function(x) {
          return("SEXP")
        })
      } else {
        self$args_types <- lapply(seq_along(ret), function(x) {
          return("etr::Vec<double>")
        })
      }

      # TODO: borrow ptr and BorrowSEXP is missing
      # NOTE: check for types: db, it, lg, dbs, its, lgs, dvp, ivp, lvp, dmp, imp, lmp
      for (i in seq_along(ret)) {
        l <- as.character(ret[[i]])
        nm <- unlist(strsplit(l, "")) |>
          tail(3) |>
          paste(collapse = "")
        if (nm == "_db") {
          self$args_types[[i]] <- "double"
        } else if (nm == "_it") {
          self$args_types[[i]] <- "int"
        } else if (nm == "_lg") {
          self$args_types[[i]] <- "bool"
        }
        nm <- unlist(strsplit(l, "")) |>
          tail(4) |>
          paste(collapse = "")
        if (nm == "_dbs") {
          stopifnot("Vec<double> arguments not available in R interface" = !self$R_fct)
          self$args_types[[i]] <- "etr::Vec<double>"
        } else if (nm == "_its") {
          stopifnot("Vec<int> arguments not available in R interface" = !self$R_fct)
          self$args_types[[i]] <- "etr::Vec<int>"
        } else if (nm == "_lgs") {
          stopifnot("Vec<bool> arguments not available in R interface" = !self$R_fct)
          self$args_types[[i]] <- "etr::Vec<bool>"
        } else if (nm == "_dvp") {
          stopifnot("ptr arguments not available in R interface" = !self$R_fct)
          self$args_types[[i]] <- "double_ptr"
        } else if (nm == "_ivp") {
          stopifnot("ptr arguments not available in R interface" = !self$R_fct)
          self$args_types[[i]] <- "int_ptr"
        } else if (nm == "_lvp") {
          stopifnot("ptr arguments not available in R interface" = !self$R_fct)
          self$args_types[[i]] <- "bool_ptr"
        } else if (nm == "_dmp") {
          stopifnot("ptr arguments not available in R interface" = !self$R_fct)
          self$args_types[[i]] <- "double_mat_ptr"
        } else if (nm == "_imp") {
          stopifnot("ptr arguments not available in R interface" = !self$R_fct)
          self$args_types[[i]] <- "int_mat_ptr"
        } else if (nm == "_lmp") {
          stopifnot("ptr arguments not available in R interface" = !self$R_fct)
          self$args_types[[i]] <- "bool_mat_ptr"
        }
      }
    },
    identify_type = function(ret) {
      self$var_types <- vector("list", length = length(ret))
      self$var_types <- lapply(self$var_types, function(x) {
        return("etr::Vec<double>")
      })
      # NOTE: check for types: db, it, lg, dbs, its, lgs
      for (i in seq_along(ret)) {
        l <- as.character(ret[[i]])
        nm <- unlist(strsplit(l, "")) |>
          tail(3) |>
          paste(collapse = "")
        if (nm == "_db") {
          self$var_types[[i]] <- "double"
        } else if (nm == "_it") {
          self$var_types[[i]] <- "int"
        } else if (nm == "_lg") {
          self$var_types[[i]] <- "bool"
        }
        nm <- unlist(strsplit(l, "")) |>
          tail(4) |>
          paste(collapse = "")
        if (nm == "_dbs") {
          self$var_types[[i]] <- "etr::Vec<double>"
        } else if (nm == "_its") {
          self$var_types[[i]] <- "etr::Vec<int>"
        } else if (nm == "_lgs") {
          self$var_types[[i]] <- "etr::Vec<bool>"
        }
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
      self$identify_type(ret)
      return(ret)
    }
  )
)
