astClass <- R6::R6Class("astClass",
  public = list(
    args = NULL,
    args_2_fct = NULL,
    body = NULL,
    ast = list(),
    code = list(),
    char = list(),
    var_all = c(),
    var_index = c(),
    return_TF = c(),
    variable_type_pairs = list(),
    return_variables = list(),
    R_fct = NULL,
    calc_derivs = FALSE,
    initialize = function(fct, name_f, R_fct) {
      self$args <- methods::formalArgs(fct)
      self$args_2_fct <- methods::formalArgs(fct)
      self$body <- body(fct)[2:length(body(fct))]
      self$R_fct <- R_fct
    },
    getast = function() {
      for (i in seq_along(self$body)) {
        temp <- NULL
        e <- tryCatch(
          {
            temp <- LC$new(self$body[[i]], self$R_fct)
            NULL
          },
          error = function(err) {
            e <- err
            e
          }
        )
        if (!is.null(e)) {
          wrong_code <- paste(
            "Error in line", i, ": \n",
            deparse(as.call(self$body[[i]]))
          )
          color_print(43, conditionMessage(e))
          color_print(41, wrong_code)

          stop()
        }

        self$ast[[i]] <- temp$ast
        self$calc_derivs <- c(self$calc_derivs, temp$calc_deriv)
        self$var_all <- c(self$var_all, temp$vars)
        self$var_index <- c(self$var_index, temp$index_vars)
        self$return_TF <- c(self$return_TF, temp$found_return)
        self$variable_type_pairs[[length(self$variable_type_pairs) + 1]] <- temp$variable_type_pair
        self$return_variables[[length(self$return_variables) + 1]] <- temp$return_variable
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
    add_semicolon = function(code) {
      for (i in seq_along(code)) {
        c <- strsplit(code[[i]], split = "")[[1]]
        if (!(c[length(c)] %in% c("{", "}"))) {
          code[[i]] <- paste0(code[[i]], ";")
        }
      }
      return(code)
    },
    call2char = function() {
      j <- 1
      for (i in seq_along(self$code)) {
        temp <- deparse(self$code[[i]], width.cutoff = 500)
        temp <- gsub("\\bin\\b", "", temp)
        temp <- gsub("`", "", temp)
        temp <- self$add_semicolon(temp)
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
      self$identify_type(ret)
      return(ret)
    },
    get_variable_type_pairs = function() {
      return(self$variable_type_pairs)
    },
    get_return_variable = function() {
      return(self$return_variables)
    },
    get_calc_deriv = function() {
      return(any(self$calc_derivs))
    }
  )
)
