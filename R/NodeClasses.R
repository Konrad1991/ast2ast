# R package ast2ast
# Copyright (C) 2021 Konrad Krämer
#
# This file is part of R package ast2ast
#
#
# ast2ast is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with ast2ast
# If not see: https://www.gnu.org/licenses/old-licenses/gpl-2.0.html#SEC4

#' @import R6
PC <- R6::R6Class("PC",
  public = list(
    name_fct = NULL,
    arguments = list(),
    namespace_etr = NULL,
    variable_type_pair = NULL,
    initialize = function(node, namespace_etr) {
      self$name_fct <- node[[1]]
      self$arguments <- node[2:length(node)]
      fct_arg <- order_args(self$arguments, self$name_fct)
      self$name_fct <- fct_arg[[1]]
      self$arguments <- (fct_arg[2:length(fct_arg)] |> as.list())[[1]]
      self$namespace_etr <- namespace_etr
    },
    get_name = function() {
      return(self$name_fct)
    },
    replace_TF = function() {
      i <- 1
      while (i <= length(self$arguments)) {
        if (is.symbol(self$arguments)) {
          return()
        }

        if (!is.null(self$arguments[[i]])) {
          if (isTRUE(self$arguments[[i]]) || (self$arguments[[i]] == as.name("T"))) {
            self$arguments[[i]] <- as.name("true")
          } else if (isFALSE(self$arguments[[i]]) ||
            (self$arguments[[i]] == as.name("F"))) {
            self$arguments[[i]] <- as.name("false")
          }
        }
        i <- i + 1
      }
    },
    replace_int = function() {
      i <- 1
      while (i <= length(self$arguments)) {
        if (is.symbol(self$arguments)) {
          return()
        }

        if (is.null(self$arguments[[i]])) {
          return()
        }
        if (is.atomic(self$arguments[[i]])) {
          number2 <- gsub("[0-9]", "", self$arguments[[i]])
          number2 <- gsub("e", "", number2)
          number2 <- gsub(as.name("+"), "", number2)
          number2 <- gsub("-", "", number2)
          size2 <- nchar(number2)
          if (is.na(size2)) {
            return()
          }
          if (size2 == 0) {
            self$arguments[[i]] <- str2lang(paste0("etr::i2d(", self$arguments[[i]], ")"))
          }
        }
        i <- i + 1
      }
    },
    replace_NA = function() {
      i <- 1
      while (i <= length(self$arguments)) {
        if (is.symbol(self$arguments)) {
          return()
        }
        if (is.null(self$arguments[[i]])) {
          return()
        }
        if ((is.atomic(self$arguments[[i]])) && (is.na(self$arguments[[i]]))) {
          self$arguments[[i]] <- str2lang("NA_REAL")
        }
        i <- i + 1
      }
    },
    replace_INF = function() {
      i <- 1
      while (i <= length(self$arguments)) {
        if (is.symbol(self$arguments)) {
          return()
        }
        if (is.null(self$arguments[[i]])) {
          return()
        }
        if ((is.atomic(self$arguments[[i]])) && (is.infinite(self$arguments[[i]]))) {
          self$arguments[[i]] <- str2lang("R_PosInf") # check for R_NegInf how?
        }
        i <- i + 1
      }
    },
    oaf = function(var) { # only allowed functions
      check <- deparse(self$name_fct) %in% var
      return(check)
    },
    get_var_names = function() {
      ret <- list()
      counter <- 1
      arg <- self$arguments
      for (i in seq_along(arg)) {
        temp <- arg[[i]]

        if (!is.call(temp) && (length(temp) == 1L && is.name(temp))) {
          ret[[counter]] <- temp
          counter <- counter + 1
        }
      }

      na_inf <- c("NA_REAL", "R_PosInf", "R_NegInf")
      indices <- ret %in% na_inf
      ret <- ret[indices == FALSE]

      # NOTE: not allowed variable names
      # TODO: add check for not allowed variable names
      na <- c("etr", "true", "false")
      indices <- ret %in% na
      ret <- ret[indices == FALSE]

      return(ret)
    },
    order_args = function(user_args, name_of_fct) {
      self$arguments <- order_args(self$arguments, self$name_fct)
    },
    change_args = function(name_of_fct, user_args) {
      # get name of list
      named <- function(x) {
        if (!is.null(names(x))) {
          return(names(x) != "")
        }
        return(FALSE)
      }
      # find named args
      named_args <- named(user_args)
      equal_names <- names(user_args[named_args])
      # find args required for function
      args_fct <- methods::formalArgs(name_of_fct)

      # check that user only use defined arg names
      check <- all(equal_names %in% args_fct)
      if (check == FALSE) {
        def_logis <- equal_names %in% args_fct
        lapply(seq_along(def_logis), function(x) {
          if (def_logis[[x]] == FALSE) {
            message(paste("Argument", equal_names[[x]], "is not defined for function", name_of_fct))
            stop("Error")
          }
        })
      }

      # combine named and unnamed args in correct order
      # is named --> use this value
      # if not named take the first, second etc. not named element
      not_equal_names <- user_args[!named(user_args)]
      counter_not_equal <- 1
      counter <- 1
      res <- list()
      i <- 1
      while (i <= length(args_fct)) {
        temp <- args_fct[[i]]
        if (temp %in% equal_names) {
          res[[counter]] <- user_args[[temp]]
          counter <- counter + 1
        } else {
          if (counter_not_equal <= length(not_equal_names)) {
            res[[counter]] <- not_equal_names[[counter_not_equal]]
            counter_not_equal <- counter_not_equal + 1
            counter <- counter + 1
          } else {
            arg <- formals(name_of_fct)[[temp]]
            if (is.atomic(arg)) {
              number2 <- gsub("[0-9]", "", arg)
              number2 <- gsub("e", "", number2)
              number2 <- gsub(as.name("+"), "", number2)
              number2 <- gsub("-", "", number2)
              size2 <- nchar(number2)
              if (size2 == 0) { # int found
                arg <- str2lang(paste0("etr::i2d(", arg, ")"))
              }
            }
            res[[counter]] <- arg
            counter <- counter + 1
          }
        }
        i <- i + 1
      }

      return(res)
    }
  )
) # end class


#' @import R6
generic <- R6::R6Class("generic",
  inherit = PC,
  public = list(
    change_code = function() {
      self$replace_int()
    },
    convert = function(var) {
      self$replace_INF()
      self$replace_NA()
      self$replace_TF()
      self$oaf(var)
      self$change_code()

      ret <- list()
      if (deparse(self$name_fct) %in% self$namespace_etr) {
        ret[[1]] <- as.name(paste0("etr::", self$name_fct))
      } else {
        ret[[1]] <- self$name_fct
      }

      ret <- c(ret, self$arguments)
      return(ret)
    }
  )
)

#' @import R6
vector_class <- R6::R6Class("vector_class",
  inherit = PC,
  public = list(
    convert = function(var) {
      self$replace_INF()
      self$replace_NA()
      self$replace_TF()
      self$oaf(var)

      ret <- list()
      if (deparse(self$name_fct) %in% self$namespace_etr) {
        ret[[1]] <- as.name(paste0("etr::", self$name_fct))
      } else {
        ret[[1]] <- self$name_fct
      }

      ret <- c(ret, self$arguments)
      return(ret)
    }
  )
)


#' @import R6
assign <- R6::R6Class("assign",
  inherit = PC,
  public = list(
    change_code = function() {
      self$replace_int()
      self$name_fct <- as.name("=")
    },
    convert = function(var, calc_d) {
      self$replace_INF()
      self$replace_NA()
      self$replace_TF()
      self$oaf(var)
      self$change_code()

      calc_deriv <- function(codeline) {
        fct3 <- as.list(codeline[[3]])[[1]] |> deparse()
        if (fct3 != "get_deriv") {
          result <- list()
          result[[1]] <- str2lang("etr::assign")
          fct <- function() stop("something went wrong")
          body(fct, envir = environment(fct)) <- codeline[[3]]
          result[[2]] <- codeline[[2]]
          result[[3]] <- codeline[[3]]
          result[[4]] <- body(d(fct))
          return(result)
        } else {
          return(codeline)
        }
      }

      if (is.language(self$arguments[[1]])) {
        temp <- as.list(self$arguments[[1]])
        if (deparse(temp[[1]]) == "::") {
          self$arguments[[1]] <- temp[[2]]
          self$variable_type_pair <- c(temp[[2]], temp[[3]])
        }
      }
      ret <- list()
      if (deparse(self$name_fct) %in% self$namespace_etr) {
        ret[[1]] <- as.name(paste0("etr::", self$name_fct))
      } else {
        ret[[1]] <- self$name_fct
      }
      lapply(self$arguments, function(x) {
        if (is.character(x)) {
          stop("You cannot use characters in assignments")
        }
      })
      ret <- c(ret, self$arguments)
      if (calc_d) {
        ret <- calc_deriv(c(self$name_fct, self$arguments))
      }
      return(ret)
    }
  )
)

#' @import R6
retur <- R6::R6Class("retur",
  inherit = PC,
  public = list(
    R_fct = NULL,
    initialize = function(node, namespace_etr, R_fct) {
      self$R_fct <- R_fct
      self$name_fct <- node[[1]]
      if (length(node) == 2) {
        self$arguments <- node[2:length(node)]
      } else {
        if (R_fct) {
          self$arguments <- as.name("R_NilValue")
        }
      }

      self$namespace_etr <- namespace_etr
    },
    change_code = function() {
      self$replace_int()
    },
    convert = function(var) {
      self$replace_INF()
      self$replace_NA()
      self$replace_TF()
      self$oaf(var)
      self$change_code()

      ret <- list()
      if (deparse(self$name_fct) %in% self$namespace_etr) {
        ret[[1]] <- as.name(paste0("etr::", self$name_fct))
      } else {
        ret[[1]] <- self$name_fct
      }

      if (self$R_fct) {
        self$arguments <- str2lang(
          paste("cpp2R(", self$arguments, ")", collapse = "")
        )
        ret <- c(ret, self$arguments)
        return(ret)
      } else {
        return(c(ret, self$arguments))
      }
    }
  )
)

# TODO: this has to be changed according to the new c++ ETR subsetting
#' @import R6
subset <- R6::R6Class("subset",
  inherit = PC,
  public = list(
    subassign = NULL,
    only_num = function() {
      sub_args <- NULL
      if (length(self$arguments) == 2) {
        sub_args <- self$arguments[[2]]
      } else if (length(self$arguments) == 3) {
        sub_args <- self$arguments[2:length(self$arguments)]
      }
      check <- sapply(sub_args, function(x) {
        if (!is.numeric(x)) {
          return(FALSE)
        }
        return(x %% 1 == 0)
      })
      return(all(check == TRUE))
    },
    initialize = function(node, subset_or_subassign, namespace_etr) {
      self$subassign <- subset_or_subassign
      self$name_fct <- node[[1]]
      self$arguments <- node[2:length(node)]
      self$namespace_etr <- namespace_etr
    },
    change_code = function() {
      self$replace_INF()
      self$replace_NA()
      self$replace_TF()
      self$arguments <- lapply(self$arguments, function(x) {
        temp <- as.character(x)
        if (length(temp) == 1L) { # temp == '' && length(temp) == 1L
          if (temp == "") {
            return(as.symbol("true"))
          } else {
            return(x)
          }
        } else {
          return(x)
        }
      })

      if (self$subassign == TRUE) {
        self$name_fct <- as.name(paste0("etr::", "subset")) # subassign
      } else if (self$subassign == FALSE) {
        if (self$only_num()) {
          self$name_fct <- as.name(paste0("etr::", "at"))
        } else {
          self$name_fct <- as.name(paste0("etr::", "subset"))
        }
      }
    },
    get_var_names = function() {
      ret <- list()
      counter <- 1
      arg <- self$arguments
      for (i in seq_along(arg)) {
        temp <- arg[[i]]

        if (!is.call(temp) && (length(temp) == 1L && is.name(temp))) {
          ret[[counter]] <- temp
          counter <- counter + 1
        }
      }

      ret <- ret[ret != as.symbol("true")]

      return(ret)
    },
    convert = function(var) {
      self$replace_INF()
      self$replace_NA()
      self$replace_TF()
      self$oaf(var)
      self$change_code()

      ret <- list()
      if (deparse(self$name_fct) %in% self$namespace_etr) {
        ret[[1]] <- as.name(paste0("etr::", self$name_fct))
      } else {
        ret[[1]] <- self$name_fct
      }
      ret <- c(ret, self$arguments)
      return(ret)
    }
  )
)

#' @import R6
loop <- R6::R6Class("loop",
  inherit = PC,
  public = list(
    index_variable = NULL,
    change_code = function() {
      self$index_variable <- self$arguments[[1]]
      self$arguments[[1]] <- as.name(paste0("auto&", self$arguments[[1]], ":"))
    },
    convert = function(var) {
      self$replace_INF()
      self$replace_NA()
      self$replace_int()
      self$replace_TF()
      self$oaf(var)
      self$change_code()

      ret <- list()
      if (deparse(self$name_fct) %in% self$namespace_etr) {
        ret[[1]] <- as.name(paste0("etr::", self$name_fct))
      } else {
        ret[[1]] <- self$name_fct
      }
      ret <- c(ret, self$arguments)
      return(ret)
    },
    return_index_vars = function() {
      return(as.name(paste0("auto&", self$index_variable, ":")))
    }
  )
)

#' @import R6
coca <- R6::R6Class("coca",
  inherit = PC,
  public = list(
    change_code = function() {
      self$name_fct <- as.name("coca")
    },
    convert = function(var) {
      self$replace_int()
      self$replace_INF()
      self$replace_NA()
      self$replace_TF()
      self$oaf(var)
      self$change_code()

      ret <- list()
      if (deparse(self$name_fct) %in% self$namespace_etr) {
        ret[[1]] <- as.name(paste0("etr::", self$name_fct))
      } else {
        ret[[1]] <- self$name_fct
      }
      ret <- c(ret, self$arguments)
      return(ret)
    }
  )
)

#' @import R6
is_na <- R6::R6Class("isNA",
  inherit = PC,
  public = list(
    change_code = function() {
      self$name_fct <- as.name("isNA")
    },
    convert = function(var) {
      self$replace_int()
      self$replace_INF()
      self$replace_NA()
      self$replace_TF()
      self$oaf(var)
      self$change_code()

      ret <- list()
      if (deparse(self$name_fct) %in% self$namespace_etr) {
        ret[[1]] <- as.name(paste0("etr::", self$name_fct))
      } else {
        ret[[1]] <- self$name_fct
      }
      ret <- c(ret, self$arguments)
      return(ret)
    }
  )
)

#' @import R6
is_infinite <- R6::R6Class("isInf",
  inherit = PC,
  public = list(
    change_code = function() {
      self$name_fct <- as.name("isInf")
    },
    convert = function(var) {
      self$replace_int()
      self$replace_INF()
      self$replace_NA()
      self$replace_TF()
      self$oaf(var)
      self$change_code()

      ret <- list()
      if (deparse(self$name_fct) %in% self$namespace_etr) {
        ret[[1]] <- as.name(paste0("etr::", self$name_fct))
      } else {
        ret[[1]] <- self$name_fct
      }
      ret <- c(ret, self$arguments)
      return(ret)
    }
  )
)

#' @import R6
fastaccess <- R6::R6Class("fastaccess",
  inherit = PC,
  public = list(
    change_code = function() {
      self$name_fct <- as.name("at")
    },
    convert = function(var) {
      self$replace_INF()
      self$replace_NA()
      self$replace_TF()
      self$oaf(var)
      self$change_code()

      ret <- list()
      if (deparse(self$name_fct) %in% self$namespace_etr) {
        ret[[1]] <- as.name(paste0("etr::", self$name_fct))
      } else {
        ret[[1]] <- self$name_fct
      }
      ret <- c(ret, self$arguments)
      return(ret)
    }
  )
)

#' @import R6
printer <- R6::R6Class("printer",
  inherit = PC,
  public = list(
    initialize = function(node, namespace_etr) {
      if (length(node) == 1L) {
        self$name_fct <- node
        self$arguments <- NULL
      } else {
        self$name_fct <- node[[1]]
        self$arguments <- node[2:length(node)]
      }
      self$namespace_etr <- namespace_etr
    },
    change_code = function() {
      self$name_fct <- as.name("etr::print")
    },
    convert = function(var) {
      self$replace_TF()
      self$oaf(var)
      self$change_code()

      ret <- list()
      if (deparse(self$name_fct) %in% self$namespace_etr) {
        ret[[1]] <- as.name(paste0("etr::", self$name_fct))
      } else {
        ret[[1]] <- self$name_fct
      }
      ret <- c(ret, self$arguments)
      return(ret)
    }
  )
)

#' @import R6
range <- R6::R6Class("range",
  inherit = PC,
  public = list(
    change_code = function() {
      self$replace_int()
      self$name_fct <- as.name("colon")
    },
    convert = function(var) {
      self$replace_INF()
      self$replace_NA()
      self$replace_TF()
      self$oaf(var)
      self$change_code()

      ret <- list()
      if (deparse(self$name_fct) %in% self$namespace_etr) {
        ret[[1]] <- as.name(paste0("etr::", self$name_fct))
      } else {
        ret[[1]] <- self$name_fct
      }
      ret <- c(ret, self$arguments)
      return(ret)
    }
  )
)


#' @import R6
math <- R6::R6Class("math",
  inherit = PC,
  public = list(
    change_code = function() {
      self$replace_int()
      if (self$name_fct == "sin") {
        self$name_fct <- as.name("sinus")
      } else if (self$name_fct == "asin") {
        self$name_fct <- as.name("asinus")
      } else if (self$name_fct == "sinh") {
        self$name_fct <- as.name("sinush")
      } else if (self$name_fct == "cos") {
        self$name_fct <- as.name("cosinus")
      } else if (self$name_fct == "acos") {
        self$name_fct <- as.name("acosinus")
      } else if (self$name_fct == "cosh") {
        self$name_fct <- as.name("cosinush")
      } else if (self$name_fct == "tan") {
        self$name_fct <- as.name("tangens")
      } else if (self$name_fct == "atan") {
        self$name_fct <- as.name("atangens")
      } else if (self$name_fct == "tanh") {
        self$name_fct <- as.name("tangensh")
      } else if (self$name_fct == "^") {
        self$name_fct <- as.name("power")
      } else if (self$name_fct == "exp") {
        self$name_fct <- as.name("exp")
      } else if (self$name_fct == "log") {
        self$name_fct <- as.name("ln")
      } else if (self$name_fct == "sqrt") {
        self$name_fct <- as.name("sqroot")
      }
    },
    convert = function(var) {
      self$replace_INF()
      self$replace_NA()
      self$replace_TF()
      self$oaf(var)
      self$change_code()

      ret <- list()
      if (deparse(self$name_fct) %in% self$namespace_etr) {
        ret[[1]] <- as.name(paste0("etr::", self$name_fct))
      } else {
        ret[[1]] <- self$name_fct
      }
      ret <- c(ret, self$arguments)
      return(ret)
    }
  )
)
