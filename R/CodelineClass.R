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
# You should have received a copy of the GNU General Public License along with ast2ast
# If not see: https://www.gnu.org/licenses/old-licenses/gpl-2.0.html#SEC4

permitted_fcts <- function() {
  c(
    "::", "=", "<-", "[", "at", "for", "while", "next", "break", "c", ":",
    "sin", "asin", "sinh", "cos", "acos", "cosh",
    "tan", "atan", "tanh", "log", "sqrt",
    "^", "+", "-", "*", "/",
    "if", "{", "(",
    "==", "!=", ">", ">=", "<", "<=", "print", "return",
    "vector", "matrix", "length", "dim",
    "exp", "&&", "||", "!",
    "is.na", "is.infinite", "is.finite",
    "Rf_ScalarReal", "i2d", "cmr", "cpp2R",
    "vector_numeric",
    "vector_logical", "while",
    "vector_integer", "rep", "get_deriv", "assign",
    "set_indep", "unset_indep", "power"
  )
}

namespace_etr <- function() {
  c(
    "coca", "sinus", "asinus", "sinush",
    "cosinus", "acosinus", "cosinush",
    "tangens", "atangens", "tangensh",
    "ln", "sqroot",
    "print", "vector", "matrix", "length", "dim", "cmr",
    "exp", "i2d", "at", "exp", "at",
    "isNA", "isInfinite", "colon", "cpp2R", "rep", "get_deriv",
    "assign_deriv", "set_indep", "unset_indep", "power"
  )
}

generic_fcts <- function() {
  c(
    "+", "-",
    "*", "/", "if", "else if", "else", "{", "(",
    "==", "!=", ">", ">=", "<", "<=", "vector",
    "rep", "::",
    "matrix", "length", "dim", "cmr", "exp", "at",
    "&&", "||", "Rf_ScalarReal", "cpp2R",
    "rep", "while", "get_deriv", "assign",
    "unset_indep", "power"
  )
}

math_fcts <- function() {
  c(
    "sin", "asin", "sinh", "cos", "acos", "cosh",
    "tan", "atan", "tanh", "log", "^", "sqrt", "power"
  )
}


#' @import R6
LC <- R6::R6Class("LC",

  # for performance reason
  # subset, subassign, <-
  # fast access via [] which will be replaced by ()
  public = list(
    R_fct = NULL,
    PF = permitted_fcts(), # permitted functions
    namespace_etr = namespace_etr(),
    namespace_etr_resolved = function() {
      paste0("etr::", self$namespace_etr)
    },
    vars = list(), # variables
    index_vars = list(),
    ast = NULL,
    code = NULL,
    generic_fct = generic_fcts(),
    math = math_fcts(),
    check_assign_subset = NULL,
    found_return = FALSE,
    variable_type_pair = NULL,
    return_variable = NULL,
    calc_deriv = FALSE,
    extractast = function(sexp) {
      if (is.null(sexp)) {
        stop("Found empty function")
      }
      if (!is.call(sexp)) {
        if (as.name("=") != sexp) {
          self$check_assign_subset <- FALSE
        }

        return(sexp)
      }

      sexp <- as.list(sexp)

      if (as.name("{") == sexp[[1]] && length(sexp) == 1) {
        return(str2lang("{}"))
      }

      # assign to node classes
      fct <- sexp[[1]]

      if ((as.name("<-") == fct) || (as.name("=") == fct)) {
        self$check_assign_subset <- TRUE
        p <- assign$new(sexp, self$namespace_etr)
        sexp <- p$convert(self$PF, self$calc_deriv)
        self$vars <- c(self$vars, p$get_var_names())
        self$variable_type_pair <- p$variable_type_pair
      } else if ((deparse(fct) %in% self$generic_fct)) {
        self$check_assign_subset <- FALSE
        p <- generic$new(sexp, self$namespace_etr)
        sexp <- p$convert(self$PF)
      } else if (as.name("return") == fct) {
        p <- retur$new(sexp, self$namespace_etr, self$R_fct)
        sexp <- p$convert(self$PF)
        self$found_return <- TRUE
        if (length(sexp) == 2) {
          self$return_variable <- sexp[[2]]
        } else if (length(sexp) == 1) {
          self$return_variable <- NULL
        }
      } else if (as.name("[") == fct) {
        p <- subset$new(sexp, self$check_assign_subset, self$namespace_etr)
        sexp <- p$convert(self$PF)
        self$check_assign_subset <- FALSE
      } else if (as.name("c") == fct) {
        p <- coca$new(sexp, self$namespace_etr)
        sexp <- p$convert(self$PF)
        self$check_assign_subset <- FALSE
      } else if (as.name(":") == fct) {
        p <- range$new(sexp, self$namespace_etr)
        sexp <- p$convert(self$PF)
        self$check_assign_subset <- FALSE
      } else if (deparse(fct) %in% self$math) {
        p <- math$new(sexp, self$namespace_etr)
        sexp <- p$convert(self$PF)
        self$check_assign_subset <- FALSE
      } else if (as.name("for") == fct) {
        p <- loop$new(sexp, self$namespace_etr)
        sexp <- p$convert(self$PF)
        self$check_assign_subset <- FALSE
        self$index_vars <- c(self$index_vars, p$return_index_vars())
      } else if (as.name("print") == fct) {
        p <- printer$new(sexp, self$namespace_etr)
        sexp <- p$convert(self$PF)
        self$check_assign_subset <- FALSE
      } else if (as.name("is.na") == fct) {
        p <- is_na$new(sexp, self$namespace_etr)
        sexp <- p$convert(self$PF)
        self$check_assign_subset <- FALSE
      } else if (as.name("is.infinite") == fct) {
        p <- is_infinite$new(sexp, self$namespace_etr)
        sexp <- p$convert(self$PF)
        self$check_assign_subset <- FALSE
      } else if (as.name("i2d") == fct) {
        # prevent stack overflow
      } else if (deparse(fct) %in% self$namespace_etr_resolved()) {
        # nothing to do.
      } else if ("::" == deparse(fct)) {
        p <- var_args$new(sexp, self$namespace_etr)
        sexp <- p$convert(self$PF)
        self$check_assign_subset <- FALSE
      } else if ("set_indep" == fct) {
        self$check_assign_subset <- FALSE
        p <- generic$new(sexp, self$namespace_etr)
        sexp <- p$convert(self$PF)
        self$calc_deriv <- TRUE
      } else {
        color_print(43, "Error: Sorry not all functions are supported")
        color_print(43, "Function: ")
        color_print(41, fct)
        color_print(43, " not supported")
        stop()
      }
      # convert node classes
      lapply(sexp, self$extractast)
    },
    initialize = function(line, R_fct) {
      self$R_fct <- R_fct
      self$ast <- self$extractast(line) # fill nodes
    },
    get_calls = function(code) {
      out <- purrr::map_if(code, is.list, self$get_calls)
      out <- as.call(out)
      return(out)
    },
    ast2call = function() {
      self$code <- self$get_calls(self$ast)
    },
    get_vars = function() {
      return(unique(self$vars))
    },
    get_indexvars = function() {
      return(unique(self$index_vars))
    }
  )
) # end class
