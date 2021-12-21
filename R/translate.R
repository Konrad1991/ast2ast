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


# Masterclass
MA <- R6::R6Class("MA",

      public = list(

        name = NULL,
        args = NULL,
        body = NULL,
        desired_type = NULL,
        temp = list(),
        ast = list(),
        code = list(),
        char = list(),
        var_all = c(),
        var_index = c(),
        return_TF = c(),

        initialize = function(fct, desired_type) {
          self$name = as.character(substitute(fct))
          self$args = methods::formalArgs(fct)
          self$body = body(fct)
          self$body = self$body[2:length(self$body)]
          self$desired_type = desired_type
        },

        getast = function() {
          for(i in seq_along(self$body)) {
            # convert ast R --> ETR
            self$temp[[i]] = LC$new(self$body[[i]])
            self$ast[[i]] = self$temp[[i]]$ast

            # get vars
            self$var_all = c(self$var_all, self$temp[[i]]$vars)
            self$var_index = c(self$var_index, self$temp[[i]]$index_vars)

            # return was found?
            self$return_TF = c(self$return_TF, self$temp[[i]]$found_return)
          }
        },

        get_calls = function(code) {
          out <- purrr::map_if(code, is.list, self$get_calls)
          out <- as.call(out)
          return(out)
        },

        ast2call = function() {
          for(i in seq_along(self$ast)) {
            self$code[[i]] = self$get_calls(self$ast[[i]])
          }
        },

        call2char = function() {
          for(i in seq_along(self$code)) {
            self$char[[i]] = deparse(self$code[[i]], width.cutoff = 500)
            self$char[[i]] = gsub("\\bin\\b", "", self$char[[i]])
            self$char[[i]] = gsub("`", "", self$char[[i]])
            self$char[[i]] = paste0(self$char[[i]], ';')
          }
        },

        get_vars = function() {
          temp1 = unique(self$var_all)
          temp2 = unique(self$var_index)

          # remove index variables
          # necessary in order to remove i and auto$i:
          temp3 = list()
          for(i in seq_along(temp2)) {
            tm <- gsub('auto&', '', temp2[[i]])

            tm <- gsub(':', '', tm)
            temp3[[i]] = tm
          }

          temp2 = c(temp2, unlist(temp3))

          for(i in seq_along(temp1)) {
            for(j in seq_along(temp2)) {
              if(temp1[[i]] == temp2[[j]]) {
                temp1[[i]] = NA
                break
              }
            }
          }

          temp1 = temp1[!is.na(temp1)]

          # remove arguments passed to fct
          for(i in seq_along(temp1)) {
            for(j in seq_along(self$args)) {
              if(temp1[[i]] == self$args[[j]]) {
                temp1[[i]] = NA
                break
              }
            }
          }

          ret = temp1[!is.na(temp1)]

          return(ret)
        },


        return_type = function() {
          return_TF <- any(TRUE %in% self$return_TF)

          # determine return type
          ret_type <- as.symbol("void")
          if(return_TF == TRUE) {
              ret_type <- as.symbol(self$desired_type)
          }

          return(ret_type)
        },

        signature = function(desired_type) {
              # arguments passed to f & define signature
              arguments_string <- sapply(self$args, function(x) {
                y <- paste(x, "SEXP", sep = "")
                temp <- paste(desired_type, y, ',', collapse = '')

                if (parent.frame()$i[] == length(self$args) )  {
                  temp <- paste(desired_type, y, collapse = '')
                }

                return(temp)
              })
              arguments_string <- paste(arguments_string, collapse = " ")

              sig <- paste(self$return_type(), self$name, '(', arguments_string, ')', '{', collapse = " ")
        },


        args_declaration = function() {
          args_dec <- sapply(self$args, function(x) {
            inp <- paste(x, "SEXP", sep = "")

            temp <- paste("sexp", x, '=', inp, ';', collapse = '')
            return(temp)
          })
          args_dec <- paste(args_dec, collapse = " ")

          return(args_dec)
        },

        vars_declaration = function(desired_type) {
          args_dec <- sapply(self$get_vars(), function(x) {
            temp <- paste("sexp", x, ';', collapse = '')
            return(temp)
          })
          args_dec <- paste(args_dec, collapse = " ")

          return(args_dec)
        },

        build = function(verbose = FALSE) {
          self$getast()
          self$ast2call()
          self$call2char()

          fct = c(
            self$signature(self$desired_type),
            self$args_declaration(),
            self$vars_declaration(self$desired_type),
            self$char,
            '}'
          )

          fct <- paste( unlist(fct), collapse='')
        }


      )


)


#' 'translate' translates a R function into a Rcpp function
#'
#' @param f The function which should be translated from R to C++.
#' @param desired_type The desired type which should be used for input and output. Default is SEXP.
#' @return The already compiled Rcpp function.
#' @details Allowed function which can be used:
#' @details "=", "<-", "[", "for", "c", ":", "sin", "asin", "sinh", "cos", "acos", "cosh",
#' @details "tan", "atan", "tanh", "log", "^", "+", "-",
#' @details "*", "/", "if", "else if", "else", "{", "(",
#' @details "==", "!=", ">", ">=", "<", "<=", "print", "return",
#' @details "vector", "matrix", "length", "dim"
#' @details Be aware that the R code is translated to ETR. An expression template library which tries to mimic R.
#' @details However, it does not behave exactly like R! Please check your compiled function before using it in a serious project.
#' @details If you want to see how ETR differs from R in detail check the vignette
#' @examples
#' f <- function() { print("Hello World!")}
#' f_compiled <- translate(f)
translate <- function(f, desired_type = 'SEXP', verbose = FALSE) {

    a = MA$new(f, desired_type)
    fct <- a$build(verbose)
    fct_ret = Rcpp::cppFunction(code = fct, plugins = c("cpp17"), depends = c("ast2ast"), includes = "#include <etr.hpp>", verbose =  verbose)

    return(fct_ret)
}
