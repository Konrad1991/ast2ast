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

        signature = function(desired_type, reference) {
              # arguments passed to f & define signature

              arguments_string <- sapply(self$args, function(x) {
                y <- paste0(x)
                if(reference == TRUE) {
                  desired_type = paste(desired_type, '&')
                }
                temp <- paste(desired_type, y, ',', collapse = '')

                if (parent.frame()$i[] == length(self$args) )  {
                  temp <- paste(desired_type, y, collapse = '')
                }

                return(temp)
              })
              arguments_string <- paste(arguments_string, collapse = " ")

              sig <- paste(self$return_type(), self$name, '(', arguments_string, ')', '{', collapse = " ")
        },

        vars_declaration = function(desired_type) {
          args_dec <- sapply(self$get_vars(), function(x) {
            temp <- paste("sexp", x, ';', collapse = '')
            return(temp)
          })
          args_dec <- paste(args_dec, collapse = " ")

          return(args_dec)
        },

        build = function(verbose = FALSE, reference = FALSE) {
          self$getast()
          self$ast2call()
          self$call2char()

          fct = c(
            self$signature(self$desired_type, reference),
            self$vars_declaration(self$desired_type),
            self$char,
            '}'
          )

          fct <- paste( unlist(fct), collapse='')
        }


      )


)

#' Translates a R function into a C++ function and returns an external pointer (XPtr) to this function.
#' @param f The function which should be translated from R to C++.
#' @param verbose If set to true the output of RcppXPtrUtils::cppXPtr is printed.
#' @param reference If set to true the arguments are passed by reference.
#' @return The external pointer of the generated C++ function
#' @details The following functions are supported:
#' @details For assignment: = and <- can be used.
#' @details In order to allocate memory the functions: vector and matrix can be used.
#' Following forms are possible: vector(size_of_elements), vector(value, size_of_elements)
#' matrix(nrows, ncols), matrix(value, nrows, ncols). The latter fills the matrix or the vector with the specified 'value'.
#' @details In order to get information about a vector or a matrix the functions length and dim work the same way as in R.
#' @details Basic operations can be conducted on scalars, vectors and matrices +, -, *, /
#' @details For indices squared brackets  '[]' or the function sub are used as in R. [] can only be used for one element. Whereas sub can be used as the []-function in R itself.
#' @details Mathematical functions: sin, asin, sinh, cos, acos, cosh, tan, atan, tanh, log, ^ and exp
#' @details For loops can be written as used in R 'for(index in whatever){}'.
#' @details To concatenate objects use the 'c' function as usually in R.
#' @details if,else if, else are used in the same way as in R
#' @details For comparison the functions ==, !=, >, <, >= and <= can be used for scalars, vectors and matrices.
#' @details The print function accepts either a scalar, vector, matrix, string, bool or nothing (empty line).
#' @details In order to return an object use the 'return' function (The last object is not returned automatically as in R).
#' @details Be aware that the R code is translated to ETR. An expression template library which tries to mimic R.
#' @details However, it does not behave exactly like R! Please check your compiled function before using it in a serious project.
#' @details If you want to see how ETR differs from R in detail check the vignette: 'Expression template R (ETR)'
#' @examples
#' @examples #Further examples can be found in vignette: 'Examples'
#' @examples #Hello World
#' \dontrun{
#' f <- function() { print("Hello World!")}
#' pointer_to_f_cpp <- ast2ast::translate(f)
#' Rcpp::sourceCpp(code = "
#' #include <Rcpp.h>
#' typedef void (*fp)();
#'
#' // [[Rcpp::export]]
#' void call_fct(Rcpp::XPtr<fp> inp) {
#'   fp f = *inp;
#'   f();
#' }
#' ")
#' call_fct(pointer_to_f_cpp)
#' }

translate <- function(f, verbose = FALSE, reference = FALSE) {

    stopifnot(is.function(f))

    desired_type = 'sexp'

    a = MA$new(f, desired_type)
    fct <- a$build(verbose, reference = reference)
    #fct_ret = Rcpp::cppFunction(code = fct, plugins = c("cpp17"), depends = c("ast2ast"), includes = "#include <etr.hpp>", verbose =  verbose)
    fct_ret = RcppXPtrUtils::cppXPtr(code = fct, plugins = c("cpp17"), depends = c("ast2ast", "RcppArmadillo"), includes = "#include <etr.hpp>", verbose =  verbose)

    return(fct_ret)
}
