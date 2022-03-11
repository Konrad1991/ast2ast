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
        var_types = c(),
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

        call2char = function() { # some errors {}
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

          temp1 = temp1[!is.na(temp1)]

          # remove true and false
          for(i in seq_along(temp1)) {
              if(temp1[[i]] == as.name("true") || temp1[[i]] == as.name("false")) {
                temp1[[i]] = NA
              }
          }

          ret = temp1[!is.na(temp1)]

          self$var_types = vector("list", length = length(ret))
          self$var_types = lapply(self$var_types, function(x) {
            return("sexp")
          })

          # check for doubles
          for(i in seq_along(ret)) {
            nm = as.character(ret[[i]])
            nm = unlist( strsplit(nm, "") )
            if(length(nm) >= 4) {
              if( (nm[[length(nm)]] == "b") && (nm[[length(nm) - 1]] == "d")
                  && (nm[[length(nm) - 2]] == "_") ) {
                  self$var_types[[i]] = "double"
              }
            }
          }

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
          variables = self$get_vars()
          args_dec <- sapply(seq_along(variables), function(x) {
            temp <- paste(self$var_types[[x]], variables[[x]], ';', collapse = '')
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
#' Further information can be found in the vignette: 'Detailed Documentation'.
#' @param f The function which should be translated from R to C++.
#' @param verbose If set to TRUE the output of RcppXPtrUtils::cppXPtr is printed.
#' @param reference If set to true the arguments are passed by reference.
#' @return The external pointer of the generated C++ function
#' @details \strong{The following types are supported: }
#'  \enumerate{
#'    \item numeric vectors
#'    \item numeric matrices
#'  }
#'  Variables can be either numeric vectors or matrices.
#'  Notably, it is possible that the variables change the type within the function.
#'  \strong{It is possible to declare a variable of a scalar numeric data type.
#'          This is done by adding '_db' to the end of the variable. Each time '_db' is found
#'          the variable is declared as a scalar numeric data type. In this case the
#'          object cannot change its type!}
#' @details \strong{The following functions are supported:}
#'  \enumerate{
#'    \item assignment: = and <-
#'    \item allocation: vector and matrix
#'    \item information about objects: length and dim
#'    \item Basic operations: +, -, *, /
#'    \item Indices: [] and at
#'    \item mathematical functions: sin, asin, sinh, cos, acos, cosh, tan, atan, tanh, log, ^ and exp
#'    \item concatenate objects: c
#     \item control flow: for, if, else if, else
#'    \item comparison: ==, !=, >, <, >= and <=
#'    \item printing: print
#'    \item returning objects: return
#'    \item catmull-rome spline: cmr
#'    \item to get a range of numbers the ':' function can be used
#'  }
#' @details  \strong{Some details about the implemented functions}
#' @details  \itemize{
#'    \item allocation of memory works: Following forms are possible: vector(size_of_elements), vector(value, size_of_elements), vector(other_vec, size_of_other_vec),
#'              matrix(nrows, ncols), matrix(value, nrows, ncols) and matrix(vector, nrows, ncols). The latter fills the matrix or the vector with the specified 'value'.
#'    \item For indices squared brackets '[]' can be used as common in R. \strong{Despite the results of calculations cannot be used!} Beyond that the function 'at' exists
#'              which accepts as first argument a variable and as the second argument you pass the desired index.
#'              The caveat of using 'at' is that only one entry can be accessed. The function '[]' can return more then one element.
#'              \strong{The 'at'function returns a reference to the vector entry.
#'                Therefore variable[index] can behave differently then at(variable, index).
#'                The function has to be used carefully when 'at' is used.
#'                Especially if '[]' and 'at' are mixed the function behaviour is difficult to predict.
#'                Please test it before using it in a serious project.}
#'    \item For-loops can be written as common in R
#'            \itemize{
#'                \item Nr.1 \cr
#'                      for(index in variable)\{ \cr
#'                        # do whatever \cr
#'                      \} \cr
#'                \item Nr.2 \cr
#'                      for(index in 1:length(variable)\{ \cr
#'                        # do whatever \cr
#'                      \} \cr
#'    }
#'    \item Be aware that it is not possible to assign the result of a comparison to a variable.
#'    \item The print function accepts either a scalar, vector, matrix, string, bool or nothing (empty line).
#'    \item In order to return an object use the 'return' function (The last object is not returned automatically as in R).
#'    \item In order to interpolate values the 'cmr' function can be used. The function needs three arguments.
#'          \enumerate{
#'            \item the first argument is the point of the independent variable (x) for which the dependent variable should be calculated (y). This has to be a vector of length one.
#'            \item the second argument is a vector defining the points of the independent variable (x). This has to be a vector of at least length four.
#'            \item the third argument is a vector defining the points of the dependent variable (y). This has to be a vector of at least length four.
#'        }
#'  }
#' \strong{Be aware that the R code is translated to ETR an expression template library which tries to mimic R.
#' However, it does not behave exactly like R! Please check your compiled function before using it in a serious project.
#' If you want to see how ast2ast differs from R in detail check the vignette: 'Detailed Documentation'.}
#' @examples #Further examples can be found in the vignette: 'Detailed Documentation'.
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
    stopifnot(is.logical(verbose))
    stopifnot(is.logical(reference))

    desired_type = 'sexp'

    a = MA$new(f, desired_type)
    fct <- a$build(verbose, reference = reference)

    #fct_ret = Rcpp::cppFunction(code = fct,
    #                            plugins = c("cpp17"),
    #                            depends = c("ast2ast"),
    #                            includes = "#include <etr.hpp>",
    #                            verbose =  verbose)

    fct_ret = NULL

    tryCatch(
      expr = {
        fct_ret = RcppXPtrUtils::cppXPtr(code = fct, plugins = c("cpp17"),
                                         depends = c("ast2ast", "RcppArmadillo"),
                                         includes = "#include <etr.hpp>",
                                         verbose =  verbose)
      },
      error = function(e) {
        print("Sorry compilation failed!")
      }
    )

    return(fct_ret)
}
