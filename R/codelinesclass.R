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

LC <- R6::R6Class("LC",

# for performance reason
# subset, subassign, <-
# fast access via [] which will be replaced by ()

      public = list(

       PF = c("=", "<-", "[", "for", "c", ":", "sin", "asin", "sinh", "cos", "acos", "cosh",
        "tan", "atan", "tanh", "log", "^", "+", "-",
         "*", "/", "if", "else if", "else", "{", "(",
        "==", "!=", ">", ">=", "<", "<=", "print", "return", "vector", "matrix", "length", "dim", "cmr", "sub", "exp", "i2d", "at", "&&", "||"), # permitted functions

       vars = list(), # variables
       index_vars = list(),

       ast = NULL,
       code = NULL,
       generic_fct = c("+", "-",
        "*", "/", "if", "else if", "else", "{", "(",
       "==", "!=", ">", ">=", "<", "<=", "return", "vector", "matrix","length", "dim", "cmr", "exp", "at", "&&", "||"),

       math = c("sin", "asin", "sinh", "cos", "acos", "cosh",
        "tan", "atan", "tanh", "log", "^"), # %% function is missing

       check_assign_subset = NULL,
       found_return = FALSE,

       extractast = function(sexp) {

          if(!is.call(sexp)) {

              if(as.name("=") != sexp) {
                self$check_assign_subset = FALSE
              }

            return(sexp)
          }

          sexp <- as.list(sexp)

          # assign to node classes
          fct <- sexp[[1]]
          if( (as.name("<-") == fct) || (as.name("=") == fct) ) {

            self$check_assign_subset = TRUE
            p <- assign$new(sexp)
            sexp <- p$convert(self$PF)
            self$vars <- c(self$vars, p$get_var_names())

          } else if( (deparse(fct) %in% self$generic_fct) ){
            self$check_assign_subset = FALSE
            p <- generic$new(sexp)
            sexp <- p$convert(self$PF)
            self$vars <- c(self$vars, p$get_var_names())

            if(fct == as.name('return')) {
              self$found_return = TRUE
            }

          } else if(as.name("[") == fct) {

            #p <- fastaccess$new(sexp)
            p <- subset$new(sexp, self$check_assign_subset)
            sexp <- p$convert(self$PF)
            self$vars <- c(self$vars, p$get_var_names())
            self$check_assign_subset = FALSE

          } else if(as.name("c") == fct) {

            p <- coca$new(sexp)
            sexp <- p$convert(self$PF)
            self$vars <- c(self$vars, p$get_var_names())
            self$check_assign_subset = FALSE

          } else if(as.name(":") == fct) {

            p <- range$new(sexp)
            sexp <- p$convert(self$PF)
            self$vars <- c(self$vars, p$get_var_names())
            self$check_assign_subset = FALSE

          } else if(deparse(fct) %in% self$math) {

            p <- math$new(sexp)
            sexp <- p$convert(self$PF)
            self$vars <- c(self$vars, p$get_var_names())
            self$check_assign_subset = FALSE

          } else if(as.name('for') == fct) {

            p <- loop$new(sexp)
            sexp <- p$convert(self$PF)
            self$vars <- c(self$vars, p$get_var_names())
            self$check_assign_subset = FALSE
            self$index_vars <- c(self$index_vars, p$return_index_vars())

          } else if(as.name('print') == fct) {

            p <- printer$new(sexp)
            sexp <- p$convert(self$PF)
            self$vars <- c(self$vars, p$get_var_names())
            self$check_assign_subset = FALSE

          } else if(as.name("i2d") == fct) {
            # prevent stack overflow
          } else {
            cat("Error: Sorry not all  functions are supported", "\n")
            cat("Function: ", fct, " not supported")
            stop()
          }

          # convert node classes
          lapply(sexp, self$extractast)

       },

       initialize = function(line) {
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
