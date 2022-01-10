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
        "==", "!=", ">", ">=", "<", "<=", "print", "return", "vector", "matrix", "length", "dim", "cmr", "sub", "exp"), # permitted functions

       vars = list(), # variables
       index_vars = list(),

       ast = NULL,
       code = NULL,
       generic_fct = c("+", "-",
        "*", "/", "if", "else if", "else", "{", "(",
       "==", "!=", ">", ">=", "<", "<=", "return", "vector", "matrix","length", "dim", "cmr", "exp"),

       math = c("sin", "asin", "sinh", "cos", "acos", "cosh",
        "tan", "atan", "tanh", "log", "^"),

       check_assign_subset = NULL,
       found_return = FALSE,

       extractast = function(sexp) {

          if(!is.call(sexp)) {

            if(as.name("=") != sexp) {
                self$check_assign_subset = FALSE
            }

            # check whether its integer
            check_var = FALSE
            check_number = FALSE
            check_double = FALSE

            number = sexp
            number1 = as.character(number)
            size1 = nchar(number)

            number2 = gsub("[A-z]", "", number1)
            size2 = nchar(number2)

            if( (size2 != size1) || is.symbol(sexp)) { # letters detected
                check_var = TRUE
            }

            if(check_var == FALSE) {
              number2 = gsub("[0-9]", "", number1)
              number2 = gsub(".", "", number2)
              number2 = gsub("e", "", number2)
              number2 = gsub(as.name("+"), "", number2)
              number2 = gsub("-", "", number2)
              size2 = nchar(number2)

              if(size2 == 0) { # numbers detected
                check_number = TRUE
              }

                if(check_number == TRUE) {
                  number2 = gsub("[0-9]", "", number1)
                  number2 = gsub("e", "", number2)
                  number2 = gsub(as.name("+"), "", number2)
                  number2 = gsub("-", "", number2)

                  size2 = nchar(number2)

                  if(size2 == 1) { # double detected # dot detected
                    if(number2 == '.') {
                      check_double = TRUE
                    }

                  }
                }

            }

            if(check_number == TRUE && check_double == FALSE) { # integer found
              sexp = str2lang(paste0('i2d(', sexp, ")") )
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

          } else if(as.name("sub") == fct) {

            p <- subset$new(sexp, self$check_assign_subset)
            sexp <- p$convert(self$PF)
            self$vars <- c(self$vars, p$get_var_names())
            self$check_assign_subset = FALSE

          } else if(as.name("[") == fct) {

            p <- fastaccess$new(sexp)
            #p <- subset$new(sexp, self$check_assign_subset)
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
