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

      public = list(

       PF = c("=", "<-", "[", "for", "c", ":", "sin", "asin", "sinh", "cos", "acos", "cosh",
        "tan", "atan", "tanh", "log", "^", "+", "-",
         "*", "/", "if", "else if", "else", "{", "(",
        "==", "!=", ">", ">=", "<", "<=", "print", "return", "vector", "matrix", "length", "dim"), # permitted functions

       vars = list(), # variables

       ast = NULL,
       code = NULL,
       generic_fct = c("+", "-",
        "*", "/", "if", "else if", "else", "{", "(",
       "==", "!=", ">", ">=", "<", "<=", "print", "for", "return", "vector", "matrix","length", "dim"),

       math = c("sin", "asin", "sinh", "cos", "acos", "cosh",
        "tan", "atan", "tanh", "log", "^"),

       check_assign_subset = NULL,
       found_return = FALSE,

       extractast = function(sexp) {

          if(!is.call(sexp)) {
            # print(sexp)
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

          } else if(deparse(fct) %in% self$generic_fct){

            self$check_assign_subset = FALSE
            p <- generic$new(sexp)
            sexp <- p$convert(self$PF)
            self$vars <- c(self$vars, p$get_var_names())

            if(fct == as.name('return')) {
              self$found_return = TRUE
            }

          } else if(as.name("[") == fct) {

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

          } else {
            cat("Error: Sorry not all  functions are suported", "\n")
            cat("Function: ", fct, " not supported")
            stop()
          }

          # convert node classes
          lapply(sexp, self$extractast)

       },

       initialize = function(line) {
         self$ast <- self$extractast(line) # fill nodes
       },

       extractast_for = function(sexp) {
          if(!is.call(sexp)) {
            #print(sexp)
            return(sexp)
          }

          sexp <- as.list(sexp)

          # assign to node classes
          fct <- sexp[[1]]

          if(as.name("for") == fct){
            p <- forloop$new(sexp)
            sexp <- p$convert(self$PF)
            self$vars <- c(self$vars, p$get_var_names())
          }

          # convert node classes
          lapply(sexp, self$extractast_for)

       },


       get_calls = function(code) {
         out <- purrr::map_if(code, is.list, self$get_calls)
         out <- as.call(out)
         return(out)
       },

       ast2call = function() {
         self$code <- self$get_calls(self$ast)
         self$ast <- self$extractast_for(self$code)
         self$code <- self$get_calls(self$ast)
       },

       get_vars = function() {
         return(unique(self$vars))
       }


      )

) # end class





#' Translates a R function into a Rcpp function
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
translate = function(f, desired_type = "SEXP") {

  # extract infos from fct f
  name <- as.character(substitute(f))
  arguments <- formalArgs(f)
  body <- body(f)
  body <- body[2:length(body) ]
  lc <- list()
  vars <- list()
  code <- list()
  return_TF <- list()

  # convert R to ETR
  for(i in seq_along(body)) {
    lc[[i]] <-LC$new(body[[i]])
    lc[[i]]$ast2call()
    temp <- lc[[i]]$code
    temp <- deparse(temp)

    looping <- function(fl) {
      fl <- as.character(fl)
      sz <- nchar(fl)
      fl_ <- gsub('for_', '', fl)
      sz_ <- nchar(fl_)

      if(fl != fl_) {
        fl <- gsub('.{2}$', '', fl) # remove {(
        fl <- paste(fl, '{', collapse = '')
      }

      fl <- gsub('`', '', fl)

      return(fl)
    }

    for(j in seq_along(temp)) {
        temp[[j]] <- looping(temp[[j]])
        temp[[j]] <- paste(temp[[j]], ";")
    }
    code[[i]] <- temp
    vars[[i]] <- lc[[i]]$get_vars()
    return_TF[[i]] <- lc[[i]]$found_return
  }

  # check used variables
  vars <- unique(vars)
  return_TF <- any(TRUE %in% return_TF)


  # determine return type
  ret_type <- as.symbol("void")
  if(return_TF == TRUE) {
      ret_type <- as.symbol("SEXP")
  }

  # arguments passed to f & define signature
  arguments_string <- sapply(arguments, function(x) {
    y <- paste(x, "SEXP", sep = "")
    temp <- paste(desired_type, y, ',', collapse = '')

    if (parent.frame()$i[] == length(arguments) )  {
      temp <- paste(desired_type, y, collapse = '')
    }

    return(temp)
  })
  arguments_string <- paste(arguments_string, collapse = " ")
  signature <- paste(ret_type, name, '(', arguments_string, ')', '{', collapse = " ")

  # define variable declaration & variable initialization
  args_dec <- sapply(arguments, function(x) {
    inp <- paste(x, "SEXP", sep = "")

    temp <- paste("sexp", x, '=', inp, ';', collapse = '')
    return(temp)
  })
  args_dec <- paste(args_dec, collapse = " ")

  # variable declaration of remaining variables
  vars_ <- as.character(unlist(vars))
  vars_ <- unique(vars_)
  if(length(arguments) >= 1) {
      vars_ <- suppressWarnings(vars_[vars_ != arguments]) # if objects passed to function but not used --> suppressWarnings
  }

  vars_ <- vars_[!is.na(vars_)]


  vars_ <- sapply(vars_, function(x) {
    temp <- paste("sexp", x, ";", collapse = '')
  })
  vars_ <- paste(vars_, collapse = " ")

  # put everything together
  fct <- c(signature, args_dec, vars_, code, '}')
  fct <- sapply(fct,  paste, collapse = "\n")
  fct <- paste(fct, collapse = "")

  # compile fct f
  fct_ret <- NULL
  #tryCatch(
  #  fct_ret = Rcpp::cppFunction(code = fct, plugins = c("cpp17"), depends = c("ast2ast"), includes = "#include <etr.hpp>" ),
  # cat("Sorry: compilation failed")
 #)
 fct_ret = Rcpp::cppFunction(code = fct, plugins = c("cpp17"), depends = c("ast2ast"), includes = "#include <etr.hpp>" )

  return(fct_ret)
}
