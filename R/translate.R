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


#' Translates an R function into a C++ function. 
#' 
#' An R function is translated to C++ source code and afterwards compiled. \cr
#'     The result can be an external pointer (XPtr) or an R function. \cr 
#'     The default value is an R function. \cr
#'     Further information can be found in the vignette: 'Detailed Documentation'. \cr
#' @param f The function which should be translated from R to C++.
#' @param output If set to "R" an R function wrapping the C++ code is returned. \cr
#'               If output is set to "XPtr" an external pointer object pointing to the C++ code is returned. \cr
#'               The default value is "R". 
#'                \strong{However, the performance of the R function is currently suboptimal}.
#' @param types_of_args define the types of the arguments passed to the function as an character vector. This is an optional input. \cr
#'               The default value is 'SEXP' as this works well together with the argument 'R' output. \cr
#'               In case one want to use an external pointer the easiest way is to pass 'sexp' for types_of_args. \cr
#'               The possible values are: \cr
#'               \itemize{
#'                    \item void
#'                    \item double
#'                    \item SEXP
#'                    \item sexp
#'                    \item Rcpp::NumericVector
#'                    \item NumericVector
#'                    \item Rcpp::NumericMatrix
#'                    \item NumericMatrix
#'                    \item arma::vec
#'                    \item vec
#'                    \item arma::mat
#'                    \item mat
#'               }
#' @param return_type is a character defining the type which the function can return. The default value is 'SEXP' as this works well with the 'R' output. \cr
#'                    The same types as for the types_of_args are possible.
#' @param reference If set to TRUE the arguments are passed by reference (not possible if output should be an R function).
#' @param verbose If set to TRUE the output of RcppXPtrUtils::cppXPtr or Rcpp::cppFunction is printed.
#' @return The external pointer of the generated C++ function or an R function is created. 
#' @details \strong{The following types are supported: }
#'  \enumerate{
#'    \item numeric vectors
#'    \item numeric matrices
#'  }
#'  Variables can be either numeric vectors or matrices.
#'  Notably, it is possible that the variables change the type within the function. \cr
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
#'    \item mathematical functions: sin, asin, sinh, cos, acos, cosh, tan, atan, tanh, sqrt, log, ^ and exp
#'    \item concatenate objects: c
#'    \item control flow: for, if, else if, else
#'    \item comparison: ==, !=, >, <, >= and <=
#'    \item printing: print
#'    \item returning objects: return
#'    \item catmull-rome spline: cmr
#'    \item to get a range of numbers the ':' function can be used
#'    \item is.na and is.infinite can be used to test for NA and Inf.
#'  }
#' @details  \strong{Some details about the implemented functions}
#' @details  \itemize{
#'    \item allocation of memory works: Following forms are possible: vector(size_of_elements), vector(value, size_of_elements), 
#'              matrix(nrows, ncols), matrix(value, nrows, ncols) and matrix(vector, nrows, ncols). The latter fills the matrix or the vector with the specified 'value'. \cr
#'    \item For indices squared brackets '[]' can be used as common in R. Beyond that the function 'at' exists
#'              which accepts as first argument a variable and as the second argument you pass the desired index.
#'              The caveat of using 'at' is that only one entry can be accessed. The function '[]' can return more then one element. \cr
#'              \strong{The 'at'function returns a reference to the vector entry.
#'                Therefore variable[index] can behave differently then at(variable, index).
#'                The function has to be used carefully when 'at' is used.
#'                Especially if '[]' and 'at' are mixed the function behaviour is difficult to predict.
#'                Please test it before using it in a serious project.}  \cr
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
#'  } \cr
#' \strong{Be aware that the R code is translated to ETR an expression template library which tries to mimic R. \cr
#' However, it does not behave exactly like R! Please check your compiled function before using it in a serious project.
#' If you want to see how ast2ast differs from R in detail check the vignette: 'Detailed Documentation'.}
#' @examples #Further examples can be found in the vignette: 'Detailed Documentation'.
#' @examples #Hello World
#' \dontrun{
#' # Hello World
#' # Translating to R_fct
#' f <- function() { print("Hello World!")}
#' ast2ast::translate(f)
#' f()
#' 
#' # Translating to external pointer
#' f <- function() { print("Hello World!")}
#' pointer_to_f_cpp <- ast2ast::translate(f, output = "XPtr")
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
#' 
#' 
#' # Run sum example:
#' run_sum <- function(x, n) {
#' sz <- length(x)
#'
#' ov <- vector(mode = "numeric", length = sz)
#'
#' ov[n] <- sum(x[1:n])
#' for(i in (n+1):sz) {
#'
#' ov[i] <- ov[i-1] + x[i] - x[i-n]
#' }
#'
#' ov[1:(n-1)] <- NA
#'
#' return(ov)
#' }
#' 
#' run_sum_fast <- function(x, n) {
#' sz <- length(x)
#' ov <- vector(sz)
#' 
#' sum_db = 0
#' for(i in 1:n) {
#'   sum_db <- sum_db + at(x, i)
#' }
#' ov[n] <- sum_db
#' 
#' for(i in (n + 1):sz) {
#'   ov[i] <- at(ov, i - 1) + at(x, i) - at(x, i - at(n, 1))
#' }
#' 
#' ov[1:(n - 1)] <- NA
#' 
#' return(ov)
#' }
#' run_sum_cpp <- ast2ast::translate(run_sum_fast, verbose = FALSE)
#' set.seed(42)
#' x <- rnorm(10000)
#' n <- 500
#' one <- run_sum(x, n)
#' two <- run_sum_cpp(x, n)
#' }
translate <- function(f, output = "R", 
                      types_of_args = "SEXP", return_type = "SEXP",
                      reference = FALSE, verbose = FALSE) {

    if(missing(f)) stop("function f is required")

    stopifnot(is.function(f))
    stopifnot(is.logical(verbose))
    stopifnot(is.logical(reference))
    stopifnot(is.character(output))
    stopifnot("found unknown output format" = 
                output %in% c("R", "XPtr"))
    stopifnot(is.character(types_of_args))
    stopifnot(is.character(return_type))
    stopifnot("found unknown type of arguments for functions" = 
              types_of_args %in% c("void", "double", "SEXP", "sexp", 
                                   "Rcpp::NumericVector", "Rcpp::NumericMatrix",
                                   "arma::vec", "arma::mat",
                                   "NumericVector", "NumericMatrix",
                                   "vec", "mat") )
    stopifnot("found unknown return type" = 
                return_type %in% c("void", "double", "SEXP", "sexp", 
                                     "Rcpp::NumericVector", "Rcpp::NumericMatrix",
                                     "arma::vec", "arma::mat",
                                     "NumericVector", "NumericMatrix",
                                     "vec", "mat") )
    
    
    stopifnot(length(return_type) == 1)
    
    if( length(formalArgs(f)) != length(types_of_args) ) {
      largs <- length(formalArgs(f))
      ltypes <- length(types_of_args)
      if( (largs %% ltypes) != 0) {
        stop(paste("In length(arguments of function) %% length(types_of_args):
                      longer object length is not a multiple of shorter object length") )
      } else if((largs %% ltypes) == 0) {
        types_of_args <- rep(types_of_args, largs/ltypes)   
      }
    }
    
    
    R_fct = NULL
    if(output == "R") {
      R_fct = TRUE
    } else if(output == "XPtr") {
      R_fct = FALSE
    }
  
    if(R_fct == TRUE) {
      if(reference == TRUE) {
        warning("The desired output is an R function.
                Therefore reference cannot be set to TRUE.
                The argument reference will be ignored!")
      }
      reference = TRUE
    }
    
    fct_ret <- compiler_a2a(f, verbose, reference,
                                     R_fct, types_of_args, return_type)

    return(fct_ret)
}
