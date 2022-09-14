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
              types_of_args %in% c("SEXP", "sexp",
                                   "ptr_vec", "ptr_mat") )
    stopifnot("found unknown return type" = 
                return_type %in% c("void", "SEXP", "sexp") )
    
    
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
    
    name_f <- as.character(substitute(f))
    
    # if output == "R" --> type has to be SEXP!
    if(R_fct) {
      if(!all(types_of_args == "SEXP")) {
        warning("If using 'R' as output form only SEXP is valid as type.
                All other types will be ignored!")
        types_of_args <- rep("SEXP", length(formalArgs(f)) )
      }
      
      if( (return_type != "SEXP") ) {
        warning("If using 'R' as output form only SEXP or void are valid as return_type.
                All other types will be ignored!")
        return_type <- "SEXP"
      }
    }
  
    if(R_fct == TRUE) {
      if(reference == TRUE) {
        warning("The desired output is an R function.
                Therefore reference cannot be set to TRUE.
                The argument reference will be ignored!")
      }
      reference = FALSE
    }
    
    fct_ret <- compiler_a2a(f, verbose, reference,
                                     R_fct, types_of_args, return_type, name_f)

    return(fct_ret)
}
