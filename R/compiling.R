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



compiler_a2a <- function(f, verbose, reference, R_fct, desired_type, return_type, name_f, getsource) {
  a = NULL
  fct = NULL
  fct_ret = NULL

  if(R_fct == FALSE) {
    a = MA$new(f, desired_type, name_f, R_fct, return_type)
    fct <- a$build_own(verbose, reference = reference) # build
      
    if(getsource == TRUE) return(fct)
    
    tryCatch(
      expr = {
        if(verbose == TRUE) {
          cat(fct)
        }
        env <- new.env()
        Rcpp::sourceCpp(code = fct, verbose = verbose, env = env) 
        fct_ret <- env$getXPtr()
        attributes(fct_ret) <- list(class = "XPtr")
      },
      error = function(e) {
        print("Sorry compilation failed!")
      }
    )
  } else {
    a = MA$new(f, desired_type, name_f, R_fct, return_type)
    fct <- a$build_own_SEXP(verbose, reference = reference) 
    
    if(getsource == TRUE) return(fct)
    
    res <- NULL
    Sys.setenv("PKG_CXXFLAGS" = "-DRFCT") # remove warnings -Wall -Wpedantic!!!!!!!!!!
    #options(warn = -1)
    tryCatch(
      expr = {
        env <- new.env()
        res <- Rcpp::sourceCpp(code = fct, verbose = verbose, env = env)
        fct_ret <- env[[name_f]]
      },
      error = function(e) {
        print("Sorry compilation failed!")
      }
    )
    
    Sys.unsetenv("PKG_CXXFLAGS") # is this correct?
    #options(warn = 0)
    
    if(verbose == TRUE) {
      cat(fct)
    }
    
  }
  
  return(fct_ret)
}