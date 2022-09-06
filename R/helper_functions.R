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



compiler_a2a <- function(f, verbose, reference, R_fct, desired_type, return_type) {
  a = NULL
  fct = NULL
  fct_ret = NULL
  name_f <- as.character(substitute(f))
  
  if(R_fct == FALSE) {
    a = MA$new(f, desired_type, name_f, R_fct, return_type)
    fct <- a$build_own(verbose, reference = reference) # build
    cat(fct)  
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
  } else {
    a = MA$new(f, desired_type, name_f, R_fct, return_type)
    fct <- a$build_own_SEXP(verbose, reference = reference) # build_SEXP
    
    fct <- paste(
      '#include "etr.hpp"', "\n",
      fct
    )
    
    res <- NULL
    Sys.setenv("PKG_CXXFLAGS" = "-DRFCT") # remove warnings -Wall -Wpedantic!!!!!!!!!!
    options(warn = -1)
    tryCatch(
      expr = {
        res <- Rcpp::sourceCpp(code = fct, verbose = verbose) 
      },
      error = function(e) {
        print("Sorry compilation failed!")
      }
    )
    
    Sys.unsetenv("PKG_CXXFLAGS") # is this correct?
    options(warn = 0)
    
    if(verbose == TRUE) {
      cat(fct)
    }
    
    args_f <- methods::formalArgs(f)
    check <- length(args_f)
    args_f <- paste(args_f, collapse = ",")
    
    res <- res$buildDirectory
    
    file <- NULL
    if( (Sys.info()['sysname'] == "Linux") || (Sys.info()['sysname'] == 'macOS') ) {
      file <- list.files(res, pattern = "\\.so$") # is it necessary to check also sl?
    } else if(Sys.info()['sysname'] == "Windows") {
      file <- list.files(res, pattern = "\\.dll$")
    } else {
      stop("unsupported operating system found")
    }
    if(length(file) == 1) { 
      stopifnot("find more then one shared object. Something went wrong. Sorry!"=length(file) == 1)
      dyn.load(paste0(res, "/", file), local = FALSE, now = TRUE)
      if(check > 0) {
        fct_ <- paste(name_f, "<- function(", args_f, ") { .Call(", quote(name_f), ",", args_f, ")}")  
      } else {
        fct_ <- paste(name_f, "<- function(", args_f, ") { .Call(", quote(name_f), ")}")
      }
      fct_ret <- eval(parse(text = fct_))
      return(fct_ret)
    }
    
    if(length(file) == 0) {
      stop("find no shared object. Something went wrong. Sorry!")
    }
    
  }
  
  return(fct_ret)
}