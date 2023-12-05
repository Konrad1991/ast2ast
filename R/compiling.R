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


#' @importFrom Rcpp sourceCpp
compiler_a2a <- function(f, verbose, reference, R_fct, desired_type, return_type, name_f, getsource) {
  a <- NULL
  fct <- NULL
  fct_ret <- NULL

  if (R_fct == FALSE) {
    fct <- buildFctXPtr(f, reference, desired_type, return_type, name_f) # issue: reference == borrow has to be documented
    if (getsource == TRUE) {
      return(fct)
    }

    tryCatch(
      expr = {
        if (verbose == TRUE) {
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
    fct <- buildFctR(f, name_f)
    if (getsource == TRUE) {
      return(fct)
    }

    res <- NULL
    # -fsanitize=alignment,bool,bounds,builtin,enum,integer-divide-by-zero,
    # nonnull-attribute,null,object-size,pointer-overflow,return,
    # returns-nonnull-attribute,shift,signed-integer-overflow,
    # unreachable,vla-bound,vptr
    # Sys.setenv("PKG_CXXFLAGS" = "-DRFCT -Wall -Wpedantic")
    # -fsanitize=address --param=max-vartrack-size=9000000 -fno-omit-frame-pointer

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

    Sys.unsetenv("PKG_CXXFLAGS")

    if (verbose == TRUE) {
      cat(fct)
    }
  }

  return(fct_ret)
}
