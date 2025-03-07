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


#' $importFrom Rcpp sourceCpp
compiler_a2a <- function(fct_code, R_fct,
                         verbose, name_f, calc_deriv) {
  fct <- fct_code
  fct_ret <- NULL

  if (calc_deriv) {
    Sys.setenv("PKG_CXXFLAGS" = "-DDERIV_ETR")
  }

  if (!R_fct) {
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
        color_print(43, "Sorry compilation failed!")
      }
    )
  } else {
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
        color_print(43, "Sorry compilation failed!")
      }
    )

    Sys.unsetenv("PKG_CXXFLAGS")

    if (verbose == TRUE) {
      print_with_line_numbers(fct)
    }
  }

  return(fct_ret)
}
