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

J <- function(f, y, x, 
              output = "R", types_of_args = "SEXP", return_type = "SEXP",
              reference = FALSE, verbose = FALSE, getsource = FALSE) {
  
  if(missing(f)) stop("function f is required")
  if(missing(y)) stop("dependent variable y is required")
  if(missing(x)) stop("independent variable x is required")
  
  stopifnot(is.function(f))
  stopifnot(is.logical(verbose))
  stopifnot(is.logical(reference))
  stopifnot(is.character(output))
  stopifnot("found unknown output format" = 
              output %in% c("R", "XPtr"))
  stopifnot(is.character(types_of_args))
  stopifnot(is.character(return_type))
  
  l <- dfdr::fcts()
  
  cmr <- function(a, b, c) 1

  l <- dfdr::fcts_add_fct(l, cmr, cmr, keep = TRUE)
  
  y <- rlang::ensym(y)
  x <- rlang::ensym(x)
  
  jac <- dfdr::jacobian(f, !!y, !!x, derivs = l)
  
  if(verbose) print(jac)

  jac_ret <- translate(jac, verbose = verbose)
  
  return(jac_ret)
}
