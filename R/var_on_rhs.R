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
LHS_and_RHS <- R6::R6Class("LHS_and_RHS",
  public = list(
    lhs = NULL,
    rhs = NULL,
    lhs_subsetted = FALSE,
    variable_to_which_assign = NULL,
    vars_from_rhs = NULL,
    on_both_sides = FALSE,
    initialize = function(l, r) {
      self$lhs <- l
      self$rhs <- r
      self$is_subsetted()
      self$variable_to_which_assign <- self$extract_var()
      self$vars_from_rhs <- unlist(all.vars(self$rhs))
      self$check()
    },
    is_subsetted = function() {
      if (!is.name(self$lhs) & is.call(self$lhs)) {
        self$lhs_subsetted <- TRUE
      }
    },
    extract_var = function() {
      res <- NULL
      if (self$lhs_subsetted) {
        res <- as.list(self$lhs)[[2]]
      } else {
        res <- self$lhs
      }
      return(deparse(res))
    },
    check = function() {
      self$on_both_sides <- self$variable_to_which_assign %in% self$vars_from_rhs
    }
  )
)

var_on_rhs <- function(a, env) {
  if (!is.call(a)) {
    return(a)
  }

  fct <- a[[1]]
  if ((as.name("<-") == fct) || (as.name("=") == fct)) {
    env$class <- LHS_and_RHS$new(a[[2]], a[[3]])
  }

  a <- as.list(a)
  lapply(a, function(x) {
    var_on_rhs(x, env)
  })
}

lhs_found_on_rhs <- function(expression) {
  env <- new.env()
  env$class <- NULL
  ast <- var_on_rhs(expression, env)
  print(env$class$on_both_sides)
}

expressions <- list(
  one = quote(y[a[b[3]]] <- x + 3),
  two = quote(y <- x + 3),
  three = quote(y <- x * y + y),
  four = quote(y[a[y[2]]] <- x * y + 3)
)

# trash <- lapply(expressions, lhs_found_on_rhs)
