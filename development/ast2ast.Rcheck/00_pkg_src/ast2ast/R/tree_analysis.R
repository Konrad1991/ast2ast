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

is_variable <- function(node, var_list) {
  node <- c(deparse(node))
  node %in% var_list
}

which_variable <- function(node, var_list) {
  node <- c(deparse(node))
  which(node == var_list) - 1
}

fill_association <- function(node, env) {
  if (is_variable(node, env$var_list)) {
    env$idx_vars_found <- c(env$idx_vars_found, env$counter)
    env$counter <- env$counter + 1
    which_var <- which_variable(node, env$var_list)
    env$which_vars_found <- c(env$which_vars_found, which_var)
  } else {
    env$which_vars_found <- c(env$which_vars_found, -1)
  }
}

get_ast <- function(a, env) {
  if (!is.call(a)) {
    return(a)
  }

  if (length(a) == 3) { # binary operation
    fill_association(a[[2]], env)
    fill_association(a[[3]], env)
    env$num_args <- c(env$num_args, 2)
  } else if (length(a) == 2) { # unary operation
    fill_association(a[[2]], env)
    env$num_args <- c(env$num_args, 1)
  }

  a <- as.list(a)
  lapply(a, function(x) {
    get_ast(x, env)
  })
}

tree_analyis <- function(expression) {
  vars <- all.vars(expression)
  env <- new.env()
  env$counter <- 0
  env$idx_vars_found <- c()
  env$which_vars_found <- c()
  env$var_list <- vars
  env$num_args <- c()
  ast <- get_ast(expression, env)
  env$which_vars_found
  env$num_args

  code <- "
  // [[Rcpp::plugins(cpp2a)]]
  // [[Rcpp::depends(RcppArmadillo)]]
  // [[Rcpp::depends(ast2ast)]]
  #include \"ad_etr.hpp\"

  std::vector<double> fct() {
    using namespace etr;
    sexp x(2);
    sexp y(2);
    sexp z(2);
    sexp f(2);

    x[0] = 2.0;
    x[1] = 3.0;
    y[0] = 4.0;
    y[1] = 5.0;
    z[0] = 6.0;
    z[1] = 7.0;

    to_replace

    return res;
  }

  // [[Rcpp::export]]
  std::vector<double> f() {
    std::vector<double> res = fct();
    return res;
  }
  "

  create_call <- function(vars, which_vars_found, unary_or_binary) {
    v <- sapply(vars, function(x) {
      paste0("&", x, ".d")
    })
    v <- paste("std::array<const variable,", length(vars), ">{", paste(v, collapse = ","), "},")
    wvf <- paste("std::array<int,", length(which_vars_found), ">{", paste(which_vars_found, collapse = ","), "},")
    uob <- paste("std::array<int,", length(unary_or_binary), ">{", paste(unary_or_binary, collapse = ","), "});")
    res <- paste(
      "std::vector<double> res = assign(f,",
      deparse(expression), ",",
      v,
      wvf,
      uob,
      collapse = "\n"
    )
    return(res)
  }

  res <- create_call(env$var_list, env$which_vars_found, env$num_args)
  gsub("to_replace", res, code)
}


create_vals <- function(expression) {
  vars <- all.vars(expression)
  env <- new.env()
  env$counter <- 0
  env$idx_vars_found <- c()
  env$which_vars_found <- c()
  env$var_list <- vars
  env$num_args <- c()
  ast <- get_ast(expression, env)
  env$which_vars_found
  env$num_args
  return(list(env$var_list, env$which_vars_found, env$num_args))
}

# create_vals(quote(3.14 + x * x))
