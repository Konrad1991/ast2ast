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

# ================================================================================
# function called by user
# ================================================================================
translate <- function(code, types_input_variables) {
  
  # seperate expressions and get AST
  code <- sep_expressions(code)
  code <- extractast(code)
  
  # identify storage variables and variables at rhs
  vars <- storage_variables(code)
  vars_rhs <- variables_at_rhs(code)
  
  # type determination
  code <- type_of_lhs(code, types_input_variables)
  
  # identify all variables
  allvar <- all_vars(code, types_input_variables)

  # simple language object transformation
  types <- types_of_each_line(code, types_input_variables)
  code <- adding_type(code, types)

  # replace R functions
  code <- replace_arrow(code)
  code <- replace_bracket(code)
  code <- replace_bracket2(code)
  code <- replace_c(code)
  code <- replace_colon(code)
  code <- replace_comparison(code)
  
  # convert to calls
  for(i in seq_along(code)) {
    code[[i]] <- get_calls(code[[i]])
    code[[i]] <- deparse(code[[i]])
    l <- length(code[[i]])
    code[[i]][[l]] <- paste(code[[i]][[l]], ";")
    code[[i]] <- extractast(code[[i]])
  }
  
  

  signature <- paste("void fct(")
  for(i in names(types_input_variables) ) {
    index <- match(i, names(types_input_variables))
    temp_i <- paste(i, "SEXP", sep = "")
    if(index < length(names(types_input_variables))) {
      signature <- c(signature, paste("SEXP", temp_i, ",") )  
    } else {
      signature <- c(signature, paste("SEXP", temp_i) )
    }
  }
  signature <- c(signature, ")")
  
  result <- c()
  for(i in seq_along(code) ) {
    result[[i]] <- as.character(code[[i]])
  }
  result <- unlist(result)
  
  dependencies <- c(
  "#include <Rcpp.h>", 
  "#include <tidyCpp>",
  "// [[Rcpp::depends(tidyCpp)]]",
  "// [[Rcpp::depends(ast2ast)]]",
  "#include \"all.hpp\" ",
  "// [[Rcpp::export]]")
  
  file <- file("result.cpp")
  for(i in dependencies) {
    write(i, file = "result.cpp", append = TRUE)
  }
  for(i in signature) {
    write(i, file = "result.cpp", append = TRUE)
  }
  write("{", file = "result.cpp", append = TRUE)
  for(i in allvar) {
    write(i, file = "result.cpp", append = TRUE)
  }
  for(i in result) {
    write(i, file = "result.cpp", append = TRUE)
  }
  write("}", file = "result.cpp", append = TRUE)
  
  close(file)
  
  return()
}