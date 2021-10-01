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
  code <- type_of_lhs(code, input_variables)
  
  # identify all variables
  allvar <- all_vars(code, input_variables)

  # simple language object transformation
  types <- types_of_each_line(code, input_variables)
  code <- adding_type(code, types)

  # replace R functions
  code <- replace_bracket(code)
  code <- replace_colon(code)
  code <- replace_arrow(code)
  code <- replace_comparison(code)
  
  # convert to calls
  for(i in seq_along(code)) {
    code[[i]] <- get_calls(code[[i]])
    #code[[i]] <- deparse(code[[i]])
    #code[[i]] <- paste(code[[i]], ";")
    #code[[i]] <- extractast(code[[i]])
  }
  
  result <- list(allvar, code)
  return(result)
}