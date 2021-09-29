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
# function to replace [ with subset
# ================================================================================
replace_bracket <- function(code_lines) {
  for(i in seq_along(code_lines)) {
    code <- get_calls(code_lines[[i]])
    temp <- replacer(code, "[", "subset") 
    code_lines[[i]] <- extractast(temp)
  }
  
  return(code_lines)
}


# ================================================================================
# function to replace : with range
# ================================================================================
replace_colon <- function(code_lines) {
  for(i in seq_along(code_lines)) {
    code <- get_calls(code_lines[[i]])
    temp <- replacer(code, ":", "range") 
    code_lines[[i]] <- extractast(temp)
  }
  
  return(code_lines)
}



# ================================================================================
# function to replace for loop pattern i in begin:end --> don't know how to implement.... 
# ================================================================================
replace_for <- function(code_lines) {
  for(i in seq_along(code_lines)) {
    code <- get_calls(code_lines[[i]])
    temp <- replacer(code, "in", ":") 
    code_lines[[i]] <- extractast(temp)
  }
  return(code_lines)
}
