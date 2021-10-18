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
  assign_lines <- get_assignments(code_lines)
  for(i in seq_along(code_lines)) {
    code <- get_calls(code_lines[[i]])
    temp <- replacer(code, "[", "subset")   
    code_lines[[i]] <- extractast(temp)
  }
  
  return(code_lines)
}



# ================================================================================
# function to replace subset with subset_self
# ================================================================================
replace_bracket2 <- function(code_lines) {
  assign_lines <- get_assignments(code_lines)
  for(i in seq_along(code_lines)) {
    if( code_lines[[i]][[1]] == as.name("=") &&
       (code_lines[[i]][[2]] == "subset")) {
      
      code <- get_calls(code_lines[[i]][[2]])
      temp <- replacer(code, "subset", "subset_self")   
      code_lines[[i]][[2]] <- extractast(temp)
      
      # hack in order to enable subset_self = vector
      if(length(code_lines[[i]][[3]]) >= 3) {
        temp <- list()
        temp[[1]] <- as.name("+")
        temp[[2]] <- code_lines[[i]][[3]]
        temp[[3]] <- 2.225074e-308
        code_lines[[i]][[3]] <- extractast(temp)        
      }

    }
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
# function to replace c with concatenate
# ================================================================================
replace_c <- function(code_lines) {
  for(i in seq_along(code_lines)) {
    code <- get_calls(code_lines[[i]])
    temp <- replacer(code, "c", "concatenate") 
    code_lines[[i]] <- extractast(temp)
  }
  
  return(code_lines)
}


# ================================================================================
# function to replace <- with =
# ================================================================================
replace_arrow <- function(code_lines) {
  for(i in seq_along(code_lines)) {
    code <- get_calls(code_lines[[i]])
    temp <- replacer(code, "<-", "=") 
    code_lines[[i]] <- extractast(temp)
  }
  
  return(code_lines)
}


# ================================================================================
# function to replace <, <=, >, >=, ==, != with s, se, l, le, e, ne
# ================================================================================
replace_comparison <- function(code_lines) {
  for(i in seq_along(code_lines)) {
    code <- get_calls(code_lines[[i]])
    temp <- replacer(code, "<", "s")
    temp <- replacer(temp, "<=", "se")
    temp <- replacer(temp, ">", "l")
    temp <- replacer(temp, ">=", "le")
    temp <- replacer(temp, "==", "e")
    temp <- replacer(temp, "!=", "ne")
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
