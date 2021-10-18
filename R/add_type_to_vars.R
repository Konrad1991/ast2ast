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
# function to unfold AST
# ================================================================================
get_calls <- function(code) {
  out <- purrr::map_if(code, is.list, get_calls)
  out <- as.call(out)
  return(out)
}


# ================================================================================
# function to replace term
# ================================================================================
replacer <- function(code, pattern, new) {
  old <- function(x) {
    x == as.name(pattern)
  }

  new_code <- function(x) {
    return(as.name(new))
  }

  code <- rrapply::rrapply(code, condition = old, f = new_code, how = "replace")
  return(code)
}


# ================================================================================
# function to update variable list
# ================================================================================
types_of_each_line <- function(code_lines, start_variables_types) {
  vars_rhs <- variables_at_rhs(code_lines)
  
  assign_lines <- get_assignments(code_lines)
  vars_at_rhs <- variables_at_rhs(code_lines)
  vars_at_lhs <- storage_variables(code_lines)
  
  types <- rep("num", length(assign_lines[assign_lines == TRUE]) ) 
  
  # for each line add new type list
  res <- list()

  # loop over assign lines
  for(i in seq_along(code_lines)) {
    
    
    # determine type due to vars at rhs
    if( (assign_lines[i] == TRUE) ) {
      indices <- match(vars_at_rhs[[i]], names(start_variables_types))
      indices <- indices[!is.na(indices)]
      for(j in indices) {
        if(start_variables_types[[j]] == "num_mat") {
          types[i] <- "num_mat"
          break
        } else if( (start_variables_types[[j]] == "num_vec") ) {
          types[i] <- "num_vec"
        }
      }
      
      # check if var at lhs is already known
      if(element_exists(vars_at_lhs[[i]], start_variables_types) == TRUE) {
        
        # var at lhs is already known but changes type
        if(start_variables_types[[vars_at_lhs[[i]] ]] != types[i]) {
          start_variables_types[[vars_at_lhs[[i]] ]] = types[i]
          
          new_type = 0 # num
          if(types[i] == "num_vec") {
            new_type = 1
          } else if(types[i] == "num_mat") {
            new_type = 2
          }
          
        }
      } else { # found a new variable and added it to the list
        
        if(is.na(types[i])) {
          type_rhs <-unlist(code_lines[[i]][3])[[1]]
          if(type_rhs == "vec" | type_rhs == "c") {
            types[i] <- "num_vec" 
          } else if(type_rhs == "mat") {
            types[i] <- "num_mat" 
          } else if(is.numeric(type_rhs)) {
            types[i] <- "num" 
          } else {
            stop("Type deduction failed")
          }
        }
        
        start_variables_types <- append(start_variables_types, types[i]) 
        names(start_variables_types)[length(start_variables_types)] <- vars_at_lhs[[i]]
      }
    }
    
    res[[i]] <- start_variables_types
    
  }
  
  return(res)
}



# ================================================================================
# function to add type information e.g. y.mat or y.vec or y.num
# ================================================================================
adding_type <- function(code_lines, variables_types) {

  # loop over assign lines
  for(i in seq_along(code_lines)) {
      names <- names(variables_types[[i]])
      types <- variables_types[[i]]
      for(j in seq_along(names)) {
        new <- paste(names[[j]], types[[j]], sep = ".")

        if(length(new) > 0) {
          code <- get_calls(code_lines[[i]])
          temp <- replacer(code, names[[j]], new) 
          code_lines[[i]] <- extractast(temp)
        }
      }
  }
  
  return(code_lines)
}
