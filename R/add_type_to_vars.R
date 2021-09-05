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

modify_if(t, t == "a", ~"b")



t <- list("a", "b", "c")
modify_if(t, t == "a", ~"b")

t <- str2lang(c("y <- y + x + 1"))
modify_if(as.list(t)[[3]], t == "y", ~"b")

replacer <- function(code) {
  out <- purrr::map_if(code, is.list,  replacer)
  out <- modify_if(out, out == "y", ~"b")
  return(out)
}

trash <- replacer(t)
trash

# ================================================================================
# function to update variable list
# ================================================================================
type_tracking <- function(code_lines, start_variables_types) {
  
  vars_rhs <- variables_at_rhs(code_lines)
  
  assign_lines <- get_assignments(code_lines)
  vars_at_rhs <- variables_at_rhs(code_lines)
  vars_at_lhs <- storage_variables(code_lines)
  
  types <- rep("num", length(assign_lines[assign_lines == TRUE]) ) 
  

  # loop over assign lines
  for(i in seq_along(assign_lines)) {
    
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
          new_type = 0 # num
          if(types[i] == "num_vec") {
            new_type = 1
          } else if(types[i] == "num_mat") {
            new_type = 2
          } 
          
          # change the list
          index_var <- match(vars_at_lhs[[i]], names(start_variables_types))
          start_variables_types[index_var] <- new_type
          start_variables_types[[vars_at_lhs[[i]] ]] = types[i]
        }
        
      } else { # found a new variable and added it to the list
        start_variables_types <- start_variables_types[[1]]
        names(start_variables_types)[length(start_variables_types)] <- vars_at_lhs[[i]]
      }
      
    }
    
    
    # using updated variable list in order to call correct members
    current_var_types <- start_variables_types
    current_line <- code_lines[[i]]
    for(k in seq_along(current_var_types) ) {
      
      if(current_var_types[[k]] == "num_mat") {
        temp <- paste(names(current_var_types[[k]]), ".MAT", sep = "")
        current_line <- replacer(current_line)
        #current_line <- purrr::modify_if(current_line, current_line == current_var_types[[k]], ~temp )
      }
      
    }
  }
  
  return(code_lines)
}



# ================================================================================
# function to add type information e.g. y.mat or y.vec or y.num
# ================================================================================

