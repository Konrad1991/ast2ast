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
# check each line of code for assignment
# ================================================================================
get_assignments <- function(code_lines) {
  lines <- vector(mode = "logical", length = length(code_lines))
  
  for(i in seq_along(code_lines)) {
    lines[i] <- is_assign(as.character(code_lines[[i]]))
  }
  
  return(lines)
}


# ================================================================================
# identify storage_variables
# ================================================================================
storage_variables <- function(code_lines) {
  
  lines <- get_assignments(code_lines)
  stors <- vector(mode = "character", length = length(code_lines))
  for(i in seq_along(lines)) {
    if(lines[i] == TRUE) {
      stors[i] <- stor_var(as.character(code_lines[[i]]))
    } else {
      stors[i] <- "NA"
    }
  }
  
  return(stors)
}

# ================================================================================
# is storage_variable a new variable
# ================================================================================
new_ <- function(var_list, code_lines) {
  lhs_vars <- storage_variables(code_lines)
  
  ret <- vector(mode = "character", length = length(lhs_vars))
  
  for(i in seq_along(lhs_vars)) {
    if(is.null(var_list[[lhs_vars[i] ]])) {
      ret[i] <- "new"
    } else {
      ret[i] <- "old"
    }
  }

  return(ret)
}

# ================================================================================
# variables at rhs
# ================================================================================
variables_at_rhs <- function(code_lines) {
  stors <- list()
  lines <- get_assignments(code_lines)
  
  for(i in seq_along(lines)) {
    if(lines[i] == TRUE) {
      stors[[i]] <- find_var_at_rhs(as.character(unlist(code_lines[[i]][3])))
    } else {
      stors[[i]] <- "NA"
    }
  }
  
  return(stors)
}



# ================================================================================
# function to determine if an element exists in list
# ================================================================================
element_exists <- function(elem, l) {
  for(i in names(l) ) {
    if(elem == i) {
      return(TRUE)
    }
  }
  return(FALSE)
}

# ================================================================================
# function to determine type of lhs
# ================================================================================
type_of_lhs <- function(code_lines, start_variables_types) {
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
        start_variables_types[[vars_at_lhs[[i]] ]] = types[i]
        
        new_type = 0 # num
        if(types[i] == "num_vec") {
          new_type = 1
        } else if(types[i] == "num_mat") {
          new_type = 2
        }
        
        index_var <- match(vars_at_lhs[[i]], names(start_variables_types))
        
        # change the code!
        code_lines[i:(length(code_lines) + 1)] <- c(
          #paste("switch_type(", start_variables_types[[vars_at_lhs[[i]] ]], ", ", new_type, ")", sep = ""), 
          paste("switch_type(", names(start_variables_types)[index_var], ", ", new_type, ")", sep = ""), 
          code_lines[i:(length(code_lines))])
      }
    } else { # found a new variable and added it to the list
      start_variables_types <- append(start_variables_types, types[i]) 
      names(start_variables_types)[length(start_variables_types)] <- vars_at_lhs[[i]]
    }
      
    }
  }
  
  return(code_lines)
}


