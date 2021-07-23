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
# types variables at rhs
# ================================================================================
deduce_type <- function(var_types) {
  mats <- sum(var_types == "mat_num", na.rm = TRUE)
  vecs <- sum(var_types == "vec_num", na.rm = TRUE)
  
  if(mats > 0) {
    return("mat_num")
  } else if( (mats == 0) && (vecs > 0)) {
    return("vec_num")
  } else {
    return("num")
  }
}

get_type_from_list <- function(var_names, var_list) {
  res <- vector(mode = "character", length = length(var_names))
  
  for(i in seq_along(var_names)) {
    if(var_names[i] != "NA") {
      res[i] <- var_list[[var_names[i] ]]
    } else {
      res[i] <- "NA"
    }
  }  
  
  return(res)
}



types <- function(var_list, code_lines) {
  relevant_lines <- get_assignments(code_lines)
  vars_rhs <- variables_at_rhs(code_lines)
  
  res <- vector(mode = "character", length = length(code_lines))
  
  counter <- 1
  for(i in relevant_lines) {
    
    if(i == TRUE) {
      temp <- vars_rhs[[counter]]
      temp <- get_type_from_list(temp, var_list)
      res[counter] <- deduce_type(temp)
      counter <- counter + 1
    } else {
      res[counter] <- "NA"
      counter <- counter + 1
    }
    
  }
  
  return(res)
}