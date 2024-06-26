# helper functions
# ============================================================================
substi <- function(inp, replace, replace_with) {
  if (is.name(inp) && identical(inp, replace)) return(bquote(.(replace_with)))
  inp
}

body_of_fct <- function(f) {
  brackets <- body(f)[[1]]
  body <- NULL
  if(brackets != as.name("{")) {
    body <- body(f)  
  } else {
    l <- length(body(f))
    body <- body(f)[2:l]
  }
  return(body)
}

check <- function(a, b, c) {
  a == b || a == c
}

#remove_bracket_from_var
rbfv <- function(var) {
  ret <- var
  if(length(as.list(var)) > 1) {
    ret <- as.list(var)[[2]]
  }
  ret
}

# get rs from one line
grs <- function(line) {
  if(length(line) >= 3) {
    return(line[[3]])
  } else {
    stop("Something went wrong.")
  }
}

# differentiate codeline
diff <- function(leftside, codeline, indep_vars, dep_var, fl, jac_mat) {
  tocheck <- rbfv(leftside)
  if(tocheck != dep_var) return(NULL)
  fct <- function() {stop("something went wrong")}
  body(fct, envir = environment(fct)) <- codeline
  ret <- lapply(indep_vars, function(inp) {
    df <- d(fct, !!inp, fl) 
    cl <- call("=", leftside, codeline)
    if(length(leftside) == 1 && length(inp)) {
      jac_mat <- paste0(jac_mat, "[1, 1]")
    } else if(length(leftside) >= 3 && length(inp) >= 3) {
      fct_index <- leftside[[3]]
      indep_index <- inp[[3]]
      jac_mat <- paste0(jac_mat, "[", fct_index, ",", indep_index, "]")
    }
    
    if(body(df) == 0) return(NULL)
    deriv <- call("=", str2lang(jac_mat), simplify_expr(body(df)))
    return(deriv)
  })
  
}

# helper classes
# ==============================================================================
Vars <- R6::R6Class(
  
  "Vars",
  
  public = list(
    
    left_vars = NULL,
    right_vars = NULL,
    # this is a bit tricky. If variables are found within this functions they are ignored
    function_to_ignore = c("if", "else if", "else", "<", ">", "<=", ">=", "==", "!=", "{", "length"),
    symbol_fct_call = NULL,
    left_side = FALSE,
    functions = c("[", "(", "+", "-", "/", "*", "^"),
    isfct = FALSE,
    
    initialize = function(const_fcts) {
      self$function_to_ignore <- c(self$function_to_ignore, const_fcts)
    },
    
    save_rs = function(inp) {
      if(self$isfct) self$right_vars <- c(self$right_vars, inp)
    },
    
    save_ls = function(inp) {
      if(self$isfct) self$left_vars <- c(self$left_vars, inp)
    },
    
    find_vars = function(codeline) {
      if(!is.call(codeline)) {
        if(self$left_side) {
          if(as.name("[") == codeline) {
            self$left_side <- TRUE
          } else if(as.name("<-") == codeline) {
            self$left_side <- TRUE
          } else {
            self$left_side <- FALSE
          }
        } 
        self$isfct <- FALSE
        return(codeline)
      } else {
        self$isfct <- TRUE
      } 
      
      codeline = as.call(codeline)
      codeline = as.list(codeline)
      
      if( (codeline[[1]] == as.name("<-")) ||
          (codeline[[1]] == as.name("=")) ) {
        
        self$save_ls(codeline[[2]])
        
        temp <- codeline[3:length(codeline)]
        for(i in seq_along(temp)) {
          ti <- temp[[i]]
          if(!is.call(ti) && length(ti) == 1L && is.name(ti)) {
            self$save_rs(ti)
          }  
        }
        
        self$left_side <- TRUE
        
      } else if( deparse(codeline[[1]]) %in% self$function_to_ignore) {
        
      } else {
        
        if(!is.call(codeline)) {
          if(codeline[[1]] != as.name("[")) {
            if(self$left_side == FALSE) {
              sapply(codeline, function(x) {
                if(is.symbol(x)) {
                  temp <- deparse(x)
                  if( !(temp %in% self$functions) ) {
                    self$save_rs(temp)
                  }
                } else {
                  if(is.language(x)) {
                    to_check <- as.list(x)[[1]]
                    to_check <- deparse(to_check)
                    if(! to_check %in% self$functions) {
                      self$symbol_fct_call <- c(self$symbol_fct_call, to_check)
                    }
                  }
                }
              } )
              
            } 
          } else {
            code <- c(as.name(codeline[[1]]), codeline[2:length(codeline)])
            code <- as.call(code)
            code <- rlang::enexpr(code)
            if(self$left_side == FALSE) {
              self$save_rs(code)
            } 
            
          }
          
        }
        
      }
      
      lapply(codeline, self$find_vars)
    },
    
    get_ls = function() {
      return(self$left_vars)
    },
    
    get_rs = function() {
      self$right_vars <- unique(self$right_vars)
      self$symbol_fct_call <- unique(self$symbol_fct_call)
      if(!is.null(self$symbol_fct_call) && !identical(self$symbol_fct_call, character(0))) {
        self$right_vars <- self$right_vars[self$right_vars != self$symbol_fct_call]  
      }
      
      ret <- lapply(self$right_vars, function(x) {
        if(is.character(x)) {
          ret <- str2lang(x)
          names(ret) <- NULL
          return(ret)
        }
        return(x)
      })
      return(ret)
    }
    
  )   # end public list                 
)

ReplaceIt <- R6::R6Class("ReplaceIt",
  
  public = list(
        
    to_replace = NULL,
    replace_with = NULL,

    initialize = function(to_replace, replace_with) {
      self$to_replace = to_replace
      self$replace_with = replace_with
    },
    
    replaceit = function(code) {
      if(!is.call(code)) {
        return(code)
      }
      fct = code[[1]]
      code <- sapply(code, function(x) {
        if(is.symbol(x)) {
          if(is.null(self$to_replace)) return(x)
          
          if(deparse(x) == self$to_replace) {
            x = str2lang(bquote(.(self$replace_with)))
          }
          return(x)
        } else {
          return(x)
        }
      })
      code <- as.call(code)
      code = as.list(code)
      lapply(code, self$replaceit)  
    },

    get_code = function(code) {
      out <- purrr::map_if(code, is.list, self$get_code)
      out <- as.call(out)
      return(out)
    }
    
  ) # end public
  
)

replace_all <- function(b, to_replace, replace_with) {
  r <- ReplaceIt$new(to_replace, replace_with)
  res <- c()
  for(i in seq_along(1:length(b))) {
    b_i <- r$replaceit(b[[i]])
    b_i_call <- r$get_code(b_i)
    res <- c(res, b_i_call)
  }
  res
}

Unfold <- R6::R6Class(
  
  "Unfold",
  
  public = list(
    
    diff = NULL,
    fl = NULL,
    to_diff = NULL,
    y = NULL,
    jac_mat = NULL,
    
    initialize = function(diff, fl, to_diff, y, jac_mat) {
      self$diff <- diff
      self$fl <- fl
      self$to_diff <- to_diff
      self$y <- y
      self$jac_mat <- jac_mat
    },
    
    rb = function(code) {
      results <- list()
      counter <- 1
      for(i in seq_along(1:length(code))) {
        fs <- as.character(code[[i]])
        if(fs[[1]] == "<-" || fs[[1]] == "=") {
          ls <- code[[i]][[2]]
          codeline  <- code[[i]][[3]]
          df <- diff(ls, codeline, self$to_diff, self$y, self$fl, self$jac_mat)
          results[[counter]] <- code[[i]]
          counter <- counter + 1
          if(!is.null(df)) {
            for(j in seq_along(1:length(df))) {
              results[[counter]] <- df[[j]]
              counter <- counter + 1
            }  
          }
          
        } else {
          results[[counter]] <- code[[i]]
          counter <- counter + 1
        }
      }
      return(results)
    },
    
    get_code = function(code) {
      out <- purrr::map_if(code, is.list, self$get_code)
      out <- as.call(out) 
      return(out)
    },
    
    uf = function(code) {
      
      if(!is.call(code)) {
        return(code)
      }
      
      if(code[[1]] == "{") {
        code  <- self$rb(code)
      }
      
      code <- as.call(code)
      code = as.list(code)
      lapply(code, self$uf)  
      
    }
    
  ) # end public list
  
)

