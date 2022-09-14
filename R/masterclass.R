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

# Masterclass
MA <- R6::R6Class("MA",
                  
                  public = list(
                    
                    R_fct = NULL,
                    name = NULL,
                    args = NULL,
                    args_2_fct = NULL,
                    body = NULL,
                    desired_type = NULL,
                    temp = list(),
                    ast = list(),
                    code = list(),
                    char = list(),
                    var_all = c(),
                    var_types = c(),
                    var_index = c(),
                    return_TF = c(),
                    return_valtype = c(),
                    
                    initialize = function(fct, desired_type, name_f, R_fct, return_valtype) {
                      self$name = name_f #as.character(substitute(fct))
                      self$args = methods::formalArgs(fct)
                      self$body = body(fct)
                      self$body = self$body[2:length(self$body)]
                      self$desired_type = desired_type
                      self$args_2_fct = methods::formalArgs(fct) #as.list(args(fct))
                      self$R_fct = R_fct
                      self$return_valtype = return_valtype
                    },
                    
                    getast = function() {
                      for(i in seq_along(self$body)) {
                        # convert ast R --> ETR
                        self$temp[[i]] = LC$new(self$body[[i]])
                        self$ast[[i]] = self$temp[[i]]$ast
                        
                        # get vars
                        self$var_all = c(self$var_all, self$temp[[i]]$vars)
                        self$var_index = c(self$var_index, self$temp[[i]]$index_vars)
                        
                        # return was found?
                        self$return_TF = c(self$return_TF, self$temp[[i]]$found_return)
                      }

                    },
                    
                    get_calls = function(code) {
                      out <- purrr::map_if(code, is.list, self$get_calls)
                      out <- as.call(out)
                      return(out)
                    },
                    
                    ast2call = function() {
                      for(i in seq_along(self$ast)) {
                        self$code[[i]] = self$get_calls(self$ast[[i]])
                      }
                    },
                    
                    call2char = function() { # some errors {}
                      
                      j = 1
                      for(i in seq_along(self$code)) {
                        temp = deparse(self$code[[i]], width.cutoff = 500)
                        temp = gsub("\\bin\\b", "", temp)
                        temp = gsub("`", "", temp)
                        temp = paste0(temp, ';')
                        
                        # loop over temp
                        temp2 = character(length(temp)*2)
                        counter = 1
                        for(i in seq_along(temp)) {
                          temp2[counter] = temp[i]
                          counter = counter + 1
                          temp2[counter] = "\n"
                          counter = counter + 1
                        }
                        temp = temp2
                        
                        self$char[[j]] = temp
                        j = j + 1
                        self$char[[j]] = "\n"
                        j = j + 1
                      }
                      
                      
                    },
                    
                    get_vars = function() {
                      temp1 = unique(self$var_all)
                      temp2 = unique(self$var_index)
                      
                      # remove index variables
                      # necessary in order to remove i and auto$i:
                      temp3 = list()
                      for(i in seq_along(temp2)) {
                        tm <- gsub('auto&', '', temp2[[i]])
                        
                        tm <- gsub(':', '', tm)
                        temp3[[i]] = tm
                      }
                      
                      temp2 = c(temp2, unlist(temp3))
                      
                      for(i in seq_along(temp1)) {
                        for(j in seq_along(temp2)) {
                          if(temp1[[i]] == temp2[[j]]) {
                            temp1[[i]] = NA
                            break
                          }
                        }
                      }
                      
                      temp1 = temp1[!is.na(temp1)]
                      
                      # remove arguments passed to fct
                      for(i in seq_along(temp1)) {
                        for(j in seq_along(self$args)) {
                          if(temp1[[i]] == self$args[[j]]) {
                            temp1[[i]] = NA
                            break
                          }
                        }
                      }
                      
                      temp1 = temp1[!is.na(temp1)]
                      
                      # remove true and false
                      for(i in seq_along(temp1)) {
                        if(temp1[[i]] == as.name("true") || temp1[[i]] == as.name("false")) {
                          temp1[[i]] = NA
                        }
                      }
                      
                      ret = temp1[!is.na(temp1)]
                      
                      self$var_types = vector("list", length = length(ret))
                      self$var_types = lapply(self$var_types, function(x) {
                        return("sexp")
                      })
                      
                      # check for doubles
                      for(i in seq_along(ret)) {
                        nm = as.character(ret[[i]])
                        nm = unlist( strsplit(nm, "") )
                        if(length(nm) >= 4) {
                          if( (nm[[length(nm)]] == "b") && (nm[[length(nm) - 1]] == "d")
                              && (nm[[length(nm) - 2]] == "_") ) {
                            self$var_types[[i]] = "double"
                          }
                        }
                      }
                      
                      return(ret)
                    },
                    
                  
                    signature_own = function(desired_type, reference, extern = FALSE, typedef = FALSE) {
                      
                      # arguments passed to f & define signature
                      
                      arguments_string <- character(length(self$args))
                      
                      for(i in seq_along(self$args)) {
                        y <- paste0(self$args[[i]])
                        type <- NULL
                        type_name <- NULL
                        temp <- NULL
                        if(reference) {
                          type <- paste0(desired_type[i], "&")
                          type_name <- desired_type[i]
                        } else {
                          type <- desired_type[i]
                          type_name <- desired_type[i]
                        }
                        
                        
                        if(desired_type[i] == "ptr_vec") {
                          
                          if(reference == TRUE) {
                            stop("Reference cannot be set to TRUE in combination with ptr_vec or ptr_mat")
                          }

                          temp <- paste("double*", paste0(y, "_double_ptr"), ',', 
                                        "int", paste0(y, "_int_size"), ',',
                                        collapse = '')
                          if(i == length(self$args)) {
                            temp <- paste("double*", paste0(y, "_double_ptr"), ',', 
                                          "int", paste0(y, "_int_size"),
                                          collapse = '')
                          }
                          
                        } else if(desired_type[i] == "ptr_mat") {
                          
                          if(reference == TRUE) {
                            stop("Reference cannot be set to TRUE in combination with ptr_vec or ptr_mat")
                          }
                          temp <- paste("double*", paste0(y, "_double_ptr"), ',', 
                                        "int", paste0(y, "_int_rows"), ',',
                                        "int", paste0(y, "_int_cols"), ',',
                                        collapse = '')
                          if(i == length(self$args)) {
                            temp <- paste("double*", paste0(y, "_double_ptr"), ',', 
                                          "int", paste0(y, "_int_rows"), ',',
                                          "int", paste0(y, "_int_cols"),
                                          collapse = '')
                          }
                          
                        } else if(self$R_fct == TRUE) { # not ptr_vec or ptr_mat
                          temp <- paste(type, paste0(y, type_name), ',', collapse = '')
                          
                          if(i == length(self$args)) {
                            temp <- paste(type, paste0(y, type_name), collapse = '')
                          }  
                        } else if(self$R_fct == FALSE) { # XPtr interface
                          temp <- paste(type, y, ',', collapse = '')
                          
                          if(i == length(self$args)) {
                            temp <- paste(type, y, collapse = '')
                          }
                        }
 
                        arguments_string[i] <- temp
                      }
                      
                      arguments_string <- paste(arguments_string, collapse = " ")
                      
                      sig = NULL
                      if(extern == FALSE) {
                        sig <- paste(self$return_valtype, self$name, '(', arguments_string, ')', '{', collapse = " ")
                      } else {
                        sig <- paste('extern "C" {' , self$return_valtype, self$name, '(', arguments_string, ');}', collapse = " ")
                      }
                      
                      if(typedef == TRUE) {
                        sig <- paste(self$return_valtype, '(*fct_ptr)', '(', arguments_string, ');', collapse = " ")
                        return(sig)
                      }
                      
                      return(sig)
                    },
                    
                    vars_declaration_own = function(desired_type) {
                      variables = self$get_vars()
                      
                      args_dec <- sapply(seq_along(variables), function(x) {
                        
                        temp <- paste(self$var_types[[x]], variables[[x]], ';', collapse = '')
                        return(temp)
                      })
                      args_dec <- paste(args_dec, collapse = " ")
                      
                      fct_args = self$args_2_fct
                      fct_args_ptr = sapply(seq_along(fct_args), function(x) {
                        if(desired_type[[x]] == "ptr_vec") {
                          return(x)
                        } else if(desired_type[[x]] == "ptr_mat") {
                          return(x)
                        }
                        return(NA)
                      })
                      fct_args_ptr = fct_args_ptr[!is.na(fct_args_ptr)]
                      
                      
                      fct_args_dec <- sapply(seq_along(fct_args), function(x) {
                        temp <- NULL
                        if(desired_type[[x]] == "ptr_vec") {
                          temp <- paste('sexp', fct_args[[x]], '(', 
                                        paste0(fct_args[[x]], "_int_size,"),
                                        paste0(fct_args[[x]], "_double_ptr,"),
                                        '2);', collapse = "") # borrow
                          
                        } else if(desired_type[[x]] == "ptr_mat") {
                          temp <- paste('sexp', fct_args[[x]], '(', 
                                        paste0(fct_args[[x]], "_int_rows,"), paste0(fct_args[[x]], "_int_cols,"),
                                        paste0(fct_args[[x]], "_double_ptr,"),
                                        '2);', collapse = "") # borrow
                          
                        } else {
                          temp <- paste('sexp', fct_args[[x]], ';')
                          temp <- c(temp, fct_args[[x]], '=', paste0(self$args[[x]], desired_type[x], ';', collapse = ''))  
                        }
                        
                        return(temp)
                      })
                      
                      if(self$R_fct == TRUE) {
                        args_dec <- c(args_dec, fct_args_dec)  
                      } else if(length(fct_args_ptr) >= 1) {
                        args_dec <- c(args_dec, fct_args_dec[fct_args_ptr])  
                      }
                      
                      return(args_dec)
                    },
                    
                    build_own = function(verbose = FALSE, reference = FALSE) {
                      self$getast()
                      self$ast2call()
                      self$call2char()
                      
                      fct = c(
                        "// [[Rcpp::depends(ast2ast)]] \n",
                        "// [[Rcpp::depends(RcppArmadillo)]] \n",
                        "// [[Rcpp::plugins(cpp17)]] \n",
                        '#include "etr.hpp"', "\n",
                        "// [[Rcpp::export]]", "\n",
                        "SEXP getXPtr();", "\n", 
                        self$signature_own(self$desired_type, reference, extern = FALSE),
                        self$vars_declaration_own(self$desired_type),
                        self$char,
                        '}',
                        
                        "SEXP getXPtr() {", "\n", 
                        paste("typedef", self$signature_own(self$desired_type, reference, extern = FALSE, typedef = TRUE), ";"),
                        paste("return Rcpp::XPtr<fct_ptr>(new fct_ptr(& ", self$name, '));'),
                        "}"
                      )
                      
                      fct <- paste( unlist(fct), collapse='')
                    },
                    
                    # build with own types
                    build_own_SEXP = function(verbose = FALSE, reference = FALSE) {
                      return_TF <- any(TRUE %in% self$return_TF)
                      
                      self$getast()
                      self$ast2call()
                      self$call2char()
                      fct = NULL
                      
                      if(return_TF == FALSE) { 
                        fct = c(
                          "// [[Rcpp::depends(ast2ast)]] \n",
                          "// [[Rcpp::depends(RcppArmadillo)]] \n",
                          "// [[Rcpp::plugins(cpp17)]] \n",
                          '#include "etr.hpp" \n',
                          #self$signature_own(self$desired_type, reference, extern = TRUE), "\n",
                          '// [[Rcpp::export]] \n',
                          self$signature_own(self$desired_type, reference, extern = FALSE), "\n",
                          self$vars_declaration_own(self$desired_type), "\n",
                          self$char, "\n",
                          "return R_NilValue;",
                          '}'
                        )
                      } else {
                        fct = c(
                          "// [[Rcpp::depends(ast2ast)]] \n",
                          "// [[Rcpp::depends(RcppArmadillo)]] \n",
                          "// [[Rcpp::plugins(cpp17)]] \n",
                          '#include "etr.hpp" \n',
                          #self$signature_own(self$desired_type, reference, extern = TRUE), "\n",
                          '// [[Rcpp::export]] \n',
                          self$signature_own(self$desired_type, reference, extern = FALSE), "\n",
                          self$vars_declaration_own(self$desired_type), "\n",
                          self$char, "\n",
                          '}'
                        )
                      }
                      
                      fct <- paste( unlist(fct), collapse='')
                    }
                    
                    
                  )
                  
                  
)


