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
                    
                    initialize = function(fct, desired_type, name_f, R_fct) {
                      self$name = name_f #as.character(substitute(fct))
                      self$args = methods::formalArgs(fct)
                      self$body = body(fct)
                      self$body = self$body[2:length(self$body)]
                      self$desired_type = desired_type
                      self$args_2_fct = methods::formalArgs(fct) #as.list(args(fct))
                      self$R_fct = R_fct
                    },
                    
                    getast = function() {
                      for(i in seq_along(self$body)) {
                        # convert ast R --> ETR
                        self$temp[[i]] = LC$new(self$body[[i]], self$R_fct)
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
                    
                    
                    return_type = function() {
                      return_TF <- any(TRUE %in% self$return_TF)
                      
                      # determine return type
                      ret_type <- as.symbol("void")
                      if(return_TF == TRUE) {
                        ret_type <- as.symbol(self$desired_type)
                      }
                      
                      return(ret_type)
                    },
                    
                    signature = function(desired_type, reference) {
                      # arguments passed to f & define signature
                      
                      arguments_string <- sapply(self$args, function(x) {
                        y <- paste0(x)
                        if(reference == TRUE) {
                          desired_type = paste(desired_type, '&')
                        }
                        temp <- paste(desired_type, y, ',', collapse = '')
                        
                        if (parent.frame()$i[] == length(self$args) )  {
                          temp <- paste(desired_type, y, collapse = '')
                        }
                        
                        return(temp)
                      })
                      arguments_string <- paste(arguments_string, collapse = " ")
                      
                      sig <- paste(self$return_type(), self$name, '(', arguments_string, ')', '{', collapse = " ")
                    },
                    
                    signature_SEXP = function(desired_type, reference, extern = FALSE) {
                      # arguments passed to f & define signature
                      
                      arguments_string <- sapply(self$args, function(x) {
                        y <- paste0(x)
                        temp <- paste(desired_type, paste0(y, 'SEXP'), ',', collapse = '')
                        
                        if (parent.frame()$i[] == length(self$args) )  {
                          temp <- paste(desired_type, paste0(y, 'SEXP'), collapse = '')
                        }
                        
                        return(temp)
                      })
                      arguments_string <- paste(arguments_string, collapse = " ")
                      
                      sig = NULL
                      if(extern == FALSE) {
                        sig <- paste("SEXP", self$name, '(', arguments_string, ')', '{', collapse = " ")
                      } else {
                        sig <- paste('extern "C" {' , "SEXP", self$name, '(', arguments_string, ');}', collapse = " ")
                      }
                      
                      
                      return(sig)
                    },
                    
                    vars_declaration = function(desired_type) {
                      variables = self$get_vars()
                      args_dec <- sapply(seq_along(variables), function(x) {
                        temp <- paste(self$var_types[[x]], variables[[x]], ';', collapse = '')
                        return(temp)
                      })
                      args_dec <- paste(args_dec, collapse = " ")
                      
                      return(args_dec)
                    },
                    
                    vars_declaration_SEXP = function(desired_type) {
                      variables = self$get_vars()
                      
                      args_dec <- sapply(seq_along(variables), function(x) {
                        temp <- paste(self$var_types[[x]], variables[[x]], ';', collapse = '')
                        return(temp)
                      })
                      args_dec <- paste(args_dec, collapse = " ")
                      
                      fct_args = self$args_2_fct
                      fct_args_dec <- sapply(seq_along(fct_args), function(x) {
                        temp <- paste('sexp', fct_args[[x]], ';')
                        temp <- c(temp, fct_args[[x]], '=', paste0(self$args[[x]], 'SEXP', ';', collapse = ''))
                        return(temp)
                      })
                      
                      args_dec <- c(args_dec, fct_args_dec)
                      return(args_dec)
                    },
                    
                    build = function(verbose = FALSE, reference = FALSE) {
                      self$getast()
                      self$ast2call()
                      self$call2char()
                      
                      fct = c(
                        self$signature(self$desired_type, reference),
                        self$vars_declaration(self$desired_type),
                        self$char,
                        '}'
                      )
                      
                      fct <- paste( unlist(fct), collapse='')
                    },
                    
                    build_SEXP = function(verbose = FALSE, reference = FALSE) {
                      self$getast()
                      self$ast2call()
                      self$call2char()
                      
                      fct = NULL
                      if(self$return_type() == "void") {
                        fct = c(
                          "// [[Rcpp::depends(ast2ast)]] \n",
                          "// [[Rcpp::depends(RcppArmadillo)]] \n",
                          "// [[Rcpp::plugins(cpp17)]] \n",
                          self$signature_SEXP(self$desired_type, reference, extern = TRUE), "\n",
                          self$signature_SEXP(self$desired_type, reference, extern = FALSE), "\n",
                          self$vars_declaration_SEXP(self$desired_type), "\n",
                          self$char, "\n",
                          "return R_NilValue;",
                          '}'
                        )
                      } else {
                        fct = c(
                          "// [[Rcpp::depends(ast2ast)]] \n",
                          "// [[Rcpp::depends(RcppArmadillo)]] \n",
                          "// [[Rcpp::plugins(cpp17)]] \n",
                          self$signature_SEXP(self$desired_type, reference, extern = TRUE), "\n",
                          self$signature_SEXP(self$desired_type, reference, extern = FALSE), "\n",
                          self$vars_declaration_SEXP(self$desired_type), "\n",
                          self$char, "\n",
                          '}'
                        )
                      }
                      
                      fct <- paste( unlist(fct), collapse='')
                    }
                    
                    
                  )
                  
                  
)