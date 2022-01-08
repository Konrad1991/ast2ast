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

PC <- R6::R6Class("PC",

      public = list(

        name_fct = NULL,
        arguments = list(),

        initialize = function(node) {
          self$name_fct = node[[1]]
          self$arguments = node[2:length(node)]
        },

        get_name = function() {
          return(self$name_fct)
        },

        replace_TF = function() {

          for(i in seq_along(self$arguments)) {
            if(isTRUE(self$arguments[[i]]) ) {
              arguments[[i]] <- "true"
            } else if(isFALSE(self$arguments[[i]])) {
              self$arguments[[i]] <- "false"
            }
          }

        },

        oaf = function(var) { # only allowed functions
          check = deparse(self$name_fct) %in% var
          return(check)
        },

        get_var_names = function() {
          ret <- list()
          counter <- 1
          arg <- self$arguments
          for(i in seq_along(arg)) {
            temp <- arg[[i]]

            if(!is.call(temp) && (length(temp) == 1L && is.name(temp)) ) {
              ret[[counter]] <- temp
              counter <- counter + 1
            }
          }

          return(ret)
        }



      )

) # end class



generic <- R6::R6Class("generic",
    inherit = PC,

    public = list(

      change_code = function() {
      },

      convert = function(var) {
        self$replace_TF()
        self$oaf(var)
        self$change_code()
        ret <- list()
        ret[[1]] <- self$name_fct
        ret <- c(ret, self$arguments)
        return(ret)
      }

    )
)


assign <- R6::R6Class("assign",
    inherit = PC,

    public = list(

      change_code = function() {
        self$name_fct = as.name("=")
      },

      convert = function(var) {
        self$replace_TF()
        self$oaf(var)
        self$change_code()

        ret <- list()
        ret[[1]] <- self$name_fct
        ret <- c(ret, self$arguments)
        return(ret)
      }

    )
)

subset <- R6::R6Class("subset",
    inherit = PC,

    public = list(

      subassign = NULL,

      initialize = function(node, subset_or_subassign) {
        self$subassign = subset_or_subassign
        self$name_fct = node[[1]]
        self$arguments = node[2:length(node)]
      },

      change_code = function() {

        self$arguments <- lapply(self$arguments, function(x) {
          temp <- as.character(x)

          if(temp == '' && length(temp) == 1L) {
            return(as.symbol('nullptr'))
          } else {
            return(x)
          }
        })


        if(self$subassign == TRUE) {
            self$name_fct = as.name("subassign")
        } else if(self$subassign == FALSE) {
          self$name_fct = as.name("subset")
        }

      },

      get_var_names = function() {
        ret <- list()
        counter <- 1
        arg <- self$arguments
        for(i in seq_along(arg)) {
          temp <- arg[[i]]

          if(!is.call(temp) && (length(temp) == 1L && is.name(temp)) ) {
            ret[[counter]] <- temp
            counter <- counter + 1
          }
        }

        ret = ret[ret != as.symbol('nullptr')]

        return(ret)
      },

      convert = function(var) {
        self$replace_TF()
        self$oaf(var)
        self$change_code()

        ret <- list()
        ret[[1]] <- self$name_fct
        ret <- c(ret, self$arguments)
        return(ret)
      }

    )
)


loop <- R6::R6Class("loop",
    inherit = PC,

    public = list(
      index_variable = NULL,

      change_code = function() {
        self$index_variable = self$arguments[[1]]
        self$arguments[[1]] = as.name(paste0('auto&',self$arguments[[1]], ':'))
      },

      convert = function(var) {
        self$replace_TF()
        self$oaf(var)
        self$change_code()

        ret <- list()
        ret[[1]] <- self$name_fct
        ret <- c(ret, self$arguments)
        return(ret)
      },


      return_index_vars = function() {
        return(as.name(paste0('auto&',self$index_variable, ':') ) )
      }

    )
)


coca <- R6::R6Class("coca",
    inherit = PC,

    public = list(

      change_code = function() {
        self$name_fct = as.name("coca")
      },

      convert = function(var) {
        self$replace_TF()
        self$oaf(var)
        self$change_code()

        ret <- list()
        ret[[1]] <- self$name_fct
        ret <- c(ret, self$arguments)
        return(ret)
      }

    )
)

fastaccess <- R6::R6Class("fastaccess",
    inherit = PC,

    public = list(

      change_code = function() {
        self$name_fct = as.name("at")
      },

      convert = function(var) {
        self$replace_TF()
        self$oaf(var)
        self$change_code()

        ret <- list()
        ret[[1]] <- self$name_fct
        ret <- c(ret, self$arguments)
        return(ret)
      }

    )
)

printer <- R6::R6Class("printer",
    inherit = PC,

    public = list(

      change_code = function() {
        self$name_fct = as.name("etr::print")
      },

      convert = function(var) {
        self$replace_TF()
        self$oaf(var)
        self$change_code()

        ret <- list()
        ret[[1]] <- self$name_fct
        ret <- c(ret, self$arguments)
        return(ret)
      }

    )
)


range <- R6::R6Class("range",
    inherit = PC,

    public = list(

      change_code = function() {
        self$name_fct = as.name("colon")
      },

      convert = function(var) {
        self$replace_TF()
        self$oaf(var)
        self$change_code()

        ret <- list()
        ret[[1]] <- self$name_fct
        ret <- c(ret, self$arguments)
        return(ret)
      }

    )
)


math <- R6::R6Class("math",
    inherit = PC,

    public = list(

      change_code = function() {
        if(self$name_fct == "sin") {
          self$name_fct = as.name("sinus")
        } else if(self$name_fct == "asin") {
            self$name_fct = as.name("asinus")
        } else if(self$name_fct == "sinh") {
            self$name_fct = as.name("sinush")
        } else if(self$name_fct == "cos") {
            self$name_fct = as.name("cosinus")
        } else if(self$name_fct == "acos") {
            self$name_fct = as.name("acosinus")
        } else if(self$name_fct == "cosh") {
            self$name_fct = as.name("cosinush")
        } else if(self$name_fct == "tan") {
            self$name_fct = as.name("tangens")
        } else if(self$name_fct == "atan") {
            self$name_fct = as.name("atangens")
        } else if(self$name_fct == "tanh") {
            self$name_fct = as.name("tangensh")
        } else if(self$name_fct == "^") {
            self$name_fct = as.name("exp")
        } else if(self$name_fct == "log") {
            self$name_fct = as.name("ln")
        }

      },

      convert = function(var) {
        self$replace_TF()
        self$oaf(var)
        self$change_code()

        ret <- list()
        ret[[1]] <- self$name_fct
        ret <- c(ret, self$arguments)
        return(ret)
      }

    )
)