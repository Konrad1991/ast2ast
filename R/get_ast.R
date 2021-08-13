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
# You should have received a copy of the GNU General Public License along with pso
# If not see: https://www.gnu.org/licenses/old-licenses/gpl-2.0.html#SEC4

# ================================================================================
# function that seperate the expressions
# ================================================================================
sep_expressions <- function(code) {
  out <- rlang::parse_exprs(code)
  return(out)
}

# ================================================================================
# function that extract abstract syntax tree
# ================================================================================
extractast <- function(seperated_code) {
  out <- purrr::map_if(as.list(seperated_code), is.call, extractast)
  return(out)
} 

