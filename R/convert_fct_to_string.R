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
# function to convert function in strings
# ================================================================================
c2s <- function(fct) {
  d <- body(fct)
  code <- list()
  for(i in 2:length(d)) {
    code[[i-1]] <- toString(d[i])
  }
  return(noquote(code))
}


# Example
# c2s(test <- function(a) {print("a")
#   a <- a + 1
#   print(a)})



# ================================================================================
# function to get arguments of function and turn it in vector
# ================================================================================
a2v <- function(fct) {
  a <- as.list(args(fct))
  a <- a[1:length(a) -1]
  a <- names(a)
  return(a)
}

# Example
# a2v(test <- function(a, b, c, d) {
#   print("a")
#   a <- a + 1
#   print(a)
# })
