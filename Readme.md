# R package ast2ast

## Scope of ast2ast

The aim is to translate a tiny subset of R to Rcpp. The user passes R code and the types of the arguments for the function. Possible types are numeric (**num**), a numeric vector (**num_vec**) and a numeric matrix (**num_mat**).

## Motivation

Write functions in R which have to be called really often e.g. for optimization problems or ode-function. 

**Project is in pre-alpha.**

## Small example 

```R
library(ast2ast)

input_variables <- list(y = "num_vec", x = "num", z = "num_mat")
code <- "y <- y + 1
         print(y)
         z <- z + z
         print(z)"

translate(code, input_variables)
# creates a file ("result.cpp")

# compile result.cpp
Rcpp::sourceCpp("result.cpp")

# call transpated and compiled function
m <- matrix(2,2,2)
fct(c(1, 2, 3),1,m)

# result:
# 2
# 3
# 4
# 4	4	4	4	
```

```Cpp
// code in result.cpp
#include <Rcpp.h>
#include <tidyCpp>
// [[Rcpp::depends(tidyCpp)]]
// [[Rcpp::depends(ast2ast)]]
#include "all.hpp" 
// [[Rcpp::export]]
void fct(
SEXP ySEXP ,
SEXP xSEXP ,
SEXP zSEXP
)
{
Master y (SEXP_to_VEC( ySEXP  ), "num_vec" );
Master x (SEXP_to_VEC( xSEXP  ), "num" );
Master z (SEXP_to_VEC( zSEXP  ), "num_mat" );
y.num_vec = y.num_vec + 1 ;
print(y.num_vec) ;
z.num_mat = z.num_mat + z.num_mat ; // (does not work correctly for matrix)
print(z.num_mat) ;
}
```

## Current project status (R)

- [x] get Abstract syntax tree (ast)
- [x] deduce type based on variables at right hand site (rhs)
- [x] declare variables
- [x] Initialice variables
- [x] replace R functions with C++ equivalents 
- [ ] replace R for loop with C++ for loop
- [x] define function signature
- [ ] return fct
- [ ] error checks (e.g. are only allowed functions used etc.)
- [x] put everything together 

## Current project status C++

- [x] Vector Class
- [x] Matrix Class
- [ ] subset fct implementation
- [ ] Functions (print, range, subset, l, le etc.)
- [ ] adding missing features (e.g. matrix + scalar etc.)
- [ ] declaration of matrix (dimension has to be transferred)
- [x] Masterclass

- [ ] Assure that it behaves like R (or at least very similar)

## Later

- [ ] give the possibility to activate side effects (data stored as pointer need extra vector and matrix module)
- [ ] add math functions (sin, cos, tan, etc.)



## Contribution

Contribution would be warmly appreciated (See Code of Conduct). 