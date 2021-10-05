# R package AstToAst

## Scope of AstToAst

The aim is to translate a tiny subset of R to Rcpp. The idea is that the user defines a function in R which is translated to an Rcpp function. It is required that the user defines the types of the arguments passed to the function. Thus, the final function **translate** gets two arguments the code itself which defines the body of the function and a named list containing the types. Allowed types are numeric (**num**), a numeric vector (**num_vec**) and a numeric matrix (**num_mat**). This functions can be used e.g. for optimization problems, as ode-function and other application where a function call has to be fast. 

**Project is in pre-alpha.**

## Small example 

```R
library(AstToAst)

input_variables <- list(y = "num_vec", x = "num", z = "num_mat")
code <- "y <- y + 1
         print(y)
         z <- z + z
         print(z)"

translate(code, input_variables)
# creates a file ("result.cpp")
```

```Cpp
// code in result.cpp
#include <Rcpp.h>
#include <tidyCpp>
// [[Rcpp::depends(tidyCpp)]]
// [[Rcpp::depends(AstToAst)]]
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
- [ ] Functions (print, range, subset, etc.)
- [ ] adding missing features (e.g. matrix + scalar etc.)
- [x] Masterclass


## Contribution

Contribution would be warmly appreciated (See Code of Conduct). 