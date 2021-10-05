# R package AstToAst

## Scope of AstToAst

The aim is to translate a tiny subset of R to Rcpp. The idea is that the user defines a function in R which is translated to an Rcpp function. It is required that the user defines the types of the arguments passed to the function. Thus, the final function **translate** gets two arguments the code itself which defines the body of the function and a named list containing the types. Allowed types are numeric (**num**), a numeric vector (**num_vec**) and a numeric matrix (**num_mat**). This functions can be used e.g. for optimization problems, as ode-function and other application where a function call has to be fast. 

**Project is in pre-alpha.**


## Current project status (R)

- [x] get Abstract syntax tree (ast)
- [x] deduce type based on variables at right hand site (rhs)
- [x] declare variables
- [x] Initialice variables
- [x] replace R functions with C++ equivalents 
- [ ] replace R for loop with C++ for loop
- [x] define function signature
- [x] put everything together 

```R
library(AstToAst)
library(rlang)

input_variables <- list(y = "num_vec") 
code <- "y <- y + 1
         print(y)"

res <- translate(code, list(y = "num_vec", x = "num", z = "num_mat"))
# creates a file ("result.cpp")
```

```Cpp
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
y.num_vec = y.num_vec + 1 ;
print(y.num_vec) ;
}
```

## Current project status C++

- [x] Vector Class
- [x] Matrix Class
- [ ] subset fct implementation
- [ ] Functions (print, range, subset, etc.)
- [x] Masterclass


## Contribution

Contribution would be warmly appreciated (See Code of Conduct). 