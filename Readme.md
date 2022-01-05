# R package ast2ast

## Overview

Translates a R function into a C++ function. An external pointer to the C++ function is returned to the user. To install the package use *devtools::install_github("Konrad1991/ast2ast")*. The motivation to write the package was that it is often cumbersome using R functions in applications which have to call this function very often (> 100 calls) (e.g. ODE solving, Optimization). One possiblity is to write the function in a faster programming language e.g. C. However, learning languages such as C is difficult and time consuming. Therefore *ast2ast* is a decent alternative as the function can be written in R.    

```R
f <- function() { print("Hello World!")}

pointer_to_f_cpp <- ast2ast::translate(f)

Rcpp::sourceCpp(code = "
#include <Rcpp.h>
typedef void (*fp)();

// [[Rcpp::export]]
void call_fct(Rcpp::XPtr<fp> inp) {
  fp f = *inp;
  f();
}
")

call_fct(pointer_to_f_cpp)
```

For more examples check the vignette *Examples*.

## Contribution

Contribution would be warmly appreciated (See Code of Conduct). 