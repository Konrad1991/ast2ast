# R package ast2ast

## Overview

Translates a R function into a C++ function. An external pointer to the C++ function is returned to the user. To install the package use *devtools::install_github("Konrad1991/ast2ast", build_vignettes = TRUE)*. The motivation to write the package was that it is often cumbersome using R functions in applications which have to call this function very often (> 100 calls) (e.g. ODE solving, Optimization). One possiblity is to write the function in a faster programming language e.g. C. However, learning languages such as C is difficult and time consuming. Therefore *ast2ast* is a decent alternative as the function can be written in R.    

As an example solving a simple ODE-System. The translated code is considerable faster then R code and almost as fast as C++. Code for the example can be found in the vignettes.

![Benchmark](https://github.com/Konrad1991/ast2ast/blob/master/vignettes/benchmark.png)

## Documentation

*translate(f, verbose = FALSE, reference = FALSE)*

*f* The function which should be translated from R to C++.
*verbose* If set to true the output of RcppXPtrUtils::cppXPtr is printed.
*reference* If set to true the arguments are passed by reference.
Returns an external pointer of the generated C++ function

### The following types are supported:

* numeric vectors
* numeric matrices
  
Variables can be either numeric vectors or matrices.
Notably, it is possible that the variable change the type within the function.
**It is possible to declare a variable of a scalar numeric data type.
This is done by adding '_db' to the end of the variable. Each time '_db' is found
the variable is declared as a scalar numeric data type. In this case the
object cannot change its type!**


### The following functions are supported:

* assignment: = and <-
* allocation: vector and matrix
* information about objects: length and dim
* Basic operations: +, -, *, /
* Indices: [] and at
* mathematical functions: sin, asin, sinh, cos, acos, cosh, tan, atan, tanh, log, ^ and exp
* concatenate objects: c
* control flow: for, if, else if, else
* comparison: ==, !=, >, <, >= and <=
* printing: print
* returning objects: return
* catmull-rome spline: cmr
* to get a range of numbers the ':' function can be used


#### Some details about the implemented functions

* allocation of memory works: Following forms are possible: vector(size_of_elements), vector(value, size_of_elements) matrix(nrows, ncols), matrix(value, nrows, ncols). The latter fills the matrix or the vector with the specified 'value'.
* For indices squared brackets '[]' can be used as common in R. \string{Despite the results of calculations cannot be used!} Beyond that the function 'at' exists which accepts as first argument a variable and as the second argument you pass the desired index. The caveat of using 'at' is that only one entry can be accessed. Whereas '[]' can return more then one element. **The 'at'function returns a reference to the vector entry. Therefore variable[index] can behave differently then at(variable, index). The function has to be use carefully when 'at' is used. Especially if '[]' and 'at' are mixed the function behaviour is difficult to predict. Please test it before using in a serious project.**
* For loops can be written as used in R
        for(index in variable)\{ 
            # do whatever 
        \} 
        for(index in 1:length(variable)\{ 
            # do whatever 
        \} 
* Be aware that it is not possible to assign the result of a comparison to a variable.
* The print function accepts either a scalar, vector, matrix, string, bool or nothing (empty line).
* In order to return an object use the 'return' function (The last object is not returned automatically as in R).
* In order to interpolate values the 'cmr' function can be used. The function needs three arguments.
    * the first argument is the point of the independent variable (x) for which the dependent variable should be calculated (y). This has to be a vector of length one.
    * the second argument is a vector defining the points of the independent variable (x). This has to be a vector of at least length four.
    * the third argument is a vector defining the points of the dependent variable (y). This has to be a vector of at least length four.
* Be aware that the R code is translated to ETR. An expression template library which tries to mimic R.
However, it does not behave exactly like R! Please check your compiled function before using it in a serious project.
If you want to see how ast2ast differs from R in detail check the vignette: 'Detailed Documentation'.


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
