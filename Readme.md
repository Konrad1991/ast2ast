<!-- badges: start -->
[![](http://cranlogs.r-pkg.org/badges/last-month/ast2ast?color=green)](https://cran.r-project.org/package=ast2ast)
[![](https://www.r-pkg.org/badges/version/ast2ast?color=green)](https://cran.r-project.org/package=ast2ast)
[![License: GPL2](https://img.shields.io/badge/license-GPL2-blue.svg)](https://cran.r-project.org/web/licenses/GPL-2)
<!-- badges: end -->


# R package ast2ast

## News

*  see more on: https://konrad1991.github.io/ast2ast/index.html
* 11-04-2023: version 0.3.1 is now on CRAN
* I gave a talk at the *useR! 2022* conference about *ast2ast*. The record can be watched using the following link: https://m.youtube.com/watch?v=5NDPOLunQTA&list=PL77T87Q0eoJjvKVFHuJZ5_BGVbPPpB8LL&index=8,

## Overview

Translates an R function into a C++ function. An external pointer to the C++ function or an R function is returned to the user. To install the Github version of the package use *devtools::install_github("Konrad1991/ast2ast", build_vignettes = TRUE)*. The package is also on CRAN and can be installed via *install.packages("ast2ast")*. 

The motivation to write the package was that it is often cumbersome using R functions in applications which have to call the R function very often (> 100 calls) (e.g. ODE solving, Optimization). One possiblity is to write the function in a faster programming language e.g. C. However, learning languages such as C is difficult and time consuming. Therefore *ast2ast* is a decent alternative as the function can be written in R.    

As an example solving a simple ODE-System. The translated code is considerable faster then R code and almost as fast as C++. Code for the example can be found in the vignettes.

![Benchmark](https://github.com/Konrad1991/ast2ast/blob/master/vignettes/benchmark.png)

## Documentation:

See the documentation of the function *translate* to see how to use *ast2ast*: [Function translate](translate.pdf).
If you want to get more details check: [Detailed Documentation ](DetailedDocumentation.pdf). 
In case you want to use *ast2ast* in other packages, or you are intersted how *ast2ast* works internally, see [Information for package authors](InformationForPackageAuthors.pdf).

## Contribution


Contribution would be warmly appreciated (See Code of Conduct).


