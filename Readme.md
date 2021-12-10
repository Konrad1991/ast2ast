# R package ast2ast

## Overview

The aim is to translate a tiny subset of R to a C++ (more accurate to ETR an expression template library which tries to mimic R (**https://github.com/Konrad1991/ETR**)). 

## Motivation

Write functions in R which have to be called really often e.g. for optimization problems or ode-function. 

**Project is in pre-alpha.**

## Small example 

```R
library(ast2ast)

f <- function() {
  print("Hello World")
}

fetr <- translate(f)
```

## Contribution

Contribution would be warmly appreciated (See Code of Conduct). 