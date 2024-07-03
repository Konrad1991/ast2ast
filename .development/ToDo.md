# ToDo

## General

- [x] remove unnecessary code
  - [x] remove RNG stuff
  - [x] AllVars stuff
  - [x] unused variables/arguments in astClass

## Tests

- [x] R CMD check --as-cran
- [x] get code working with clang

### XPtr tests

- [x] write test with f recieving Vec, Borrows mixed
  - [x] some of the Vecs are than borrowed
- [x] tests for diff
  - [x] 5 examples from wikipedia
  - [x] tests from dfdr
  - [x] tests how to handle scalar values

### Safety

- [x] sanatizer 
  - https://github.com/eddelbuettel/sanitizers
  - [x] docker run --rm -ti -v $PWD:/work -w /work rocker/r-devel-san bash
  - [x] RD CMD INSTALL .
  - [x] RD
  - [x] test package

## Documentation

- [x] update translate.Rd
- [x] remove J.Rd
- [ ] update DetailedDocumentation.Rmd
- [ ] update InformationForPackageAuthors.Rmd
- [x] docu code structure R
- [x] docu code structure C++

## Error handling

- [x] in LC: pass error from nodeclasses
- [x] replace all stopifnot with own assert with color
- [x] replace all warnings/messages with own assert with color

## Derivatives

- [x] own version of *d* in ast2ast
  - [x] instead of replacing:
    * [ ]is.name(.) && . == x with quote(1) and 
      is.name(.) with quote(0),
    * [ ]is.name(.) with get_deriv(.)
- [x] add a Buffer\<double\> deriv in Vector
    * add a #ifdef so that it is not always included
- [x] update SubsetClass so that it stores also a deriv_p* of BaseStore\<double\>
- [x] define get_deriv function which returns deriv as Vec\<Buffer\>
- [x] define assign_deriv
- [x] update at function in ETR
- [x] handle subsetting correctly in R fct d
- [x] add all const functions to R fct d
  - vector_logical
  - vector_integer
  - vector_numeric
  - matrix
  - rep
  - colon
  - coca
  - cmr
- [x] add argument so that the user sets the independent variable
- [x] change each expression to:
    assign_deriv(a, expr)
    a = expr
- [x] at the end the user extracts the derivatives
  * e.g. ydot_deriv = get_deriv(ydot) 
- [x] define set_indep
- [x] calc of deriv in loop
  - calc of deriv in node class assign
