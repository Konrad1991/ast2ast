# ToDo

## General

- [ ] remove unnecessary code
- [ ] remove RNG stuff

## Tests

- [ ] R CMD check --as-cran
- [x] get code working with clang

### XPtr tests

- [ ] write test with f recieving Vec, Borrows mixed
  - [ ] some of the Vecs are than borrowed

### Safety

- [ ] sanatizer and valgrind

## Documentation

- [ ] update translate.Rd
- [ ] update J.Rd
- [ ] update DetailedDocumentation.Rmd
- [ ] update InformationForPackageAuthors.Rmd

## Error handling

- [x] in LC: pass error from nodeclasses

## Derivatives

- [ ] own version of *d* in ast2ast
  - [ ] instead of replacing:
    * [ ]is.name(.) && . == x with quote(1) and 
      is.name(.) with quote(0),
    * [ ]is.name(.) with get_deriv(.)
- [ ] add a Buffer\<double\> deriv in Vector
    * add a #ifdef so that it is not always included
- [ ] define get_deriv function which returns deriv as Vec\<Buffer\>
- [ ] define assign_deriv
- [ ] change each expression to:
    assign_deriv(a, expr)
    a = expr
- [ ] at the end the user extracts the derivatives
  * e.g. ydot_deriv = get_deriv(ydot) 
