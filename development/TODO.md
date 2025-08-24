Types:
- don't allow to return iterator variables
- fix borrow vec arguments

FunctionRegistry:
- update the check function for matrix.
  * the args names have to be checked
  * the type of the arguments to matrix have to be checked
- add the missing functions & and |
- the indices in subsetting can only be logical, integers or numeric.
  * add a check (maybe also in R) to prevent negative indices (this is not supported)
  * check that negative indices are handled in C++
  * maybe add a remove function in order to remove elements from vectors and matrices
- add a check in assignment that rhs is not of type character
- check how are literal NAs handled in operations?
- How to handle NULL. It has to be handled as literal. Is it required?
- add the args-sorting function also to the registry
- add missing flattening of types. Check at least by writing tests.
- document that integers have to be defined using L. Otherwise they are emitted as doubles except in subsetting, colon and printing
- check that print cannot be added within expressions.
- prevent iterator assignment. In C++ the iterator is const. Thus, it is read only. Needs documentation.

- prohibit to assign strings to variables.
  * Strings should only be used in printing and as argument to mode in function vector

Write tests. for every error which can be thrown.
