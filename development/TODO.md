Nodes:
- add while node
- add repeat node

FunctionRegistry:
- add the function names to the function registry
- update the check function for matrix.
  * the args names have to be checked
  * the type of the arguments to matrix have to be checked
- infer has to be more subtle.
  * for example in infer_binary_math the basic data struct is vector
  * But this is wrong for scalar, scalar. Therefore, this has to be updated.
- add the missing functions & and |
- the indices in subsetting can only be logical, integers or numeric.
  * add a check (maybe also in R) to prevent negative indices (this is not supported)
  * check that negative indices are handled in C++
  * maybe add a remove function in order to remove elements from vectors and matrices
- add a check in assignment that rhs is not of type character
- check how are literal NAs handled in operations?
- How to handle NULL. It has to be handled as literal. Is it required?
- add the args-sorting function also to the registry
- Infer length returns a vector. Change it to scalar.
- add missing flattening of types. Check at least by writing tests.
- document that integers have to be defined using L. Otherwise they are emitted as doubles except in subsetting, colon and printing
- check that print cannot be added within expressions.
- prevent iterator assignment. In C++ the iterator is const. Thus, it is read only. Needs documentation.

Return Type handling
- if no return type is found; Return type is void/R_NilValue
- if user does return() than use return type is void/R_NilValue
- convert the type directly to a string. Don't handle this in assemble.
- The problem is that void/R_NilValue are character but when the user returns something else it has class type_node
- don't allow to return iterator variables

- prohibit to assign strings to variables.
  * Strings should only be used in printing and as argument to mode in function vector

Write tests. for every error which can be thrown.
