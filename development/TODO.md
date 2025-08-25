Types:
- add check that when someone is using unallowed base type or data struct
- when user declares type dont infer it
- check that type decl is at first occurance of variable

FunctionRegistry:
- add the missing functions & and |
  * [x] add it to function registry
  * [ ] write the C++ functions
- maybe add a remove function in order to remove elements from vectors and matrices
- check how are literal NAs handled in operations?
- How to handle NULL. It has to be handled as literal. Is it required?
- add missing flattening of types. Check at least by writing tests.

Tests:
Write tests. for every error which can be thrown.

Documentation:
- document that integers have to be defined using L. Otherwise they are emitted as doubles except in subsetting, colon and printing
- cannot assign to iterator
