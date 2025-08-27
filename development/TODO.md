FunctionRegistry:
- maybe add a remove function in order to remove elements from vectors and matrices
- add missing flattening of types. Check at least by writing tests.

Tests:
- Write tests. for every error which can be thrown.

Documentation:
- document that integers have to be defined using L. Otherwise they are emitted as doubles except in subsetting, colon and printing
- cannot assign to iterator

Fixes for:

- no curly braces
f <- function() {
  a <- 1L
  while(a[1]) {
    a <- a + 1
    if (a == 4) break
  }
}

- if a type scalar is determined or set by the user
  * the case int a; a = aSEXP has to be handled
