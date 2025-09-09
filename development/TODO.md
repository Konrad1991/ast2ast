## Type system:
- handle NA, NaN, and char better. These types are not handled in function registry

## FunctionRegistry:
- maybe add a remove function in order to remove elements from vectors and matrices
- add modulo

## Tests:
- Write tests. for every error which can be thrown.

## Nicer code emitted:
- for loop
- check that blank lines are always removed before combining error messages

## Documentation:
- document that integers have to be defined using L. Otherwise they are emitted as doubles except in subsetting, colon and printing
- cannot assign to iterator

## Fixes for:

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
f <- function() {
  a <- 1
  b <- 2
  cmr(if (a == 1) b, c(1,2), c(1))
}
- prevent usage of if, for, while and repeat within another expression.
  Similar approach as for <-/=

## Cpp

- adapt binary in the same way as unary
- maybe subsetting requires also the same adaption?
