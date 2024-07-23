# Translating R to C

Within this document the scope of the translation from R to C is described.
It is crucial, that very few functions are supported. Even though the relevant functions
to define ode systems or loss functions should be available.

## Supported functions

- arithmetic functions:
  - +, -, * and /
- trigonometric functions:
  - sin, cos, tan, etc.
- subsetting:
  - one element subsetting
- interpolation via catmull rom spline interpolation


## Objects

- numeric vectors (can be matrices -> handled at runtime)
- scalar doubles
- scalar ints

## Plan

- first traversion over R code
  - find all variables
  - stop at prohibited functions: e.g. EXPR_d, Vector, VectorManager, etc.
  - create symbol table [index, variable name, type]

- second traversion over R code
  - remove type annotation of each EXPR
  - gather constant ints, doubles and bools
  - gather all variables found in the expression

- define function signature

- define function declaration

- define memory freeing

- assemble function
