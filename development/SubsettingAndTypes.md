# Kanbas ToDo board

## What has to be done in R
- 1. Add type Mat (I)
- 2. Define a node class for type; which has methods such is_scalar, etc.
- 3. Add the checks from (II)
- 4. Update subsetting rules (IV)
- 5. update *determine_type* function
  - Include the new subsetting rules and add missing features as type determination for concatentation
- 5. Implement type inference (V)
- 6. Add the checks for initialisation of variables (VI)

## What has to be done in C++

## I New type matrix
- [ ] switch to store type as tree and not as string
  - [ ] instead of double_vec the user types vec(double) or v(d)
- [ ] add new type *matrix* as option, abbreviated as mat

## II Type checks 
- 1. Check that used type is valid
- 2. Check that for each variable only one unique type is declared
- Example: a |> type(double); a |> type(integer); Only one of those types is valid
- 3. Check that for each variable only once a type is declared
- Example: a |> type(double); a |> type(double) Redeclaration
- 4. Check that the type declaration occures at the first occurance of the variable
- 5. Check that the iterator variables are not used elsewhere
- 6. Check that the iterator is not at lhs of an assignment as it is defined as const
- 7. Check that function arguments are not type of type declarations
- 8. check for assignments and type declarations in forbidden contexts. For example within other function calls.

## III Names in vectors
- [ ] add a unordered map holding the names to Vec
- [ ] concatenate has to be updated in order to handle names
- [ ] implement function names() in C++
- [ ] TODO: can unordered maps hold duplicated keys?

## IV Subsetting
- ast2ast currently supports only vectors and later maybe support lists.
- 1. normal bracket -> returns a vector
- 2. double bracket -> returns a scalar.
  If the user passes something that requires to return something larger than one element a run time error is thrown.
  Thus, an extra function in C++ is required with the only purpose to throw the error. But this is better than a compiler error.
- 3. using names in single brackets -> returns a vector. As a side note names don’t have to be unique.
- 4. using names in double brackets -> returns a scalar.
- [ ] TODO: Need to check how r reacts if names are not unique. Maybe a vector has to be returned instead.
- 5. the dollar sign -> works as 4 but with partial matching.
  I don’t want to support this (at least for now), therefore I will throw a warning during transpilation.

| R                                          | C++                                 |
|--------------------------------------------|-------------------------------------|
| All blocks {}, if or for share same env    | Blocks create new scopes            |
| Variables are function wide defined        | Variables are block wide defined    |

=> Thus, considering the table I cannot allow to have different variables of different types in the emitted C++ code
=> As the aim is not to create a DSL which mimics C++ in R. Instead the user should write R which is subsequently translated
   to valid C++.

## V Type inference
- [ ] Collect all assignments
- [ ] Write determine type of expression
- [ ] Find the variables for which the type is not known
- [ ] Only one assignment. a <- 1; Infer type of variable from rhs.
- [ ] More than one assignment. a <- 1; a <- TRUE;
  - [ ] Infer type for each assignment from rhs
  - [ ] If a common type is found use it otherwise warning or error

## VI Initialised variables
- [ ] Find variables which are defined by plain type declarations. a |> type(d)
- [ ] If the variable is never used afterwards discard it and throw warning
- [ ] Iterator over assignments.
  If the variable is found at the lhs before it is found at rhs break. Otherwise error.
