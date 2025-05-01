# Type checks Kanbas ToDo board
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

# Names in vectors Kanbas ToDo board
- [ ] add a unordered map holding the names to Vec
- [ ] concatenate has to be updated in order to handle names
- [ ] implement function names() in C++
- [ ] TODO: can unordered maps hold duplicated keys?

# Subsetting Kanbas ToDo board
- ast2ast currently supports only vectors and later maybe support lists.
- 1. normal bracket -> returns a vector
- 2. double bracket -> returns a scalar.
  If the user passes something that requires to return something larger than one element a run time error is thrown.
  Thus, an extra function in C++ is required whith the only purpose to throw the error. But this is better than a compiler error.
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

# Type inference Kanbas ToDo board
- [ ] Collect all assignments
- [ ] Write determine type of expression
- [ ] Find the variables for which the type is not known
- [ ] Only one assignment. a <- 1; Infer type of variable from rhs.
- [ ] More than one assignment. a <- 1; a <- TRUE;
  - [ ] Infer type for each assignment from rhs
  - [ ] If a common type is found use it otherwise warning or error

# Initialised variables Kanbas ToDo board
- [ ] Find variables which are defined by plain type declarations. a |> type(d)
- [ ] If the variable is never used afterwards discard it and throw warning
- [ ] Iterator over assignments.
  If the variable is found at the lhs before it is found at rhs break. Otherwise error.
