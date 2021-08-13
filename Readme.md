# R package AstToAst

## Scope of AstToAst

The aim is to translate a tiny subset of R to Rcpp. The idea is that the user defines a function in R which is translated to an Rcpp function. It is required that the user defines the types of the arguments passed to the function. Thus, the final function **translate** gets two arguments the code itself which defines the body of the function and a named list containing the types. Allowed types are numeric (**num**), a numeric vector (**num_vec**) and a numeric matrix (**num_mat**). This functions can be used e.g. for optimization problems, as ode-function and other application where a function call has to be fast. 

## Work in progress