# R package AstToAst

## Scope of AstToAst

The aim is to translate a tiny subset of R to Rcpp. The idea is that the user defines a function in R which is translated to an Rcpp function. It is required that the user defines the types of the arguments passed to the function. Thus, the final function **translate** gets two arguments the code itself which defines the body of the function and a named list containing the types. Allowed types are numeric (**num**), a numeric vector (**num_vec**) and a numeric matrix (**num_mat**). This functions can be used e.g. for optimization problems, as ode-function and other application where a function call has to be fast. 

**Project is in pre-alpha.**


## Current project status (R)

- [x] get Abstract syntax tree (ast)
- [x] deduce type based on variables at right hand site (rhs)
- [x] declare variables
- [ ] Initialice variables
- [x] replace R functions with C++ equivalents 
- [ ] replace R for loop with C++ for loop
- [ ] define function signature
- [ ] put everything together 

```R
library(AstToAst)
library(rlang)

input_variables <- list(y = "num_vec", x = "num", z = "num_mat") # y = vec_num und x = scalar num
code <- "y <- y + x + 1
         z <- y + 1
         h <- z[1]
         print(y)
         if(z > 1) {
         print('test') }
         for(i in 1:10) {print(i)}"

res <- translate(code, list(y = "num_vec", x = "num", z = "num_mat"))
res

# [[1]]
# [[1]][[1]]
# [1] "Master y ;"
# 
# [[1]][[2]]
# [1] "Master x ;"
# 
# [[1]][[3]]
# [1] "Master z ;"
# 
# [[1]][[4]]
# [1] "Master h ;"
# 
# 
# [[2]]
# [[2]][[1]]
# y.num_vec = y.num_vec + x.num + 1
# 
# [[2]][[2]]
# z.num_vec = y.num_vec + 1
# 
# [[2]][[3]]
# h.num_vec = subset(z.num_vec, 1)
# 
# [[2]][[4]]
# print(y.num_vec)
# 
# [[2]][[5]]
# if (l(z.num_vec, 1)) {
#     print("test")
# }
# 
# [[2]][[6]]
# for (i in range(1, 10)) {
#     print(i)
# }

```

## Current project status C++

- [x] Vector Class
- [x] Matrix Class
- [ ] subset fct implementation
- [ ] Functions (print, range, subset, etc.)
- [ ] Masterclass


## Contribution

Contribution would be warmly appreciated (See Code of Conduct). 