#setwd("/home/konrad/Dokumente/1Privat/Programming/AST")

#Rcpp::compileAttributes("ast2ast")

#install.packages("ast2ast", repos = NULL, type = "source")


#detach("package:ast2ast", unload = TRUE)
# library(ast2ast)
# 
# input_variables <- list(y = "vec_num", x = "num") # y = vec_num und x = scalar num
# code <- "y <- y + x
#          y <- y + 1
#          print(y)"
# test <- sep_expressions(code)
# test <- extractast(test)
# get_assignments(test)
# vars <- storage_variables(test)
# vars
# temp <- unlist(test[[2]][3])
# find_var_at_rhs(as.character(temp))
# vars_rhs <- variables_at_rhs(test)
# vars_rhs
# vars_list <- translate(test, input_variables)
# vars_list
# types(vars_list, test)
# new_(vars_list, test)
