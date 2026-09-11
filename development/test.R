system('find -name "*.o" | xargs rm')
system('find -name "*.so" | xargs rm')
Rcpp::compileAttributes()
install.packages(".", types = "source", repo = NULL)
# NOTE: don't use tinytest::test_package as here
# TT_AT_HOME is reset to FALSE after the first file
# even when setting Sys.setenv(TT_AT_HOME = "TRUE")
# it is reset after the first file
files <- list.files("~/Documents/ast2ast/inst/tinytest/", full.names = TRUE)
invisible(lapply(files, tinytest::run_test_file))

tinytest::run_test_file("./inst/tinytest/test_det_dsl.R")
tinytest::run_test_file("./inst/tinytest/test_subsetting.R")

# win builder R4.6.1 --> only the note that it was archived and test time was 588 seconds
# win builder R under development --> only the note that it was archived and test time was 588 seconds
# win builder R4.5.3 --> only the note that it was archived and test time was 588 seconds
#
# TODO: if a variable used inside a inner function is not passed to the function
# a weird error is thrown: > fcpp <- ast2ast::translate(diffuse_heat_ast2ast)
# Error in node$vars_types_list[[i]]$real_type <- real_type : 
#   cannot add bindings to a locked environment


files <- list.files("./R", full.names = TRUE)
invisible(lapply(files, source))

install.packages(".", types = "source", repo = NULL)

f <- function() {
  a <- array(1:24, dim = c(3, 4, 2))
  a[1L:2L, c(1L, 4L), 1L:2L]
}
fcpp <- ast2ast::translate(f, debug = FALSE)
fcpp()
