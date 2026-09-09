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
tinytest::run_test_file("./inst/tinytest/test_new_type.R")

# win builder R4.6.1 --> only the note that it was archived and test time was 588 seconds
# win builder R under development --> only the note that it was archived and test time was 588 seconds
# win builder R4.5.3 --> only the note that it was archived and test time was 588 seconds

files <- list.files("./R", full.names = TRUE)
invisible(lapply(files, source))

f <- function(interval, maxiter) {
  argtypes(
    interval |> type(vec(double)),
    maxiter |> type(int)
  )
  fct <- fn(
    argtypes(
      a |> type(double)
    ),
    return(double),
    {
      a*a - 4
    }
  )
  call_uniroot <- fn(
    argtypes(
      interval |> type(vec(double)),
      maxiter |> type(int)
    ),
    return(uniroot_result),
    {
      uniroot(fct, interval, 1e-10, maxiter)
    }
  )
  call_uniroot(interval, maxiter)
}
fcpp <- translate(f, verbose = TRUE, debug = FALSE)
interval <- c(0.0, 100)
maxiter <- 100L
fcpp(interval, maxiter)

demo <- function(v) {
  argtypes(v |> type(vec(double)))

  double_it <- fn(
    argtypes(out |> type(vec(double)) |> ref()),
    return(void),
    {
      for (i in seq_len(length(out))) {
        out[[i]] <- out[[i]] * 2
      }
    }
  )

  double_it(v)   # v is filled in place -- no copy, no allocation
  return(v)
}
f <- ast2ast::translate(demo)
f(c(1, 2, 3))    # c(2, 4, 6)
