# This is for gathering the output of a function
# during running of tests
capture_output <- function(expr) {
  out_con <- textConnection("output", "w", local = TRUE)
  err_con <- textConnection("error_output", "w", local = TRUE)
  sink(out_con, type = "output")
  sink(err_con, type = "message")
  e <- try(
    {
      res <- expr
      cat(res, "\n")
    },
    silent = TRUE
  )
  sink(NULL, type = "output")
  sink(NULL, type = "message")
  close(out_con)
  close(err_con)
  return(list(stdout = output, stderr = error_output))
}
