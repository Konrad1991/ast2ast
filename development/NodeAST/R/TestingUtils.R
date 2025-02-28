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

expect_error <- function(expr, expected_out, expected_err) {
  messages <- capture_output(expr)

  if (!identical(messages$stdout, expected_out)) {
    stop("❌ Expected stdout:\n", expected_out, "\n✅ Got:\n", messages$stdout)
  }
  if (!identical(messages$stderr, expected_err)) {
    stop("❌ Expected stderr:\n", expected_err, "\n✅ Got:\n", messages$stderr)
  }
  cat("✅ Test passed!\n")
}

expect_correct <- function(expr, code) {
  code_calc <- expr
  if (!identical(code_calc, code)) {
    stop("❌ Expected:\n", code, "\n✅ Got:\n", code_calc)
  }
  cat("✅ Test passed!\n")
}
