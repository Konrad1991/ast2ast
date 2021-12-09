test_that("simple example", {

  f <- function() {
    print("hello world")
    a <- 1
    return(a)
  }
  
  fcpp <- ast2ast::translate(f)
  
  expect_equal(fcpp(), f())
})



